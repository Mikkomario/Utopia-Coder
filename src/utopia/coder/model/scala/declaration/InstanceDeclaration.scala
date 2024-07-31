package utopia.coder.model.scala.declaration

import utopia.coder.model.scala.doc.ScalaDocKeyword.{Author, Since}
import utopia.flow.collection.CollectionExtensions._
import utopia.flow.collection.immutable.Pair
import utopia.flow.util.StringExtensions._
import utopia.coder.model.scala.code.CodeBuilder
import utopia.coder.model.merging.{MergeConflict, Mergeable}
import utopia.coder.model.scala.code.Code
import utopia.coder.model.scala.datatype.{Extension, GenericType, ScalaType}
import utopia.coder.model.scala.declaration.InstanceDeclarationType.{ClassD, ObjectD, TraitD}
import utopia.coder.model.scala.doc.ScalaDocPart
import utopia.coder.model.scala.{Annotation, DeclarationDate, Parameters, Visibility}
import utopia.coder.model.scala.template.CodeConvertible
import utopia.flow.operator.ordering.CombinedOrdering

import scala.collection.immutable.VectorBuilder

/**
  * Declares an object or a class
  * @author Mikko Hilpinen
  * @since 30.8.2021, v0.1
  */
trait InstanceDeclaration extends Declaration with Mergeable[InstanceDeclaration, InstanceDeclaration]
{
	// ABSTRACT ------------------------------
	
	/**
	 * @return Type of this declaration
	 */
	def declarationType: InstanceDeclarationType
	
	/**
	  * @return Comments presented before the main declaration, but not included in the scaladoc
	  */
	def headerComments: Seq[String]
	/**
	  * @return parameters accepted by this instance's constructor, if it has one.
	  */
	protected def constructorParams: Option[Parameters]
	/**
	  * @return Classes & traits this instance extends, including possible construction parameters etc.
	  */
	def extensions: Seq[Extension]
	/**
	 * @return Declared types
	 */
	def types: Seq[TypeDeclaration]
	/**
	  * @return Code executed every time an instance is created
	  */
	def creationCode: Code
	/**
	  * @return Properties defined in this instance
	  */
	def properties: Seq[PropertyDeclaration]
	/**
	  * @return Methods defined for this instance
	  */
	def methods: Set[MethodDeclaration]
	/**
	  * @return Nested classes & objects
	  */
	def nested: Set[InstanceDeclaration]
	/**
	  * @return Description of this instance (may be empty)
	  */
	def description: String
	/**
	  * @return Author that wrote this declaration (may be empty)
	  */
	def author: String
	/**
	  * @return Date when this declaration was made
	  */
	def since: DeclarationDate
	
	/**
	  * Creates a copy of this instance, with altered information
	  * @param visibility New visibility
	  * @param genericTypes Generic types to introduce / use within this declaration
	  * @param extensions New extensions
	  * @param types New type declarations
	 * @param creationCode New creation code
	  * @param properties New properties
	  * @param methods New methods
	  * @param nested New nested instances
	  * @param annotations New annotations
	  * @param description New description
	  * @param author New author
	  * @param headerComments New header comments
	  * @return A modified copy of this instance
	  */
	protected def makeCopy(visibility: Visibility = visibility, genericTypes: Seq[GenericType] = genericTypes,
	                       extensions: Seq[Extension] = extensions, types: Seq[TypeDeclaration] = types,
	                       creationCode: Code = creationCode, properties: Seq[PropertyDeclaration] = properties,
	                       methods: Set[MethodDeclaration] = methods, nested: Set[InstanceDeclaration] = nested,
	                       annotations: Seq[Annotation] = annotations, description: String = description,
	                       author: String = author, headerComments: Seq[String] = headerComments,
	                       since: DeclarationDate = since): InstanceDeclaration
	
	
	// COMPUTED ------------------------------
	
	/**
	 * @return Whether this declares an object
	 */
	def isObject = declarationType == ObjectD
	/**
	 * @return Whether this declares a class
	 */
	def isClass = declarationType == ClassD
	/**
	 * @return Whether this declares a trait
	 */
	def isTrait = declarationType == TraitD
	
	/**
	  * @return A basic type reference to the declared type.
	  *         Won't contain a package reference and should therefore only be used within the same file.
	  */
	def toBasicType = ScalaType.basic(name)
	
	
	// IMPLEMENTED  --------------------------
	
	override def documentation = {
		val builder = new VectorBuilder[ScalaDocPart]()
		val desc = description
		if (desc.nonEmpty)
			builder += ScalaDocPart.description(desc)
		constructorParams.foreach { builder ++= _.documentation }
		genericTypes.foreach { builder ++= _.documentation }
		// If there are other scaladocs, adds author and since -tags
		val since = this.since
		if (description.nonEmpty)
		{
			if (author.nonEmpty)
				builder += ScalaDocPart(Author, author)
			builder += ScalaDocPart(Since, since.toString)
		}
		builder.result()
	}
	
	override def toCode = {
		val builder = new CodeBuilder()
		
		// Writes the scaladoc
		builder ++= scalaDoc
		// Writes possible comments
		headerComments.foreach { c => builder += s"// $c" }
		// Writes possible annotations
		builder ++= annotationsPart
		// Writes the declaration and the extensions
		builder += basePart
		constructorParams.foreach { builder += _.toScala }
		
		val ext = extensions
		if (ext.nonEmpty)
			builder.appendPartial(ext.map { _.toScala }.reduceLeft { _.append(_, " with ") }
				.withPrefix("extends "), " ", allowLineSplit = true)
		
		// Starts writing the instance body
		/* Write order is as follows:
			1) Type declarations
			2) Attributes
			3) Creation code
			4) Abstract properties
			5) Abstract methods
			6) Computed properties (non-implemented, first public, then private)
			7) Implicit functions
			8) Implemented properties, then implemented methods (first public, then protected)
			9) Other methods (first public)
			10) Nested objects, then nested classes (first public)
		*/
		val (attributes, computed) = properties.divideBy { _.isComputed }.toTuple
		
		val (concreteComputed, abstractComputed) = computed.divideBy { _.isAbstract }.toTuple
		val (concreteMethods, abstractMethods) = methods.divideBy { _.isAbstract }.toTuple
		
		val (newComputed, implementedComputed) = concreteComputed.divideBy { _.isOverridden }.toTuple
		val (nonImplementedMethods, implementedMethods) = concreteMethods.divideBy { _.isOverridden }.toTuple
		val (otherMethods, implicitMethods) = nonImplementedMethods.divideBy { _.isImplicit }.toTuple
		
		val visibilityOrdering: Ordering[Declaration] = (a, b) => -a.visibility.compareTo(b.visibility)
		val fullOrdering = new CombinedOrdering[Declaration](Vector(
			visibilityOrdering, Ordering.by[Declaration, String] { _.name }))
		
		appendSegments(builder, Vector[(Iterable[CodeConvertible], String)](
			types -> "TYPES",
			attributes -> "ATTRIBUTES",
			creationCode.notEmpty.toVector -> "INITIAL CODE",
			(abstractComputed.sorted(visibilityOrdering) ++
				abstractMethods.toVector.sorted(fullOrdering)) -> "ABSTRACT",
			newComputed.sorted(visibilityOrdering) -> "COMPUTED",
			implicitMethods -> "IMPLICIT",
			(implementedComputed.sorted(fullOrdering) ++
				implementedMethods.toVector.sorted(fullOrdering)) -> "IMPLEMENTED",
			otherMethods.toVector.sorted(fullOrdering) -> "OTHER",
			nested.toVector.sorted(fullOrdering) -> "NESTED"
		))
		
		builder.result()
	}
	
	override def matches(other: InstanceDeclaration): Boolean = name == other.name
	
	override def mergeWith(other: InstanceDeclaration) = {
		val parties = Pair(this, other)
		val conflictsBuilder = new VectorBuilder[MergeConflict]()
		
		val myDeclaration = basePart
		val theirDeclaration = other.basePart
		if (myDeclaration != theirDeclaration)
			conflictsBuilder += MergeConflict.line(theirDeclaration.toString, myDeclaration.toString,
				s"$name declarations differ")
		val mySuperConstructor = extensions.find { _.hasConstructor }
		val theirSuperConstructor = other.extensions.find { _.hasConstructor}
		if (mySuperConstructor.exists { my => theirSuperConstructor.exists { _ != my } })
			conflictsBuilder += MergeConflict.line(theirSuperConstructor.get.toString,
				mySuperConstructor.get.toString, s"$name versions specify different super constructors")
		
		val (mergedAnnotations, annotationConflicts) = Annotation.merge(parties.map { _.annotations })
		annotationConflicts.foreach { case Pair(_, their) =>
			conflictsBuilder += MergeConflict.note(s"Annotation ${ their.toScala } was (partially) overwritten")
		}
		
		def _mergeDeclarations[A <: Mergeable[A, A] with Declaration](my: Seq[A], their: Seq[A]): Seq[A] = {
			my.map { declaration =>
				their.find { _ matches declaration } match {
					case Some(otherVersion) =>
						val (merged, conflicts) = declaration.mergeWith(otherVersion)
						conflictsBuilder ++= conflicts
						merged
					case None => declaration
				}
			} ++ their.filterNot { theirDeclaration => my.exists { _.name == theirDeclaration.name } }
		}
		
		val newTypes = _mergeDeclarations(types, other.types)
		val newProperties = _mergeDeclarations(properties, other.properties)
		val newMethods = _mergeDeclarations(methods.toVector, other.methods.toVector).toSet
		val newNested = _mergeDeclarations(nested.toVector, other.nested.toVector).toSet
		
		val (comparableExtensions, addedExtensions) = other.extensions.divideWith { ext =>
			extensions.find { _.parentType.matches(ext.parentType)} match {
				case Some(myVersion) => Left(myVersion -> ext)
				case None => Right(ext)
			}
		}
		comparableExtensions.foreach { case (my, their) =>
			if (my.toString != their.toString)
				conflictsBuilder += MergeConflict.line(their.toString, my.toString,
					s"$name extension differs")
		}
		if (addedExtensions.nonEmpty)
			conflictsBuilder += MergeConflict.line(other.extensions.mkString(" with "), extensions.mkString(" with "),
				"Merging introduces new extensions")
		val newExtensions = mySuperConstructor.orElse(theirSuperConstructor).toVector ++
			extensions.filterNot { _.hasConstructor } ++ addedExtensions.filterNot { _.hasConstructor }
		
		makeCopy(visibility min other.visibility,
			genericTypes ++ other.genericTypes.filterNot { t => genericTypes.exists { _.name == t.name } },
			newExtensions, newTypes,
			creationCode ++ Code(other.creationCode.lines.filterNot(creationCode.lines.contains),
				other.creationCode.references),
			newProperties, newMethods, newNested, mergedAnnotations, description.nonEmptyOrElse(other.description),
			author.nonEmptyOrElse(other.author),
			headerComments ++ other.headerComments.filterNot(headerComments.contains), since min other.since) ->
			conflictsBuilder.result()
	}
	
	
	// OTHER    ---------------------------------
	
	/**
	 * @param function Searched function identifier
	 * @return Whether this instance contains a function with that identifier
	 */
	def contains(function: FunctionIdentifier) =
		(properties.iterator ++ methods).exists { _.identifier == function }
	
	/**
	 * @param method Method to add to this instance
	 * @return Copy of this instance with the specified method added
	 */
	def +(method: MethodDeclaration) = makeCopy(methods = methods + method)
	/**
	 * @param nested Nested instance to add to this one
	 * @return Copy of this instance with the specified nested instance
	 */
	def +(nested: InstanceDeclaration) = makeCopy(nested = this.nested + nested)
	
	/**
	 * @param function Identifier of the function to remove from this instance, if specified
	 * @return Copy of this instance with the specified function removed
	 */
	def -(function: FunctionIdentifier) =
		makeCopy(properties = properties.filterNot { _.identifier == function },
			methods = methods.filterNot { _.identifier == function })
	
	/**
	 * @param functions Identifiers of the functions to remove from this instance, if present
	 * @return Copy of this instance with the specified functions removed
	 */
	def --(functions: Iterable[FunctionIdentifier]) = {
		if (functions.isEmpty)
			this
		else
			makeCopy(
				properties = properties.filterNot { prop =>
					val id = prop.identifier
					functions.exists { _ == id }
				},
				methods = methods.filterNot { method =>
					val id = method.identifier
					functions.exists { _ == id }
				}
			)
	}
	
	/**
	 * @param f A filter function that returns true for the methods to keep
	 * @return Copy of this instance with filtered methods
	 */
	def filterMethods(f: MethodDeclaration => Boolean) = makeCopy(methods = methods.filter(f))
	/**
	 * @param f A function that will return true for instance declarations to keep
	 * @return Copy of this declaration with filtered nested declarations
	 */
	def filterNested(f: InstanceDeclaration => Boolean) = makeCopy(nested = nested.filter(f))
	
	private def appendSegments(builder: CodeBuilder, segments: Seq[(Iterable[CodeConvertible], String)]) = {
		val segmentsToWrite = segments
			.map { case (code, header) => code.map { _.toCode } -> header }
			.filter { _._1.nonEmpty }
		if (segmentsToWrite.nonEmpty) {
			builder.openBlock(forceNewLine = true)
			segmentsToWrite.dropRight(1).foreach { case (codes, header) =>
				builder += s"// $header\t--------------------"
				builder.addEmptyLine()
				codes.foreach { code =>
					builder ++= code
					builder.addEmptyLine()
				}
				builder.addEmptyLine()
			}
			// Writes the last portion separately because the separators are different at the end
			val (lastCodes, lastHeader) = segmentsToWrite.last
			builder += s"// $lastHeader\t--------------------"
			lastCodes.foreach { code =>
				builder.addEmptyLine()
				builder ++= code
			}
			builder.closeBlock()
		}
	}
}
