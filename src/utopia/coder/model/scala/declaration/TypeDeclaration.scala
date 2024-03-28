package utopia.coder.model.scala.declaration

import utopia.coder.model.merging.{MergeConflict, Mergeable}
import utopia.coder.model.scala.Visibility.Public
import utopia.coder.model.scala.{Annotation, Visibility}
import utopia.coder.model.scala.code.{Code, CodeBuilder, CodePiece}
import utopia.coder.model.scala.datatype.GenericType
import utopia.coder.model.scala.doc.ScalaDocPart
import utopia.flow.collection.immutable.Pair
import utopia.flow.util.StringExtensions._

import scala.collection.immutable.VectorBuilder

private object TypeDeclaration
{
	private lazy val keyword = CodePiece("type")
}

/**
 * Contains information about a declared type (alias)
 *
 * @author Mikko Hilpinen
 * @since 28/03/2024, v1.0.2
 */
case class TypeDeclaration(name: String, genericTypes: Seq[GenericType] = Vector.empty, visibility: Visibility = Public,
                           annotations: Seq[Annotation] = Vector.empty, description: String = "",
                           headerComments: Vector[String] = Vector.empty)
	extends Declaration with Mergeable[TypeDeclaration, TypeDeclaration]
{
	override def keyword: CodePiece = TypeDeclaration.keyword
	
	override def documentation: Vector[ScalaDocPart] = description.notEmpty.map(ScalaDocPart.description).toVector
	
	// WET WET: Contains copy code from InstanceDeclaration
	override def toCode: Code = {
		val builder = new CodeBuilder()
		// Writes the scaladoc
		builder ++= scalaDoc
		// Writes possible comments
		headerComments.foreach { c => builder += s"// $c" }
		// Writes possible annotations
		builder ++= annotationsPart
		// Writes the declaration
		builder += basePart
		
		builder.result()
	}
	
	override def matches(other: TypeDeclaration): Boolean = name == other.name
	
	// WET WET: Contains copy code from InstanceDeclaration
	override def mergeWith(other: TypeDeclaration): (TypeDeclaration, Vector[MergeConflict]) = {
		val parties = Pair(this, other)
		val conflictsBuilder = new VectorBuilder[MergeConflict]()
		
		val myDeclaration = basePart
		val theirDeclaration = other.basePart
		if (myDeclaration != theirDeclaration)
			conflictsBuilder += MergeConflict.line(theirDeclaration.toString, myDeclaration.toString,
				s"$name declarations differ")
		
		val (mergedAnnotations, annotationConflicts) = Annotation.merge(parties.map { _.annotations })
		annotationConflicts.foreach { case Pair(_, their) =>
			conflictsBuilder += MergeConflict.note(s"Annotation ${ their.toScala } was (partially) overwritten")
		}
		
		val newDescription = description.nonEmptyOrElse(other.description)
		val newHeaderComments = headerComments ++ other.headerComments.filterNot(headerComments.contains)
		
		copy(annotations = mergedAnnotations, description = newDescription, headerComments = newHeaderComments) ->
			conflictsBuilder.result()
	}
}
