package utopia.coder.model.scala.datatype

import utopia.flow.collection.CollectionExtensions._
import utopia.flow.parse.string.Regex
import utopia.flow.util.StringExtensions._
import utopia.coder.model.scala.Package
import utopia.coder.model.scala.code.CodePiece
import utopia.coder.model.scala.datatype.ScalaTypeCategory.{CallByName, Standard}
import utopia.coder.model.scala.template.ScalaConvertible
import utopia.flow.collection.immutable.Empty
import utopia.flow.operator.equality.EqualsFunction

import scala.language.implicitConversions

object ScalaType
{
	// ATTRIBUTES   ---------------------------
	
	private lazy val typeParamsSeparator = Regex.escape(',').ignoringWithin('[', ']')
	
	val string = basic("String")
	val int = basic("Int")
	val long = basic("Long")
	val double = basic("Double")
	val boolean = basic("Boolean")
	
	lazy val instant = apply(Reference.instant)
	
	/**
	  * A function that matches types that have the same base type
	  */
	implicit val matchFunction: EqualsFunction[ScalaType] = EqualsFunction.by { t: ScalaType => t.code.text }
	
	
	// IMPLICIT  ------------------------------
	
	// Implicitly converts from a reference
	implicit def referenceToType(reference: Reference): ScalaType = apply(reference)
	
	
	// OTHER    -------------------------------
	
	/**
	  * @param contentType Collection content type
	  * @return Iterable type
	  */
	def iterable(contentType: ScalaType) = generic("Iterable", contentType)
	/**
	  * @param contentType Option content type
	  * @return An option type
	  */
	def option(contentType: ScalaType) = generic("Option", contentType)
	/**
	 * @param contentType Vector content type
	 * @return A vector type
	 */
	def seq(contentType: ScalaType) = generic("Seq", contentType)
	/**
	  * @param contentType Vector content type
	  * @return A vector type
	  */
	def vector(contentType: ScalaType) = generic("Vector", contentType)
	/**
	  * @param contentType Set content type
	  * @return A set type
	  */
	def set(contentType: ScalaType) = generic("Set", contentType)
	
	/**
	  * @param name Name of the (basic) data type
	  * @return A data type with that name and no import
	  */
	def basic(name: String) = apply(CodePiece(name))
	
	/**
	  * @param reference A reference
	  * @return A data type based on that reference
	  */
	def apply(reference: Reference): ScalaType = apply(reference.targetCode)
	
	/**
	  * Parses a scala type from a string, which may be a reference and/or a generic type
	  * @param typeString A string representing a scala data type
	  * @return Data type parsed from that string
	  */
	def parse(typeString: String): ScalaType = {
		val (beforeTypes, typesPart) = typeString.splitAtFirst("[").toTuple
		val basePart = {
			if (Package.separatorRegex.existsIn(beforeTypes))
				Reference(beforeTypes).targetCode
			else
				CodePiece(beforeTypes)
		}
		// Case: Standard (ie. non-generic) data type
		if (typesPart.isEmpty)
			apply(basePart)
		// Case: Generic data type => parses the type parameters, also (recursive)
		else {
			val typeParamStrings = typesPart.untilLast("]").split(typeParamsSeparator).map { _.trim }
			apply(basePart, typeParamStrings.map(parse))
		}
	}
	
	/**
	  * Creates a generic type
	  * @param reference Base type reference
	  * @param firstParam First type parameter
	  * @param moreParams More type parameters
	  * @return A generic type
	  */
	def generic(reference: Reference, firstParam: ScalaType, moreParams: ScalaType*) =
		apply(reference.targetCode, firstParam +: moreParams)
	/**
	  * Creates a generic type
	  * @param name Name of the basic generic class
	  * @param firstParam First type parameter
	  * @param moreParams More type parameters
	  * @return A generic type
	  */
	def generic(name: String, firstParam: ScalaType, moreParams: ScalaType*) =
		apply(CodePiece(name), firstParam +: moreParams)
	
	/**
	  * Creates a function scala type
	  * @param paramTypes Accepted parameter types (default = empty)
	  * @param resultType Resulting data type (raw)
	  * @param typeParams Generic type arguments for the resulting data type (default = empty)
	  * @return A functional scala type
	  */
	def function(paramTypes: ScalaType*)(resultType: CodePiece, typeParams: ScalaType*) =
		apply(resultType, typeParams, ScalaTypeCategory.Function(paramTypes))
}

/**
  * Represents either a basic data type, which doesn't require an import, or a custom reference
  * @author Mikko Hilpinen
  * @since 30.8.2021, v0.1
  */
case class ScalaType(code: CodePiece, typeParameters: Seq[ScalaType] = Empty, category: ScalaTypeCategory = Standard)
	extends ScalaConvertible
{
	// ATTRIBUTES   ------------------------------
	
	override lazy val toScala: CodePiece = {
		val withTypeParams = {
			if (typeParameters.isEmpty)
				code
			else
				code + typeParameters.map { _.toScala }.reduceLeft { _.append(_, ", ") }.withinSquareBrackets
		}
		category match {
			case Standard => withTypeParams
			case CallByName => withTypeParams.withPrefix("=> ")
			case ScalaTypeCategory.Function(parameterTypes) =>
				val parameterList = {
					if (parameterTypes.isEmpty)
						CodePiece("()")
					else
						parameterTypes.only match {
							case Some(only) => only.toScala
							case None =>
								parameterTypes.map { _.toScala }.reduceLeft { _.append(_, ", ") }
									.withinParenthesis
						}
				}
				parameterList.append(withTypeParams, " => ")
		}
	}
	
	
	// COMPUTED ------------------------
	
	/**
	 * @return Copy of this type with no generic type parameters specified
	 */
	def withoutGenericTypeParameters = if (typeParameters.isEmpty) this else copy(typeParameters = Empty)
	
	
	// OTHER    ------------------------
	
	/**
	  * @param typeParameters The generic type parameters to assign to this type
	  * @return A generic type that wraps the specified types
	  */
	def apply(typeParameters: Seq[ScalaType]) = copy(typeParameters = typeParameters)
	/**
	  * @param firstTypeParameter First type parameter to append
	  * @param moreTypeParameters More type parameters to append
	  * @return Copy of this type that uses the specified generic type parameters
	  */
	def apply(firstTypeParameter: ScalaType, moreTypeParameters: ScalaType*) =
		copy(typeParameters = firstTypeParameter +: moreTypeParameters)
	
	/**
	 * @param otherType Another type
	 * @return A combination of these two types
	 */
	def withOther(otherType: ScalaType) =
		ScalaType(toScala.flatMapText { first => otherType.toScala.mapText { second => s"$first with $second" } })
	
	/**
	  * @param parameterTypes A list of accepted parameter types
	  * @return A functional data type that returns this data type
	  */
	def fromParameters(parameterTypes: Seq[ScalaType]) = category match {
		// Functions that return functions don't handle references properly at this time
		case _ :ScalaTypeCategory.Function =>
			ScalaType(toScala, category = ScalaTypeCategory.Function(parameterTypes))
		case _ => copy(category = ScalaTypeCategory.Function(parameterTypes))
	}
	def fromParameters(firstParameter: ScalaType, moreParameters: ScalaType*): ScalaType =
		fromParameters(firstParameter +: moreParameters)
	
	/**
	  * @param other Another type
	  * @return Whether these types are similar, when considering their base types
	  */
	def matches(other: ScalaType) = code.text == other.code.text
}
