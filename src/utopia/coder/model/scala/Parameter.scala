package utopia.coder.model.scala

import utopia.coder.model.scala.doc.ScalaDocKeyword.Param
import utopia.coder.model.scala.code.CodePiece
import utopia.coder.model.scala.datatype.ScalaType
import utopia.coder.model.scala.declaration.DeclarationStart
import utopia.coder.model.scala.doc.{ScalaDoc, ScalaDocPart}
import utopia.coder.model.scala.template.{Documented, ScalaConvertible}
import utopia.flow.collection.immutable.{Empty, Single}
import utopia.flow.collection.CollectionExtensions._

/**
  * Represents a scala method parameter
  * @author Mikko Hilpinen
  * @since 30.8.2021, v0.1
  */
case class Parameter(name: String, dataType: ScalaType, default: CodePiece = CodePiece.empty,
                     prefix: Option[DeclarationStart] = None, description: String = "")
	extends ScalaConvertible with Documented
{
	// COMPUTED -------------------------------
	
	/**
	  * @return Whether this parameter has a default value
	  */
	def hasDefault = default.nonEmpty
	/**
	  * @return Whether this parameter doesn't have a default value
	  */
	def hasNoDefault = !hasDefault
	
	/**
	 * @return Scaladoc entry matching this parameter
	 */
	def scalaDocLine = {
		if (description.nonEmpty)
			Single(ScalaDocPart(Param(name), description.linesIterator.toOptimizedSeq))
		else
			Empty
	}
	
	/**
	  * @return A copy of this parameter without a default value
	  */
	def withoutDefault = if (hasDefault) copy(default = CodePiece.empty) else this
	
	
	// IMPLEMENTED  ---------------------------
	
	override def toScala = {
		val defaultPart = if (default.isEmpty) default else default.withPrefix(" = ")
		val mainPart = dataType.toScala.withPrefix(name + ": ") + defaultPart
		prefix match {
			case Some(prefix) => prefix.toScala.append(mainPart, " ")
			case None => mainPart
		}
	}
	
	override def scalaDoc = ScalaDoc(scalaDocLine)
	
	
	// OTHER    ------------------------------
	
	/**
	  * Creates a new parameter list by adding implicits to this parameter
	  * @param firstParam First implicit parameter
	  * @param moreParams More implicit parameters
	  * @return A new parameters list
	  */
	def withImplicits(firstParam: Parameter, moreParams: Parameter*) =
		Parameters(Single(Single(this)), firstParam +: moreParams)
}
