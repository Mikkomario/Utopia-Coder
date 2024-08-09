package utopia.coder.model.scala.declaration

import utopia.coder.model.scala.code.Code
import utopia.coder.model.scala.Visibility.{Protected, Public}
import utopia.coder.model.scala.datatype.{Reference, ScalaType}
import utopia.coder.model.scala.{Annotation, Parameter, Visibility}
import utopia.coder.model.scala.template.ScalaConvertible
import utopia.flow.collection.immutable.Empty

/**
  * An enumeration for different types of property declarations
  * @author Mikko Hilpinen
  * @since 30.8.2021, v0.1
  */
sealed trait PropertyDeclarationType extends ScalaConvertible
{
	/**
	  * Creates a new property declaration based on this type
	  * @param name Name of the declared property
	  * @param references References made within the code (default = empty)
	  * @param visibility Visibility of this property (default = public)
	  * @param explicitOutputType Data type returned by this function when explicitly defined (optional)
	  * @param implicitParams Implicit parameters accepted by this (computed) property
	  * @param annotations Annotations that apply to this declaration (default = empty)
	  * @param description Documentation for this property
	  * @param isOverridden Whether this property overrides a base member (default = false)
	  * @param isImplicit Whether this is an implicit property (default = false)
	  * @param isLowMergePriority Whether this declaration should be considered the lower priority
	  *                           implementation when merging with another version
	  * @param line1 First line of code
	  * @param moreLines More lines of code
	  * @return A new property declaration
	  */
	def apply(name: String, references: Set[Reference] = Set(), visibility: Visibility = Public,
	          explicitOutputType: Option[ScalaType] = None,
	          implicitParams: Seq[Parameter] = Empty, annotations: Seq[Annotation] = Empty,
	          description: String = "", isOverridden: Boolean = false,
	          isImplicit: Boolean = false, isLowMergePriority: Boolean = false)
	         (line1: String, moreLines: String*) =
	{
		// Empty body leads to an abstract property
		val codeLines = {
			if (line1.isEmpty && moreLines.forall { _.isEmpty })
				Empty
			else
				line1 +: moreLines
		}
		PropertyDeclaration(this, name, Code.from(codeLines).referringTo(references), visibility,
			explicitOutputType, implicitParams, annotations, description, Empty,
			isOverridden, isImplicit, isLowMergePriority)
	}
	
	/**
	 * Creates a new abstract property
	 * @param name Name of the declared property
	 * @param outputType Data type returned by this function
	 * @param implicitParams Implicit parameters accepted by this (computed) property (default = empty)
	 * @param description Documentation for this property (default = empty)
	 * @param isProtected Whether the visibility of this property should be protected instead of public (default = false)
	 * @param isOverridden Whether this property overrides a base member (default = false)
	 * @param isLowMergePriority Whether this declaration should be considered the lower priority
	 *                           implementation when merging with another version (default = false)
	 * @return A new abstract property
	 */
	def newAbstract(name: String, outputType: ScalaType, implicitParams: Seq[Parameter] = Empty,
	                description: String = "", isProtected: Boolean = false, isOverridden: Boolean = false,
	                isLowMergePriority: Boolean = false) =
		PropertyDeclaration(this, name, Code.empty, if (isProtected) Protected else Public, Some(outputType),
			implicitParams, Empty, description, Empty, isOverridden, isImplicit = false, isLowMergePriority)
}

object PropertyDeclarationType
{
	/**
	  * Used for defining immutable values
	  */
	case object ImmutableValue extends PropertyDeclarationType
	{
		override def toScala = "val"
	}
	
	case object LazyValue extends PropertyDeclarationType
	{
		override def toScala = "lazy val"
	}
	
	/**
	  * Used for defining mutable properties / variables
	  */
	case object Variable extends PropertyDeclarationType
	{
		override def toScala = "var"
	}
	
	/**
	  * Used for defining properties which are calculated when called
	  */
	case object ComputedProperty extends PropertyDeclarationType
	{
		override def toScala = "def"
	}
}
