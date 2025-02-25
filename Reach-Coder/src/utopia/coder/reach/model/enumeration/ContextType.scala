package utopia.coder.reach.model.enumeration

import utopia.coder.model.scala.datatype.Reference
import utopia.flow.operator.equality.EqualsExtensions._
import utopia.flow.util.StringExtensions._
import utopia.coder.reach.util.ReachReferences.Reach._
import utopia.coder.reach.util.ReachReferences._

/**
  * An enumeration for different available component creation context types
  * @author Mikko Hilpinen
  * @since 19.5.2023, v1.0
  */
sealed trait ContextType
{
	/**
	  * @return Keyword used for identifying this type from user input
	  */
	def keyword: String
	/**
	  * @return Reference to the associated context class
	  */
	def reference: Reference
	/**
	  * @return Reference to the associated component factory trait.
	  *         None if no specific component factory trait is supported with this context type.
	  */
	def factory: Option[Reference]
}

object ContextType
{
	// ATTRIBUTES   ------------------------
	
	val values = Vector[ContextType](Base, StaticBase, VariableBase, Color, StaticColor, VariableColor,
		Text, StaticText, VariableText, StaticWindow, VariableWindow)
	
	
	// OTHER    ----------------------------
	
	/**
	  * @param input User input
	  * @return The context type mentioned in that input. None if no context type was mentioned.
	  */
	def apply(input: String) =
		values.find { _.keyword ~== input }.orElse { values.find { c => input.containsIgnoreCase(c.keyword) } }
	
	
	// VALUES   ----------------------------
	
	/**
	  * Represents the BaseContextPropsView class from Firmament
	  */
	case object Base extends ContextType
	{
		override val keyword: String = "base"
		override lazy val reference: Reference = firmament.baseContextProps
		override val factory = None
	}
	/**
	 * Represents the StaticBaseContext class from Firmament
	 */
	case object StaticBase extends ContextType
	{
		override val keyword: String = "base-static"
		override lazy val reference: Reference = firmament.staticBaseContext
		override val factory = Some(baseContextualFactory)
	}
	/**
	 * Represents the VariableBaseContext class from Firmament
	 */
	case object VariableBase extends ContextType
	{
		override val keyword: String = "base-variable"
		override lazy val reference: Reference = firmament.variableBaseContext
		override val factory = None
	}
	/**
	  * Represents the ColorContextPropsView class from Firmament
	  */
	case object Color extends ContextType
	{
		override val keyword: String = "color"
		override lazy val reference: Reference = firmament.colorContextProps
		override val factory = None
	}
	/**
	 * Represents the StaticColorContext class from Firmament
	 */
	case object StaticColor extends ContextType
	{
		override val keyword: String = "color-static"
		override lazy val reference: Reference = firmament.staticColorContext
		override val factory = Some(colorContextualFactory)
	}
	/**
	 * Represents the VariableColorContext class from Firmament
	 */
	case object VariableColor extends ContextType
	{
		override val keyword: String = "color-variable"
		override lazy val reference: Reference = firmament.variableColorContext
		override val factory = None
	}
	/**
	  * Represents the TextContextPropsView class from Firmament
	  */
	case object Text extends ContextType
	{
		override val keyword: String = "text"
		override lazy val reference: Reference = firmament.textContextProps
		override val factory = None
	}
	/**
	 * Represents the StaticTextContext class from Firmament
	 */
	case object StaticText extends ContextType
	{
		override val keyword: String = "text-static"
		override lazy val reference: Reference = firmament.staticTextContext
		override val factory = Some(textContextualFactory)
	}
	/**
	 * Represents the VariableTextContext class from Firmament
	 */
	case object VariableText extends ContextType
	{
		override val keyword: String = "text-variable"
		override lazy val reference: Reference = firmament.variableTextContext
		override val factory = None
	}
	/**
	  * Represents the StaticReachContentWindowContext class
	  */
	case object StaticWindow extends ContextType
	{
		override val keyword: String = "window-static"
		
		override def reference: Reference = staticContentWindowContext
		override def factory = Some(contentWindowContextualFactory)
	}
	/**
	 * Represents the VariableReachContentWindowContext class
	 */
	case object VariableWindow extends ContextType
	{
		override val keyword: String = "window-variable"
		
		override def reference: Reference = variableContentWindowContext
		override def factory = None
	}
}
