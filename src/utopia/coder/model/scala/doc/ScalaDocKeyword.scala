package utopia.coder.model.scala.doc

import utopia.flow.operator.ordering.SelfComparable

/**
  * An enumeration for different keywords used in scaladocs
  * @author Mikko Hilpinen
  * @since 3.9.2021, v0.1
  */
sealed trait ScalaDocKeyword extends SelfComparable[ScalaDocKeyword]
{
	// ABSTRACT ------------------------------
	
	/**
	  * @return String of the keyword part (e.g. "param")
	  */
	protected def keywordString: String
	/**
	  * @return Priority used when ordering this keyword. Higher values come last.
	  */
	protected def orderIndex: Int
	
	/**
	 * @return Whether this keyword pads its description to the same starting point with other keywords
	 */
	def padsToSameLength: Boolean
	
	
	// IMPLEMENTED  -------------------------
	
	override def self = this
	
	override def toString = s"@$keywordString"
	
	override def compareTo(o: ScalaDocKeyword) = orderIndex.compareTo(o.orderIndex)
}

object ScalaDocKeyword
{
	/**
	  * Values of this enumeration which don't take any parameters
	  */
	val staticValues = Vector[ScalaDocKeyword](Return, Author, Since)
	
	/**
	  * @param keywordString Searched keyword string (not including @)
	  * @return The keyword that matches that string
	  */
	def matching(keywordString: String, paramPart: => String) =
	{
		staticValues.find { _.keywordString == keywordString }
			.orElse {
				keywordString match
				{
					case "param" => Some(Param(paramPart))
					case "tparam" => Some(TypeParam(paramPart))
					case _ => None
				}
			}
	}
	
	/**
	  * Keyword for describing method return values
	  */
	case object Return extends ScalaDocKeyword
	{
		override protected val keywordString = "return"
		override protected val orderIndex = 10
		override val padsToSameLength: Boolean = false
	}
	
	/**
	  * Keyword for describing file author
	  */
	case object Author extends ScalaDocKeyword
	{
		override protected val keywordString = "author"
		override protected val orderIndex = 12
		override val padsToSameLength: Boolean = false
	}
	
	/**
	  * Keyword for describing file creation time
	  */
	case object Since extends ScalaDocKeyword
	{
		override protected val keywordString = "since"
		override protected val orderIndex = 13
		override val padsToSameLength: Boolean = false
	}
	
	/**
	  * Keyword for describing function parameters
	  */
	case class Param(paramName: String) extends ScalaDocKeyword
	{
		override protected val keywordString = "param"
		override protected val orderIndex = 7
		override val padsToSameLength: Boolean = true
		
		override def toString = s"${super.toString} $paramName"
	}
	
	/**
	  * Keyword for describing generic type parameters
	  */
	case class TypeParam(paramName: String) extends ScalaDocKeyword
	{
		override protected val keywordString = "tparam"
		override protected val orderIndex = 8
		override val padsToSameLength: Boolean = false
		
		override def toString = s"${super.toString} $paramName"
	}
}
