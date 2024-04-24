package utopia.coder.model.enumeration

import utopia.flow.operator.equality.EqualsFunction
import utopia.flow.util.{OpenEnumeration, OpenEnumerationValue}

/**
 * An open enumeration for categorizing types of items that may be merged together
 *
 * @author Mikko Hilpinen
 * @since 23.04.2024, v1.1
 */
trait MergeGroup extends OpenEnumerationValue[String]
{
	// ABSTRACT ------------------------
	
	/**
	 * @return Keyword used to represent this enumeration type
	 */
	def keyword: String
	
	
	// IMPLEMENTED  --------------------
	
	override def identifier: String = keyword
}

object MergeGroup extends OpenEnumeration[MergeGroup, String](identifiersMatch = EqualsFunction.stringCaseInsensitive)


