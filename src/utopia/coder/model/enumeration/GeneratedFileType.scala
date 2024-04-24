package utopia.coder.model.enumeration

import utopia.flow.operator.equality.EqualsFunction
import utopia.flow.util.{OpenEnumeration, OpenEnumerationValue}

/**
 * An open enumeration for different types of generated files.
 * Typically these match specific process phases.
 * May be targeted by refactoring requests.
 * @author Mikko Hilpinen
 * @since 23.04.2024, v1.1
 */
trait GeneratedFileType extends OpenEnumerationValue[String]
{
	// ABSTRACT ----------------------
	
	/**
	 * @return Keyword used for identifying this file type from user input
	 */
	def keyword: String
	
	
	// IMPLEMENTED  -----------------
	
	override def identifier: String = keyword
}

object GeneratedFileType
	extends OpenEnumeration[GeneratedFileType, String](identifiersMatch = EqualsFunction.stringCaseInsensitive)
