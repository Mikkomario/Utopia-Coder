package utopia.coder.model.refactoring

import utopia.flow.util.StringExtensions._

/**
 * Represents a result acquired using an [[ExtractorRegex]]
 * @param from The original string from which data is extracted
 * @param range The extracted range of the original string (including before & after portions)
 * @param targetRange The range of the "target" portion in the original string
 * @author Mikko Hilpinen
 * @since 23.04.2024, v1.1
 */
case class Extraction(from: String, range: Range, targetRange: Range)
{
	// ATTRIBUTES   ------------------------
	
	/**
	 * The whole extracted string (including before & after portions)
	 */
	lazy val all = from.slice(range)
	/**
	 * The extracted "target" portion
	 */
	lazy val target = from.slice(targetRange)
	
	
	// OTHER    ----------------------------
	
	/**
	 * @param newTarget New name for the target portion of this extraction
	 * @return Extracted string where the target portion has been renamed
	 */
	def renamed(newTarget: String) =
		s"${ from.slice(range.start, targetRange.start) }$newTarget${ from.slice(targetRange.end, range.end) }"
}