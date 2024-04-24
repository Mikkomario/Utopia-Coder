package utopia.coder.model.refactoring

import utopia.coder.model.refactoring.ExtractorRegex.standardPartRegex
import utopia.flow.collection.CollectionExtensions._
import utopia.flow.parse.string.Regex
import utopia.flow.util.Mutate
import utopia.flow.util.StringExtensions._

object ExtractorRegex
{
	// ATTRIBUTES   ------------------------
	
	private val standardCharRegex = Regex.letterOrDigit || Regex.escape('_')
	private val standardPartRegex = standardCharRegex.anyTimes
}

/**
 * A regular expression used for extracting a keyword from a specific context
 *
 * @author Mikko Hilpinen
 * @since 23.04.2024, v11.1
 */
case class ExtractorRegex(before: Regex = standardPartRegex, target: Regex, after: Regex = standardPartRegex)
{
	// OTHER    ------------------------
	
	/**
	 * Mutates the before portion of this regex
	 * @param f A mutator function
	 * @return Mutated copy of this regex
	 */
	def mapBefore(f: Mutate[Regex]) = copy(before = f(before))
	/**
	 * Mutates the after portion of this regex
	 * @param f A mutator function
	 * @return Mutated copy of this regex
	 */
	def mapAfter(f: Mutate[Regex]) = copy(after = f(after))
	
	/**
	 * @param prefix A prefix to add to this regex
	 * @return A prepended copy of this regex
	 */
	def +:(prefix: Regex) = mapBefore { prefix + _ }
	/**
	 * @param suffix A suffix to add to this regex
	 * @return An appended copy of this regex
	 */
	def :+(suffix: Regex) = mapAfter { _ + suffix }
	
	/**
	 * Attempts to extract the first match from the specified string
	 * @param string A string
	 * @return First successful extraction acquired from the specified string.
	 *         None if that string didn't contain any matches.
	 */
	def apply(string: String) = before.notEmpty match {
		// Case: Before part defined => Starts by finding potential starting locations
		case Some(before) =>
			before.rangesIteratorIn(string).findMap { beforeRange =>
				val afterBefore = string.drop(beforeRange.end)
				after.notEmpty match {
					// Case: After portion defined as well => Finds potential ending locations
					case Some(after) =>
						after.rangesIteratorIn(afterBefore).findMap { relativeAfterRange =>
							// Selects the first option where the in-between range is acceptable
							val targetRange = beforeRange.end until (beforeRange.end + relativeAfterRange.start)
							val targetString = string.slice(targetRange)
							if (target(targetString))
								Some(Extraction(string,
									beforeRange.start until (beforeRange.end + relativeAfterRange.end),
									targetRange))
							else
								None
						}
					// Case: No end portion defined => Continues if the target is joinable to the found beginning
					case None =>
						target.firstRangeFrom(afterBefore).filter { _.start == 0 }.map { relativeTargetRange =>
							val targetRange = beforeRange.end until (beforeRange.end + relativeTargetRange.end)
							Extraction(string, beforeRange.start until targetRange.end, targetRange)
						}
				}
			}
		// Case: No before part defined
		case None =>
			after.notEmpty match {
				// Case: Only after part defined => Finds the earliest after part that is joinable to a valid target
				case Some(after) =>
					after.rangesIteratorIn(string).findMap { afterRange =>
						target.rangesIteratorIn(string).takeWhile { _.start <= afterRange.start }
							.filter { _.end >= afterRange.start }
							.findMap { targetRange =>
								// Crops the target so that it won't overlap with the ending portion
								val croppedTargetRange = targetRange.start until afterRange.start
								// The cropped target must be acceptable by the target regex
								val targetString = string.slice(croppedTargetRange)
								if (target(targetString))
									Some(Extraction(string, croppedTargetRange.start until afterRange.end,
										croppedTargetRange))
								else
									None
							}
					}
				// Case: Neither before nor after has been defined => Finds the first target match
				case None =>
					target.firstRangeFrom(string).map { targetRange => Extraction(string, targetRange, targetRange) }
			}
	}
}