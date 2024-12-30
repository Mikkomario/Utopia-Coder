package utopia.coder.test

import utopia.coder.model.scala.code.CodeLine
import utopia.flow.util.StringExtensions._

/**
 * Tests automated code line -splitting
 *
 * @author Mikko Hilpinen
 * @since 26.12.2024, v1.1.2
 */
object LineSplitTest extends App
{
	private val str = "* @return This is a scaladoc line that describes a return value. We're trying to make this into a lengthy statement, so that it needs to be split. I think the splitting threshold is at about 100 characters. Let's make sure this line is long enough to achieve the length of 200 characters."
	private val l1 = CodeLine(str)
	l1.split.foreach(println)
	
	println()
	str.splitToLinesIterator(100).foreach(println)
}
