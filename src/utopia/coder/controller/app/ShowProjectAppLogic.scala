package utopia.coder.controller.app

import utopia.coder.model.data.{Filter, LazyProjectPaths}
import utopia.flow.parse.file.FileExtensions._
import utopia.flow.util.EitherExtensions._
import utopia.flow.util.console.CommandArguments
import utopia.flow.util.logging.Logger
import utopia.flow.view.immutable.caching.Lazy

import java.nio.file.Path
import scala.concurrent.ExecutionContext

/**
 * Application logic that shows project information
 * @param projectsStoreLocation Path to the file where stored projects are listed
 * @param supportsAlternativeMergeRoots Whether inclusion of multiple merge / source directories is supported
 *                                      (default = false)
 * @param exc Implicit execution context
 * @param log Implicit logger
 * @author Mikko Hilpinen
 * @since 31.07.2024, v1.1.1
 */
class ShowProjectAppLogic(override val projectsStoreLocation: Path,
                          override val supportsAlternativeMergeRoots: Boolean = false)
                         (implicit override val exc: ExecutionContext, override val log: Logger)
	extends CoderAppLogic
{
	override def name: String = "show"
	
	override protected def run(args: CommandArguments, paths: LazyProjectPaths, filter: Lazy[Option[Filter]],
	                           targetGroup: Option[String]): Boolean =
	{
		println(s"Root: ${ paths.root }")
		println(s"Input: ${ paths.input.relativeTo(paths.root).either }")
		println(s"Output: ${ paths.output.relativeTo(paths.root).either }")
		println(s"Sources: [${ paths.sources.map { _.relativeTo(paths.root).either }.mkString(", ") }]")
		true
	}
}
