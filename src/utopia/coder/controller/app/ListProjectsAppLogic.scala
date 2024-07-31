package utopia.coder.controller.app

import utopia.coder.model.data.ProjectPaths
import utopia.flow.collection.immutable.Empty
import utopia.flow.collection.CollectionExtensions._
import utopia.flow.parse.file.container.{ObjectMapFileContainer, SaveTiming}
import utopia.flow.parse.json.JsonParser
import utopia.flow.util.console.{ArgumentSchema, CommandArguments}
import utopia.flow.util.logging.Logger

import java.nio.file.Path
import scala.concurrent.ExecutionContext

/**
 * Application logic for listing registered projects
 * @param projectsStorePath Path to the json file where the projects are stored
 * @param jsonParser Implicit json parsing logic used
 * @param exc Implicit execution context
 * @param log Implicit logging implementation
 * @author Mikko Hilpinen
 * @since 31.07.2024, v1.1.1
 */
class ListProjectsAppLogic(projectsStorePath: Path)
                          (implicit jsonParser: JsonParser, exc: ExecutionContext, log: Logger)
	extends AppLogic
{
	// ATTRIBUTES   --------------------------
	
	private lazy val projects = new ObjectMapFileContainer(projectsStorePath, ProjectPaths, SaveTiming.OnlyOnTrigger)
	
	
	// IMPLEMENTED  --------------------------
	
	override def name: String = "list"
	override def argumentSchema: Seq[ArgumentSchema] = Empty
	
	override def apply(args: CommandArguments): Unit = {
		projects.current.notEmpty match {
			case Some(projects) =>
				projects.toVector.sortBy { _._1 }.foreach { case (name, paths) => println(s"$name: ${ paths.root }") }
			case None => println("No projects have been registered yet")
		}
	}
}
