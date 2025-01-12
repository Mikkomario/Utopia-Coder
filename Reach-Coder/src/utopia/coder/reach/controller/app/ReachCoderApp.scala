package utopia.coder.reach.controller.app

import utopia.coder.controller.app.{AppLogic, CoderApp, ListProjectsAppLogic, ShowProjectAppLogic}
import utopia.coder.reach.util.Common.jsonParser
import utopia.coder.reach.util.Common._

/**
  * The main application class for this project / module
  * @author Mikko Hilpinen
  * @since 30.5.2023, v1.0
  */
object ReachCoderApp extends App with CoderApp
{
	override protected val logicOptions: Iterable[AppLogic] = Vector(
		ReachCoderAppLogic,
		new ListProjectsAppLogic(projectsPath),
		new ShowProjectAppLogic(projectsPath))
	
	run(args.toIndexedSeq)
}
