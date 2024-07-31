package utopia.coder.vault.main

import utopia.coder.controller.app.{AppLogic, CoderApp, ListProjectsAppLogic, ShowProjectAppLogic}
import utopia.coder.vault.util.Common._

/**
  * The command line application for this project, which simply reads data from a json file and outputs it to a certain
  * location
  * @author Mikko Hilpinen
  * @since 4.9.2021, v0.1
  */
object VaultCoderApp extends App with CoderApp
{
	// IMPLEMENTED  ----------------------
	
	override protected def logicOptions: Iterable[AppLogic] =
		Vector(MainAppLogic, TableReadAppLogic, UpgradeVaultAppLogic,
			new ListProjectsAppLogic(projectsPath),
			new ShowProjectAppLogic(projectsPath, supportsAlternativeMergeRoots = true))
	
	
	// APP CODE -------------------------
	
	run(args.toIndexedSeq)
}
