package utopia.coder.vault.main

import utopia.coder.controller.app.{AppLogic, CoderAppLogic}
import utopia.coder.controller.refactoring.Backup
import utopia.coder.model.data.{Filter, LazyProjectPaths}
import utopia.coder.vault.controller.reader.ClassReader
import utopia.coder.vault.controller.refactoring.UpgradeVault
import utopia.coder.vault.util.Common
import utopia.flow.collection.immutable.{Empty, Pair}
import utopia.flow.generic.casting.ValueConversions._
import utopia.flow.parse.file.FileExtensions._
import utopia.flow.util.Version
import utopia.flow.util.console.{ArgumentSchema, CommandArguments}
import utopia.flow.util.logging.Logger
import utopia.flow.view.immutable.caching.Lazy

import java.nio.file.Path
import scala.concurrent.ExecutionContext

/**
 * An application logic for performing Vault version upgrades
 *
 * @author Mikko Hilpinen
 * @since 30.07.2024, v1.11.1
 */
object UpgradeVaultAppLogic extends CoderAppLogic
{
	override protected implicit def exc: ExecutionContext = Common.exc
	override protected implicit def log: Logger = Common.log
	
	override def name: String = "upgrade"
	
	override protected def projectsStoreLocation: Path = Common.projectsPath
	override protected def supportsAlternativeMergeRoots: Boolean = true
	
	override def argumentSchema = {
		val defaultArgs = super.argumentSchema
		val from = ArgumentSchema("from", defaultValue = "v1.0", help = "Currently used Vault version")
		val to = ArgumentSchema("to", defaultValue = "v1.20", help = "Next Vault version")
		(defaultArgs.head +: Pair(from, to)) ++ defaultArgs.tail
	}
	
	override protected def run(args: CommandArguments, paths: LazyProjectPaths, filter: Lazy[Option[Filter]],
	                           targetGroup: Option[String]): Boolean =
	{
		// Checks whether targeting a multimodal project
		val sources = paths.sources ++ ClassReader.moduleReferencesFrom(paths.input, paths.root).getOrElse(Empty)
			.flatMap { _.sources }
		
		val currentVersion = Version(args("from").getString)
		val targetVersion = Version(args("to").getString)
		if (sources.isEmpty) {
			println("No sources are defined for this project => Can't perform the refactoring")
			false
		}
		else {
			MainAppLogic.backup(paths.output)
			val backupDir = paths.output/"backup"
			implicit val backup: Backup = new Backup(sources, backupDir)
			sources.foreach { src =>
				println(s"Upgrading $src from $currentVersion to $targetVersion")
				UpgradeVault(src, currentVersion, targetVersion)
			}
			println("Refactoring completed")
			true
		}
	}
}
