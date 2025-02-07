package utopia.coder.reach.controller.app

import utopia.coder.controller.app.CoderAppLogic
import utopia.coder.model.data.{Filter, LazyProjectPaths, NamingRules, ProjectSetup}
import utopia.coder.model.enumeration.NameContext.FileName
import utopia.coder.reach.controller.reader.ComponentFactoryReader
import utopia.coder.reach.controller.writer.ComponentFactoryWriter
import utopia.coder.reach.util.Common
import utopia.flow.parse.file.FileExtensions._
import utopia.flow.time.Today
import utopia.flow.util.TryExtensions._
import utopia.flow.util.console.CommandArguments
import utopia.flow.util.logging.Logger
import utopia.flow.view.immutable.View
import utopia.flow.view.immutable.caching.Lazy

import java.nio.file.Path
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

/**
  * Main application logic for the Reach Coder application
  * @author Mikko Hilpinen
  * @since 30.5.2023, v1.0
  */
object ReachCoderAppLogic extends CoderAppLogic
{
	// IMPLEMENTED  ---------------------------
	
	override protected implicit def exc: ExecutionContext = Common.exc
	override protected implicit def log: Logger = Common.log
	
	override def name: String = "main"
	override protected def projectsStoreLocation: Path = Common.projectsPath
	override protected def supportsAlternativeMergeRoots: Boolean = false
	
	override protected def run(args: CommandArguments, paths: LazyProjectPaths, filter: Lazy[Option[Filter]],
	                           targetGroup: Option[String]): Boolean =
	{
		val input = paths.input
		if (input.notExists) {
			println(s"The specified input path ${input.toAbsolutePath} doesn't exist")
			false
		}
		else {
			// Creates the output directory
			paths.output.asExistingDirectory.flatMap { output =>
				// Moves out the previous build files, if possible
				(output / "last-build").asExistingDirectory.flatMap { lastBuildDir =>
					// Deletes the previous backup
					lastBuildDir.deleteContents().logWithMessage("Failed to delete last build")
					// Moves the last build files to the backup directory, if possible
					output.iterateChildren {
						_.filterNot { _ == lastBuildDir }.map { _.moveTo(lastBuildDir) }.toTryCatch.logToTry
					}.flatten
				}.logWithMessage("Failed to move the previous build")
				
				// Processes the input file/files
				val mergeRoot = paths.srcView.mapValue { _.headOption }
				if (input.isDirectory)
					input.allRegularFileChildrenOfType("json").flatMap { inputPaths =>
						inputPaths.map { process(_, output, mergeRoot, filter) }.toTryCatch.logToTry.map { _.flatten }
					}
				else
					process(input, output, mergeRoot, filter)
			} match {
				case Success(results) =>
					println(s"Successfully wrote ${ results.size } component factory files")
					true
				case Failure(error) =>
					error.printStackTrace()
					println("Component writing process failed")
					false
			}
		}
	}
	
	
	// OTHER    ----------------------------
	
	private def process(inputPath: Path, outputPath: Path, mergeRoot: View[Option[Path]],
	                    filter: View[Option[Filter]]) =
	{
		// Reads the input path
		ComponentFactoryReader(inputPath).flatMap { projectData =>
			// Writes the new files
			implicit val naming: NamingRules = NamingRules.default
			val mergeConflictsFileName = {
				val base = s"${(projectData.name.inContext(FileName) ++
					Vector("merge", "conflicts", Today.toString)).fileName}"
				projectData.version match {
					case Some(version) => s"$base${naming(FileName).separator}$version.txt"
					case None => s"$base.txt"
				}
			}
			implicit val setup: ProjectSetup = ProjectSetup(
				mergeConflictsFilePath = outputPath/mergeConflictsFileName,
				sourceRoot = subDirectory(outputPath, "src"),
				backupRoot = subDirectory(outputPath, "backup"),
				mergeSourceRoots = mergeRoot.value.toVector,
				version = projectData.version
			)
			val factoriesToWrite = filter.value match {
				case Some(filter) => projectData.factories.filter { f => filter(f.componentName) }
				case None => projectData.factories
			}
			factoriesToWrite.map { ComponentFactoryWriter(_) }.toTryCatch.logToTry
		}
	}
}
