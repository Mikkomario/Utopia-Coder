package utopia.coder.controller.refactoring

import utopia.coder.controller.parsing.scala.ScalaParser
import utopia.coder.model.refactoring.PackageTarget
import utopia.coder.model.scala.declaration.InstanceDeclaration
import utopia.flow.parse.file.FileExtensions._
import utopia.flow.parse.string.{IterateLines, Regex}
import utopia.flow.util.Mutate
import utopia.flow.util.TryExtensions._
import utopia.flow.util.logging.Logger

import java.nio.file.Path
import scala.io.Codec

object ReplaceCode
{
	/**
	 * Creates a new refactoring logic
	 * @param targetPackage Targeted package or packages
	 * @param identifierRegex Regular expression that must appear in at least one line in a document,
	 *                        for that document to be edited.
	 * @param modifyInstance A function which modifies an instance declaration somehow.
	 *                       Called for all instance declarations in the targeted files.
	 * @return
	 */
	def apply(targetPackage: PackageTarget, identifierRegex: Regex)(modifyInstance: Mutate[InstanceDeclaration]) =
		new ReplaceCode(targetPackage, identifierRegex)(modifyInstance)
}

/**
 * A refactoring logic used for replacing a declaration with other declarations.
 * @param targetPackage Targeted package or packages
 * @param identifierRegex Regular expression that must appear in at least one line in a document,
 *                        for that document to be edited.
 * @param modifyInstance A function which modifies an instance declaration somehow.
 *                       Called for all instance declarations in the targeted files.
 * @author Mikko Hilpinen
 * @since 30.07.2024, v1.1.1
 */
class ReplaceCode(targetPackage: PackageTarget, identifierRegex: Regex)
                 (modifyInstance: Mutate[InstanceDeclaration])
{
	// COMPUTED ----------------------
	
	private implicit def codec: Codec = Codec.UTF8
	
	
	// OTHER    ----------------------
	
	/**
	 * Performs this refactoring process in the specified source root directory
	 * @param sourceRoot Targeted directory of source files (the "src" directory)
	 * @param log Implicit logging implementation used for logging some partial failures
	 * @param backup Implicit backup implementation for backing up files before editing them
	 * @return Edited files. Failure if editing failed for all files.
	 */
	def apply(sourceRoot: Path)(implicit log: Logger, backup: Backup) = {
		// Locates the targeted packages
		targetPackage.locate(sourceRoot)
			.flatMap { p => p.targets }.distinct
			.map { targetDirectory =>
				// Identifies the files which contain targeted strings
				targetDirectory.iterateChildren { files =>
					files
						.filter { file =>
							file.fileType == "scala" &&
								IterateLines.fromPath(file) { _.exists(identifierRegex.existsIn) }
									.log.getOrElse(false)
						}
						// Edits the targeted files
						.map { edit(_) }
						.toVector
				}
			}
			// Converts the refactoring results into a single TryCatch
			.map { _.flatMapCatching { _.toTryCatch } }.toTryCatch.map { _.flatten }
	}
	
	private def edit(filePath: Path)(implicit backup: Backup) = {
		// Backs up the file before editing it
		backup(filePath)
		// Performs the edit and writes a new version of the file, overwriting the existing file
		ScalaParser(filePath).flatMap { _.mapInstanceDeclarations(modifyInstance).writeTo(filePath) }
	}
}
