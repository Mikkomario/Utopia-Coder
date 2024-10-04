package utopia.coder.controller.refactoring

import utopia.flow.collection.CollectionExtensions._
import utopia.flow.parse.file.FileConflictResolution.Fail
import utopia.flow.parse.file.FileExtensions._
import utopia.flow.util.logging.Logger
import utopia.flow.util.TryExtensions._

import java.nio.file.Path
import scala.collection.mutable

/**
 * An interface used for backing up files before editing them
 * @author Mikko Hilpinen
 * @since 24.04.2024, v1.1
 */
class Backup(sourceRoots: Iterable[Path], backupRoot: Path)(implicit log: Logger)
{
	// ATTRIBUTES   ------------------------
	
	// Collects a list of files that have already been backed up, in order to avoid overwrites
	private val backedUpFiles = mutable.Set[Path]()
	
	
	// OTHER    ----------------------------
	
	/**
	 * Backs up a file
	 * @param file File path to back up
	 */
	def apply(file: Path): Unit = {
		// /home/mikko/Workspace/IDEA/Fuel/Fuel-Server-Core/src/aac/scala/fuel/core/server/database/FuelTables.scala
		// Only backs up each file once
		if (!backedUpFiles.contains(file) && file.isRegularFile) {
			val destination = sourceRoots.findMap { file.relativeTo(_).toOption } match {
				case Some(relativePath) => backupRoot/relativePath
				case None =>
					log(s"Warning: $file is not located in any of the available source roots: ${
						sourceRoots.mkString(", ") }")
					backupRoot/s"other/${ file.fileName }"
			}
			destination.createDirectories().map { file.copyAs(_, Fail) }.log
		}
	}
}
