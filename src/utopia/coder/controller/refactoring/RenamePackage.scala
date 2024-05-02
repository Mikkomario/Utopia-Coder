package utopia.coder.controller.refactoring

import utopia.coder.model.refactoring.PackageTarget
import utopia.coder.model.scala.Package
import utopia.flow.collection.CollectionExtensions._
import utopia.flow.operator.equality.EqualsExtensions._
import utopia.flow.parse.file.FileConflictResolution.Rename
import utopia.flow.parse.file.FileExtensions._
import utopia.flow.util.NotEmpty
import utopia.flow.util.logging.{LogQueue, Logger}

import java.nio.file.{Path, Paths}

/**
 * Used for renaming or moving a specific package or a linear sequence of packages
 * @param target Targeted package / package sequence
 * @param to New name to assign to the target section of the 'target'.
 *           Will be placed under to the target's parent directory / package.
 * @author Mikko Hilpinen
 * @since 26.04.2024, v1.1
 */
case class RenamePackage(target: PackageTarget, to: Package)
{
	// OTHER    ------------------------------
	
	// TODO: Use less tryMap and more error-catching / logging
	// TODO: Use a collecting logger and return a TryCatch
	def apply(sourceRoot: Path)(implicit backup: Backup) = {
		implicit val logQueue: LogQueue = new LogQueue()
		target.locate(sourceRoot).toVector.groupBy { _.parents.lastOption.getOrElse(sourceRoot) }
			.flatMap { case (parent, packagesToRename) =>
				// Creates or otherwise acquires the renamed directory
				to.toPathIn(parent).createDirectories()
					// If directory-creation failed. Logs and skips this group of packages
					.logToOptionWithMessage(s"Failed to create the new package $to")
					.map { targetDirectory =>
						// Moves all the lowest level target directory contents to the new directory
						packagesToRename
							.flatMap { packageToRename =>
								val fileMoves = packageToRename.directory.iterateChildren { filesIter =>
									filesIter.filter { _.fileType ~== "scala" }.toVector.map { fileToMove =>
										// Before moving files, backs them up
										backup(fileToMove)
										fileToMove.moveTo(targetDirectory, Rename)
									}
								}
								fileMoves
									// If file-iteration failed, logs rather than interrupts the process
									.logToOptionWithMessage(s"Failed to acquire a list of files in $packageToRename")
									.map { fileMoves =>
										val (moveFailures, movedFiles) = fileMoves.divided
										// Logs move errors
										moveFailures.headOption.foreach { error =>
											logQueue(error, s"Failed to move ${
												moveFailures.size } of ${
												fileMoves.size } files from package $packageToRename")
										}
										val fromString = packageToRename.pck.toString
										fromString -> movedFiles
									}
							}
					}
					// 'renames' here contains multiple entries, containing
					//      1) String for the original package
					//      2) Files moved from that package
					.map { renames =>
						// Determines the full name of the new package
						val newPackageString = (packagesToRename.head.parents.map { _.fileName } ++ to.parts)
							.mkString(".")
						newPackageString -> renames
					}
			}
			// 'renames' here contains:
			//      1) New package name
			//      2) Original package names
			//      3) Moved files from each original package
			.map { case (newPackageString, renames) =>
				if (renames.exists { case (_, fileMoves) => fileMoves.nonEmpty }) {
					// Once all files have been moved, modifies the package statements
					// as well as import statements throughout the project
					// TODO: Implement
					???
				}
				
				???
			}
	}
}