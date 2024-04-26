package utopia.coder.controller.refactoring

import utopia.coder.controller.parsing.scala.ScalaParser
import utopia.coder.model.refactoring.{ExtractorRegex, PackageTarget}
import utopia.coder.model.scala.declaration.InstanceDeclarationType
import utopia.flow.collection.CollectionExtensions._
import utopia.flow.operator.equality.EqualsExtensions._
import utopia.flow.parse.file.FileExtensions._
import utopia.flow.parse.string.{IterateLines, Regex}
import utopia.flow.util.logging.Logger

import java.nio.file.Path
import scala.io.Codec
import scala.util.Success

/**
 * Used for renaming a specific instance declaration or multiple such declarations
 * @param targetPackage Package or packages where the targeted files are located
 * @param targetType Targeted instance type
 * @param targetInstance A method for identifying the targeted instances, as well as to extract the part to replace
 * @param to Renaming for the target section of 'targetInstance'
 * @author Mikko Hilpinen
 * @since 23.04.2024, v1.1
 */
case class RenameInstance(targetPackage: PackageTarget, targetType: InstanceDeclarationType,
                          targetInstance: ExtractorRegex, to: String)
{
	// ATTRIBUTES   -----------------
	
	implicit val codec: Codec = Codec.UTF8
	
	private lazy val targetTypeRegex = Regex(targetType.keyword) + Regex.whiteSpace
	
	
	// OTHER    ---------------------
	
	/**
	 * Performs this renaming operation within the specified source root directory
	 * @param sourceRoot Directory that represents the root / first package and contains all source files
	 * @param log Implicit logging implementation for recording certain file-access failures
	 * @return Success or failure
	 */
	def apply(sourceRoot: Path)(implicit log: Logger) = {
		// Locates the target packages
		targetPackage.locate(sourceRoot)
			.toVector
			.tryMap { locatedPackage =>
				// Renames the targeted package instances by editing their files
				locatedPackage.directory
					.tryIterateChildren { childIter =>
						// Identifies the files to rename
						// Contains 1) Targeted primary scala file, 2) original string, 3) renamed string
						val filesToRename = childIter.filter { _.fileType ~== "scala" }
							.flatMap { scalaFile =>
								targetInstance(scalaFile.fileName).map { extraction =>
									(scalaFile, extraction.all, extraction.renamed(to))
								}
							}
							.toVector
						
						// Performs the renaming within the primary files
						// TODO: Take backups
						filesToRename
							.tryMap { case (file, from, to) =>
								file.edit { editor =>
									// Finds the instance declaration line to replace
									editor.nextLineIterator
										.findMap { line =>
											targetTypeRegex.endIndexIteratorIn(line).nextOption()
												.flatMap { instanceIndex =>
													if (line.drop(instanceIndex).contains(from))
														Some(line.replaceFirst(from, to))
													else
														None
												}
										}
										.foreach { rewrittenLine =>
											// Performs the overwrite
											editor.overwrite(rewrittenLine)
											// Leaves the rest of the lines unedited
											editor.flush()
										}
									(locatedPackage, file, from, to)
								}
							}
					}
			}
			.flatMap { edits =>
				// Once all primary files have been edited, edits all the references
				val flatEdits = edits.flatten
				val editsPerPackage = flatEdits
					.groupMap { _._1.pck } { case (_, _, from, to) => from -> to }
					.view.mapValues { _.toMap }.toMap
					.withDefaultValue(Map.empty)
				val completedEditPerFile = flatEdits.map { case (_, file, from, _) => file -> from }.toMap
				
				// TODO: Again, take backups before editing
				sourceRoot.toTree.nodesBelowIterator.filter { _.nav.fileType ~== "scala" }.tryForeach { fileNode =>
					val file = fileNode.nav
					// Reads the package and the imports section of the file in order to see whether
					// that file is affected by these changes
					IterateLines
						.fromPath(file) { iter =>
							val linesIter = iter.pollable
							ScalaParser.readPackageAndImportsFrom(linesIter)
						}
						.flatMap { case (filePackage, imports) =>
							// Applies edits from:
							//      1) All changes in the same package
							//      2) All modified import references
							// But not the change made originally within that file
							val editsToApply = editsPerPackage(filePackage) ++
								imports.flatMap { ref =>
									editsPerPackage(ref.packagePath).get(ref.target).map { ref.target -> _ }
								} -- completedEditPerFile.get(file)
							
							// Case: There are edits to apply => Rewrites the file lines
							if (editsToApply.nonEmpty) {
								file.edit { editor =>
									editor.mapRemaining { line =>
										editsToApply.foldLeft(line) { case (line, (from, to)) =>
											line.replaceAll(from, to)
										}
									}
								}
							}
							// Case: No edits to apply => Skips this file
							else
								Success(())
						}
				}
			}
	}
}