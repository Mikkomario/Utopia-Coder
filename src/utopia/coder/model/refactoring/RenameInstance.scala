package utopia.coder.model.refactoring

import utopia.coder.controller.parsing.scala.ScalaParser
import utopia.coder.model.scala.Package
import utopia.coder.model.scala.declaration.InstanceDeclarationType
import utopia.flow.collection.CollectionExtensions._
import utopia.flow.operator.equality.EqualsExtensions._
import utopia.flow.parse.string.{IterateLines, Regex}
import utopia.flow.parse.file.FileExtensions._
import utopia.flow.util.logging.Logger
import utopia.flow.util.StringExtensions._

import java.nio.file.Path
import scala.io.Codec

/**
 * Used for renaming a specific instance declaration or multiple such declarations
 *
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
			.map { edits =>
				// Once all primary files have been edited, edits all the references
				val flatEdits = edits.flatten
				val editsPerPackage = flatEdits
					.groupMap { _._1.pck } { case (_, _, from, to) => from -> to }
					.view.mapValues { _.toMap }.toMap
					.withDefaultValue(Map.empty)
				val completedEditPerFile = flatEdits.map { case (_, file, from, _) => file -> from }.toMap
				
				// TODO: Again, take backups before editing
				sourceRoot.toTree.nodesBelowIterator.filter { _.nav.fileType ~== "scala" }.tryForeach { file =>
					// Reads the package and the imports section of the file in order to see whether
					// that file is affected by these changes
					IterateLines.fromPath(file.nav){ iter =>
						val linesIter = iter.pollable
						val (filePackage, imports) = ScalaParser.readPackageAndImportsFrom(linesIter)
						
						// Applies edits from:
						//      1) All changes in the same package
						//      2) All modified import references
						// But not the change made originally within that file
						val editsToApply = editsPerPackage(filePackage) ++
							imports.flatMap { ref =>
								editsPerPackage(ref.packagePath).get(ref.target).map { ref.target -> _ }
							} -- completedEditPerFile.get(file.nav)
						
						// TODO: If there are edits to apply, open the file in edit mode and apply the changes
						???
					}
				}
				
				// TODO: Continue
			}
	}
}