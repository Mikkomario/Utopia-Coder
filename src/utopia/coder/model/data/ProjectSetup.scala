package utopia.coder.model.data

import utopia.coder.controller.refactoring.Backup
import utopia.coder.model.merging.MergeConflict
import utopia.flow.collection.CollectionExtensions._
import utopia.flow.parse.file.FileExtensions._
import utopia.flow.util.StringExtensions._
import utopia.flow.util.Version
import utopia.flow.util.logging.Logger

import java.nio.file.Path

object ProjectSetup
{
	// OTHER    -------------------------
	
	/**
	  * @param mergeConflictsFilePath Path to the text file where merge conflicts shall be documented
	  * @param sourceRoot Path to the export source directory
	  * @param backupRoot Path to the generated backup directory
	 * @param mergeSourceRoots Paths to the source roots where existing versions are read from and merged (may be empty)
	  * @param version Current project version, if known
	  * @param log Implicit logging implementation used for non-critical error-logging
	 * @return A new project setup instance
	  */
	def apply(mergeConflictsFilePath: Path, sourceRoot: Path, backupRoot: Path,
	          mergeSourceRoots: Vector[Path] = Vector(), version: Option[Version] = None)
	         (implicit log: Logger): ProjectSetup =
		new _ProjectSetup(sourceRoot, backupRoot, mergeSourceRoots, mergeConflictsFilePath, version)
	
	
	// NESTED   -------------------------
	
	private class _ProjectSetup(override val sourceRoot: Path, backupRoot: Path,
	                            override val mergeSourceRoots: Vector[Path],
	                            override val mergeConflictsFilePath: Path, override val version: Option[Version])
	                           (implicit log: Logger)
		extends ProjectSetup
	{
		override val backup: Backup = new Backup(mergeSourceRoots, backupRoot)
	}
}

/**
  * Represents project specific settings used when writing documents
  * @author Mikko Hilpinen
  * @since 1.9.2021, Vault Coder v0.1
  */
trait ProjectSetup
{
	// ABSTRACT -------------------------
	
	/**
	  * @return Path to the export source directory
	  */
	def sourceRoot: Path
	/**
	  * @return Paths to the source roots where existing versions are read from and merged (may be empty)
	  */
	def mergeSourceRoots: Vector[Path]
	/**
	  * @return Path to the text file where merge conflicts shall be documented
	  */
	def mergeConflictsFilePath: Path
	/**
	 * @return Backup logic used in this project build
	 */
	def backup: Backup
	/**
	  * @return Current project version, if known
	  */
	def version: Option[Version]
	
	
	// OTHER    -------------------------
	
	/**
	  * Records conflicts to a local file
	  * @param conflicts Merge conflicts to record
	  * @param header Header to assign for these conflicts (call-by-name)
	  */
	def recordConflicts(conflicts: Vector[MergeConflict], header: => String) = {
		if (conflicts.nonEmpty) {
			mergeConflictsFilePath.createParentDirectories().flatMap { path =>
				path.appendLines(Vector("", "", s"// $header ${"-" * 10}") ++ conflicts.flatMap { conflict =>
					if (conflict.readVersion.nonEmpty || conflict.generatedVersion.nonEmpty)
						("" +: conflict.description.notEmpty.map { "// " + _ }.toVector) ++
							("// Old Version" +: conflict.readVersion.map { _.toString }) ++
							("// New Version" +: conflict.generatedVersion.map { _.toString })
					else
						Vector(s"// ${conflict.description}")
				})
			}.failure.foreach { error =>
				println(s"Failed to write conflicts document due to an error: ${error.getMessage}")
			}
		}
	}
}
