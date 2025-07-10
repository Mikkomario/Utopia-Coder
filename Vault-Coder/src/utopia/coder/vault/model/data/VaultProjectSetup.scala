package utopia.coder.vault.model.data

import utopia.coder.controller.refactoring.Backup
import utopia.coder.model.data.{Name, ProjectSetup}
import utopia.coder.model.scala.Package
import utopia.coder.vault.model.enumeration.Mutability
import utopia.flow.util.Version
import utopia.flow.util.logging.Logger

import java.nio.file.Path

/**
  * Represents project specific settings used when writing documents
  * @author Mikko Hilpinen
  * @since 1.9.2021, v0.1
  * @param dbModuleName Name of this project (the database portion)
  * @param modelPackage Package for the model and enum classes
  * @param databasePackage Package for the database interaction classes
  * @param sourceRoot Path to the export source directory
  * @param mergeSourceRoots Paths to the source roots where existing versions are read from and merged (optional)
  * @param version Current project version
  * @param defaultMutability Whether properties should be considered mutable or immutable by default
 * @param modelCanReferToDB Whether model classes are allowed to refer to database classes
  * @param prefixSqlProperties Whether a prefix should be added to sql properties, making them unique
  */
class VaultProjectSetup(val dbModuleName: Name, val modelPackage: Package, val databasePackage: Package,
                        override val sourceRoot: Path, backupRoot: Path,
                        override val mergeSourceRoots: Seq[Path], override val mergeConflictsFilePath: Path,
                        override val version: Option[Version], val defaultMutability: Mutability,
                        val modelCanReferToDB: Boolean, val prefixSqlProperties: Boolean)
                       (implicit val log: Logger)
	extends ProjectSetup
{
	// ATTRIBUTES   ------------------------
	
	override val backup: Backup = new Backup(mergeSourceRoots, backupRoot)
	
	/**
	  * @return Package that contains database access points
	  */
	lazy val accessPackage = databasePackage/"access"
	
	
	// COMPUTED ---------------------------
	
	/**
	  * @return Package that contains combined models
	  */
	def combinedModelPackage = modelPackage/"combined"
	
	/**
	  * @return Package that contains database access points that retrieve individual items
	  */
	def singleAccessPackage = accessPackage/"single"
	/**
	  * @return Package that contains database access points that retrieve multiple items at once
	  */
	def manyAccessPackage = accessPackage/"many"
	
	/**
	  * @return Package that contains from database read factories
	  */
	@deprecated("Replaced with factoryPackageFor(Boolean)", "v1.13")
	def factoryPackage = databasePackage/"factory"
	/**
	  * @return Package that contains database interaction models
	  */
	def dbModelPackage = databasePackage/"storable"
	/**
	 * @param targeting Whether writing targeting DB access classes
	 * @return Package that contains from database read factories
	 */
	def factoryPackageFor(targeting: Boolean) = databasePackage / (if (targeting) "reader" else "factory")
}
