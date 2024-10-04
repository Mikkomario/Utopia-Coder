package utopia.coder.vault.controller.refactoring

import utopia.coder.controller.refactoring.{Backup, ReplaceCode}
import utopia.coder.model.refactoring.PackageTarget
import utopia.coder.model.scala.Package
import utopia.coder.model.scala.datatype.ScalaType
import utopia.coder.model.scala.declaration.FunctionIdentifier
import utopia.coder.vault.controller.writer.database.AccessWriter
import utopia.coder.vault.util.VaultReferences._
import utopia.flow.collection.immutable.Pair
import utopia.flow.parse.string.Regex
import utopia.flow.util.TryExtensions._
import utopia.flow.util.Version
import utopia.flow.util.logging.Logger

import java.nio.file.Path

/**
 * Contains refactoring implementations intended to facilitate the migration to more recent Vault versions
 *
 * @author Mikko Hilpinen
 * @since 30.07.2024, v1.11.1
 */
object UpgradeVault
{
	// ATTRIBUTES   --------------------------
	
	private lazy val accessPackages = PackageTarget(Package("database.access.*"), recursive = true)
	// private lazy val manyAccessPackages = PackageTarget(Package("database.access.many"), recursive = true)
	
	
	// OTHER    ------------------------------
	
	/**
	 * Performs refactorings to facilitate migration between Vault versions
	 * @param sourceRoot The targeted "src" directory
	 * @param from Current version
	 * @param to Target version
	 * @param log Implicit logging implementation
	 * @param backup Implicit backup implementation
	 */
	def apply(sourceRoot: Path, from: Version, to: Version)(implicit log: Logger, backup: Backup) = {
		val v1p20 = Version(1, 20)
		if (from < v1p20 && to >= v1p20)
			to1p20(sourceRoot).logWithMessage(s"Failed to upgrade to $v1p20")
	}
	
	/**
	 * Upgrades a project to Vault v1.20, where .filter(Condition) was replaced with .apply(Condition)
	 * with different logic too.
	 * @param sourceRoot The targeted "src" directory
	 * @param log Implicit logging implementation used for logging certain failures
	 * @param backup Implicit backup implementation
	 * @return List of edited files.
	 *         Failure if editing failed for all targeted files.
	 */
	def to1p20(sourceRoot: Path)(implicit log: Logger, backup: Backup) = {
		val oldFilterMethodNames = Set("filter", "_filter")
		val oldFilterIdentifiers = oldFilterMethodNames.map { FunctionIdentifier(_, vault.condition) }
		
		// Finds sub-view implementations and replaces them with new ones
		val subAccessTargetRegex = Regex("Sub") + (Regex("Access") || "View").withinParentheses + Regex.escape('(') +
			(Regex("override val parent") || Regex("condition\\: Condition\\) extends")).withinParentheses
		val replaceAccessCompanions = ReplaceCode(accessPackages, subAccessTargetRegex) { i =>
			if (i.isObject) {
				println(s"Replacing ${ i.name } companion object")
				AccessWriter.accessCompanionObject(ScalaType.basic(i.name))
			}
			else {
				// Also replaces the filter function with apply
				println(s"Replacing ${ i.name } filter method")
				i -- oldFilterIdentifiers + AccessWriter.filteringApply(ScalaType.basic(i.name))
			}
		}
		val companionsResults = replaceAccessCompanions(sourceRoot)
		
		// Also replaces the other filter function implementations
		val manyAccessRegex = (Regex("Many") || "Unique").withinParentheses + Regex.word + "Access"
		val replaceFilters = ReplaceCode(accessPackages, Regex("trait ") + manyAccessRegex) { i =>
			if (i.isTrait && manyAccessRegex(i.name) && oldFilterIdentifiers.exists(i.contains)) {
				println(s"Replacing ${ i.name } filter method")
				i -- oldFilterIdentifiers + AccessWriter.filteringApply(ScalaType.basic(i.name))
			}
			else
				i
		}
		val filtersResult = replaceFilters(sourceRoot)
		
		Pair(companionsResults, filtersResult).toTryCatch.map { _.flatten }
	}
}
