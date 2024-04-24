package utopia.coder.model.refactoring

import utopia.coder.model.scala.Package
import utopia.flow.collection.CollectionExtensions._
import utopia.flow.collection.immutable.caching.LazyTree
import utopia.flow.parse.file.FileExtensions._
import utopia.flow.util.logging.Logger

import java.nio.file.Path

/**
 * Used for targeting a specific package in a context where the path is only partially known
 * @param knownParents The package portion that should precede the targeted package,
 *                  but is not part of the targeted package
 * @param target Targeted package appearing under 'knownParents'
 * @author Mikko Hilpinen
 * @since 23.04.2024, v1.1
 */
case class PackageTarget(knownParents: Package = Package.empty, target: Package)
{
	// COMPUTED ---------------------------
	
	/**
	 * @return Name of the targeted package
	 */
	def name = target.parts.last
	
	
	// OTHER    ---------------------------
	
	/**
	 * Finds the directories targeted by this package target's 'target' portion
	 * @param root Root source directory
	 * @param log Implicit logging implementation for recording file access failures
	 * @return An iterator that returns all locations where this package is encountered under the specified
	 *         root package
	 */
	def locate(root: Path)(implicit log: Logger) = {
		val tree = root.toTree
		knownParents.notEmpty match {
			// Case: Some parents are known => Locates those first
			case Some(knownParents) =>
				val topKnownParent = knownParents.parts.head
				val subParents = knownParents.parts.tail
				tree.pathsToRootsWhereIterator { n => n.hasChildren && n.nav.fileName == topKnownParent }
					.flatMap { pathToTopParent =>
						val subParentPaths = subParents.foldLeftIterator(pathToTopParent.last.nav) { _/_ }.toVector
						val bottomParent = subParentPaths.last
						// Continues by identifying the targets under the located parent paths
						val targetPaths = target.parts.foldLeftIterator(bottomParent) { _/_ }.toVector
						
						if (targetPaths.last.exists)
							Some(LocatedPackage(pathToTopParent.map { _.nav } ++ subParentPaths, targetPaths))
						else
							None
					}
			
			// Case: No parents are known => Looks for the first target package instead
			case None =>
				val firstTargetDirName = target.parts.head
				val subDirNames = target.parts.tail
				tree.pathsToRootsWhereIterator { n => n.hasChildren && n.nav.fileName == firstTargetDirName }
					.flatMap { pathToFirstTarget =>
						val firstTarget = pathToFirstTarget.last.nav
						val otherTargets = subDirNames.foldLeftIterator(firstTarget) { _/_ }.toVector
						
						if (otherTargets.lastOption.getOrElse(firstTarget).exists)
							Some(LocatedPackage(pathToFirstTarget.dropRight(1).map { _.nav },
								firstTarget +: otherTargets))
						else
							None
					}
		}
	}
}