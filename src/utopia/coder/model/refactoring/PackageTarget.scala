package utopia.coder.model.refactoring

import utopia.coder.model.scala.Package
import utopia.flow.collection.immutable.caching.LazyTree
import utopia.flow.collection.immutable.caching.iterable.CachingSeq
import utopia.flow.parse.file.FileExtensions._
import utopia.flow.util.logging.Logger

import java.nio.file.Path

/**
 * Used for targeting a specific package in a context where the path is only partially known.
 * Supports * as a package name, which means that all packages within that layer are included.
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
				val subParents = knownParents.tail
				tree.pathsToRootsWhereIterator { n => n.hasChildren && n.nav.fileName == topKnownParent }
					// TODO: use flatMap (targeting all) or find (targeting one)?
					.flatMap { pathToTopParent =>
						lazy val topParentPaths = pathToTopParent.map { _.nav }
						resolvePackagePaths(pathToTopParent.last, subParents).flatMap { parents =>
							val bottomParent = parents.last
							lazy val parentPaths = topParentPaths ++ parents.tail.map { _.nav }
							// Continues by identifying the targets under the located parent paths
							resolvePackagePaths(bottomParent, target).map { targetPath =>
								LocatedPackage(parentPaths, targetPath.map { _.nav })
							}
						}
					}
			
			// Case: No parents are known => Looks for the first target package instead
			case None =>
				val firstTargetDirName = target.parts.head
				tree.pathsToRootsWhereIterator { n => n.hasChildren && n.nav.fileName == firstTargetDirName }
					.flatMap { pathToFirstTarget =>
						val firstTarget = pathToFirstTarget.last
						lazy val parentPaths = pathToFirstTarget.map { _.nav }
						resolvePackagePaths(firstTarget, target.tail).map { targetPaths =>
							LocatedPackage(parentPaths, targetPaths.map { _.nav })
						}
					}
		}
	}
	
	// Returns whole package paths (i.e. lists all directories on each path)
	// Branches if * is listed as a package
	private def resolvePackagePaths(parent: LazyTree[Path], childPackages: Package) = {
		// Iterates down one layer at a time
		childPackages.parts.foldLeft(CachingSeq(Vector(parent))) { (parents, next) =>
			// Case: Invalid path
			if (parents.isEmpty)
				parents
			else {
				// Case: Next part targets all sub-directories
				if (next == "*")
					parents.flatMap { p => p.last.children.map { p :+ _ } }
				// Case: Next part targets a specific package / directory
				else
					parents.flatMap { p => p.last.children.find { _.nav.fileName == next }.map { p :+ _ } }
			}
		}
	}
}