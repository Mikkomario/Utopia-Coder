package utopia.coder.model.refactoring

import utopia.coder.model.scala.Package
import utopia.flow.parse.file.FileExtensions._

import java.nio.file.Path

/**
 * Represents a package located using [[PackageTarget]]
 * @param parents The directories that represents packages above the targeted range
 * @param targets Targeted package directories
 * @author Mikko Hilpinen
 * @since 23.04.2024, v1.1
 */
case class LocatedPackage(parents: Vector[Path], targets: Vector[Path])
{
	// ATTRIBUTES   -------------------------
	
	/**
	 * A package instance that matches this package by location
	 */
	lazy val pck = Package((parents ++ targets).map { _.fileName })
	
	
	// COMPUTED -----------------------------
	
	/**
	 * @return Targeted directory
	 */
	def directory = targets.last
	
	
	// IMPLEMENTED  -------------------------
	
	override def toString = pck.toString
}