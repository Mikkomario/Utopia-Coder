package utopia.coder.model.enumeration

import utopia.coder.model.refactoring.{ExtractorRegex, PackageTarget}

/**
 * An enumeration used when specifying what (kind of) entity / entities is/are targeted by a refactoring operation
 *
 * @author Mikko Hilpinen
 * @since 23.04.2024, v1.1
 */
sealed trait RefactoringTarget
{
	
}

// TODO: Replace with another approach?
object RefactoringTarget
{
	case class ClassCategory(targetedPackage: PackageTarget, className: ExtractorRegex)
	
	case class SpecificPackage(target: PackageTarget)
	
}