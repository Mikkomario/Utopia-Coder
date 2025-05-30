package utopia.coder.vault.model.data.reference

import utopia.coder.model.scala.datatype.Reference

/**
 * Provides references to the generated (targeting) value access interfaces
 *
 * @author Mikko Hilpinen
 * @since 29.05.2025, v1.13
 * @param value Reference to the AccessXValue -class
 * @param values Reference to the AccessXValues -class
 * @param filtering Reference to the FilterXs -trait, if defined
 */
case class TargetingReferences(value: Reference, values: Reference, filtering: Option[Reference] = None)
{
	def valueFor(accessMany: Boolean) = if (accessMany) values else value
}