package utopia.coder.vault.model.data.reference

import utopia.coder.model.scala.datatype.Reference
import utopia.flow.view.template.Extender

/**
 * Combination of references to traits generated for generic classes
 * @param modelRefs References related to model classes
 * @param dbProps Reference to XDbProps
 * @param dbPropsWrapper Reference to XDbPropsWrapper
 * @param dbModel References relating to database model classes
 * @param dbFactoryLike Reference to XDbFactoryLike
 * @author Mikko Hilpinen
 * @since 21.06.2024, v1.11
 */
case class GenericClassReferences(modelRefs: GenericClassModelReferences, dbProps: Reference, dbPropsWrapper: Reference,
                                  dbModel: GenericDbModelRefs, dbFactoryLike: Reference)
	extends Extender[GenericClassModelReferences]
{
	override def wrapped: GenericClassModelReferences = modelRefs
}