package utopia.coder.vault.model.data

import utopia.coder.model.scala.datatype.Reference

/**
 * Contains detailed references to generated database model traits.
 * Applicable for generic classes only.
 * @param modelLike Reference to XModelLike[Repr]
 * @param factoryLike Reference to XModelFactoryLike[A, S, Repr]
 * @param model Reference to XModel
 * @param factory Reference to XModelFactory
 * @author Mikko Hilpinen
 * @since 21.06.2024, v1.11
 */
case class GenericDbModelRefs(modelLike: Reference, factoryLike: Reference, model: Reference, factory: Reference)
