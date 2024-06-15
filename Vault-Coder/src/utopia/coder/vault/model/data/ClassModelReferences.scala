package utopia.coder.vault.model.data

import utopia.coder.model.scala.datatype.Reference
import utopia.flow.collection.immutable.Pair

/**
 * Contains references generated for class models & generic traits,
 * typically associated with the core module, if applicable
 * @param data Reference to the XData class or trait
 * @param stored Reference to the X class or trait
 * @param factory Reference to the XFactory trait
 * @param factoryWrapper Reference to the XFactoryWrapper trait
 * @param dataAndStoredLike References to 1) XDataLike trait and 2) StoredXLike trait.
 *                             None if no generic traits were written.
 * @author Mikko Hilpinen
 * @since 14.06.2024, v1.11
 */
case class ClassModelReferences(data: Reference, stored: Reference, factory: Reference, factoryWrapper: Reference,
                                dataAndStoredLike: Option[Pair[Reference]] = None)
