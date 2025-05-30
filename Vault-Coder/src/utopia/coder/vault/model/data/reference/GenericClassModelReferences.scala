package utopia.coder.vault.model.data.reference

import utopia.coder.model.scala.datatype.Reference

/**
 * Contains references to model traits generated only for generic classes.
 * @param hasProps Reference to HasXProps trait
 * @param dataLike Reference to XDataLike[Repr] trait
 * @param storedLike Reference to XLike[Repr]
 * @author Mikko Hilpinen
 * @since 21.06.2024, v1.11
 */
case class GenericClassModelReferences(hasProps: Reference, dataLike: Reference, storedLike: Reference)
