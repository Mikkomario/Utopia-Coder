package utopia.coder.vault.model.enumeration

import utopia.flow.collection.immutable.Pair
import utopia.flow.operator.enumeration.Binary

/**
 * An enumeration for signifying whether something is mutable or immutable
 *
 * @author Mikko Hilpinen
 * @since 04.06.2024, v1.11
 */
sealed trait Mutability extends Binary[Mutability]
{
	// ABSTRACT ----------------------
	
	/**
	 * @return Whether this mutability allows changes
	 */
	def isMutable: Boolean
	
	
	// COMPUTED ----------------------
	
	/**
	 * @return Whether this mutability doesn't allow any changes
	 */
	def isImmutable: Boolean = !isMutable
	
	
	// IMPLEMENTED  ------------------
	
	override def self = this
	
	override def compareTo(o: Mutability) = isMutable.compareTo(o.isMutable)
}

object Mutability
{
	// ATTRIBUTES   -----------------
	
	/**
	 * Values of this enumeration (first immutable, then mutable)
	 */
	val values = Pair[Mutability](Immutable, Mutable)
	
	
	// OTHER    ---------------------
	
	/**
	 * @param isMutable A boolean indicating mutability
	 * @return Mutability matching that boolean value
	 */
	def forIsMutable(isMutable: Boolean): Mutability = if (isMutable) Mutable else Immutable
	
	
	// VALUES   ---------------------
	
	/**
	 * Signifies that something may be mutated or changed
	 */
	case object Mutable extends Mutability
	{
		override def isMutable: Boolean = true
		override def unary_- : Mutability = Immutable
	}
	
	/**
	 * Signifies that something cannot or must not be changed
	 */
	case object Immutable extends Mutability
	{
		override def isMutable: Boolean = false
		override def unary_- : Mutability = Mutable
	}
}