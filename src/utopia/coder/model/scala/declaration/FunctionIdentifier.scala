package utopia.coder.model.scala.declaration

import utopia.coder.model.scala.datatype.ScalaType
import utopia.flow.collection.immutable.Empty

object FunctionIdentifier
{
	/**
	 * @param name Function name
	 * @return Identifier for a function with the specified name and no parameters
	 */
	def apply(name: String): FunctionIdentifier = apply(name, Empty)
	
	/**
	 * @param name Function name
	 * @param firstParam Type of the first accepted parameter
	 * @param moreParams Types of the other parameters
	 * @return A new function identifier
	 */
	def apply(name: String, firstParam: ScalaType, moreParams: ScalaType*): FunctionIdentifier =
		apply(name, firstParam +: moreParams)
}

/**
 * Functions are typically identified by their name.
 * Different variations of functions with the same name are identified with their top parameter types.
 * @param name Function name
 * @param parameterTypes Types of the accepted parameters. Shouldn't include any generic type parameters.
 * @author Mikko Hilpinen
 * @since 31.07.2024, v1.1.1
 */
case class FunctionIdentifier(name: String, parameterTypes: Seq[ScalaType])
