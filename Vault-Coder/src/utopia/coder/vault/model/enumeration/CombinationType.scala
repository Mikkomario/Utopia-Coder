package utopia.coder.vault.model.enumeration

import utopia.coder.model.data.{Name, NamingRules}
import utopia.coder.model.scala.code.CodePiece
import utopia.coder.model.scala.datatype.{Extension, Reference, ScalaType}
import utopia.coder.model.scala.declaration.MethodDeclaration
import utopia.coder.model.scala.{Parameter, Parameters}
import utopia.coder.vault.model.data.reference.CombinationReferences
import utopia.coder.vault.util.VaultReferences.Vault._

/**
  * Used for determining, how models should be combined with each other
  * @author Mikko Hilpinen
  * @since 14.10.2021, v1.2
  */
sealed trait CombinationType
{
	// ABSTRACT -----------------------------
	
	/**
	  * @return Whether this combination connects multiple children to a single parent
	  */
	def isOneToMany: Boolean
	
	/**
	  * @return Reference to the parent factory trait for this combination type
	  */
	def parentDbFactoryRef: Reference
	/**
	 * @return Reference to the parent DB reader trait used with this combination type
	 */
	def parentDbReaderRef: Reference
	
	/**
	  * @return Whether the implementation should contain isAlwaysLinked: Boolean -property
	  */
	def shouldSpecifyWhetherAlwaysLinked: Boolean
	
	protected def childParamTypeFrom(childRef: Reference): ScalaType
	
	protected def secondApplyParameterFrom(childRef: Reference): Parameter
	
	
	// COMPUTED -----------------------------
	
	/**
	  * @return Whether this combination type only connects up to one child per parent
	  */
	def isOneToOne = !isOneToMany
	
	
	// OTHER    -----------------------------
	
	/**
	  * @param references Combination-related references
	  * @param targeting Whether targeting DB access classes are written
	 * @return Extension for the factory implementation
	  */
	def extensionWith(references: CombinationReferences, targeting: Boolean): Extension = {
		val parent = if (targeting) parentDbReaderRef else parentDbFactoryRef
		parent(references.combined, references.parent, references.child)
	}
	
	/**
	  * @param parentName Name of the parent parameter
	  * @param childName Name of the child parameter
	  * @param parentRef Reference to the parent model
	  * @param childRef References to the child model
	  * @return Parameters that the combined model constructor (and the factory apply method) should take
	  */
	def applyParamsWith(parentName: Name, childName: Name, parentRef: Reference, childRef: Reference)
	                   (implicit naming: NamingRules) =
	{
		val (childPropName, childDocName) = {
			if (isOneToMany)
				childName.props -> childName.pluralDoc
			else
				childName.prop -> childName.doc
		}
		
		Parameters(
			Parameter(parentName.prop, parentRef, description = s"${ parentName.doc } to wrap"),
			Parameter(childPropName, childParamTypeFrom(childRef),
				description = s"$childDocName to attach to this ${ parentName.doc }"))
	}
	
	/**
	  * @param parentName Name of the parent parameter
	  * @param childName Name of the child parameter
	  * @param references Combination-related references
	  * @param targeting Whether writing targeting DB access classes
	 * @return An apply method implementation for the factory implementation
	  */
	def factoryApplyMethodWith(parentName: Name, childName: Name, references: CombinationReferences, targeting: Boolean)
	                          (implicit naming: NamingRules) =
	{
		val methodName = if (targeting) "combine" else "apply"
		val params = applyParamsWith(parentName, childName, references.parent, references.child)
		MethodDeclaration(methodName, Set(references.combined), isOverridden = true)(params)(
			s"${references.combined.target}(${params.head.name}, ${params(1).name})")
	}
}

object CombinationType
{
	// OTHER    -------------------------------
	
	/**
	  * Attempts to interpret a combination type based on a string
	  * @param typeName Type name string
	  * @return A combination type based on that string
	  */
	def interpret(typeName: String) =
		typeName.toLowerCase match {
			case "one" | "link" => Some(Combined)
			case "optional" | "option" => Some(PossiblyCombined)
			case "multi" | "many" => Some(MultiCombined)
			case _ => None
		}
	
	
	// NESTED   -------------------------------
	
	/**
	  * Combines a single parent to a single child, always
	  */
	case object Combined extends CombinationType
	{
		override val isOneToMany = false
		
		override val parentDbFactoryRef = combiningFactory
		override val parentDbReaderRef: Reference = combiningReader
		
		override val shouldSpecifyWhetherAlwaysLinked = false
		
		override protected def childParamTypeFrom(childRef: Reference) = childRef
		override protected def secondApplyParameterFrom(childRef: Reference) = Parameter("child", childRef)
	}
	/**
	  * Combines a single parent to 0-1 children
	  */
	case object PossiblyCombined extends CombinationType
	{
		override val isOneToMany = false
		
		override val parentDbFactoryRef = possiblyCombiningFactory
		override val parentDbReaderRef: Reference = possiblyCombiningReader
		
		override val shouldSpecifyWhetherAlwaysLinked = false
		
		override protected def childParamTypeFrom(childRef: Reference) = ScalaType.option(childRef)
		override protected def secondApplyParameterFrom(childRef: Reference) =
			Parameter("child", ScalaType.option(childRef), default = CodePiece.none)
	}
	/**
	  * Combines a single parent to 0-n children
	  */
	case object MultiCombined extends CombinationType
	{
		override val isOneToMany = true
		
		override val parentDbFactoryRef = multiCombiningFactory
		override val parentDbReaderRef: Reference = multiLinkedReader
		
		override val shouldSpecifyWhetherAlwaysLinked = true
		
		override protected def childParamTypeFrom(childRef: Reference) = ScalaType.seq(childRef)
		override protected def secondApplyParameterFrom(childRef: Reference) =
			Parameter("children", ScalaType.seq(childRef))
	}
}