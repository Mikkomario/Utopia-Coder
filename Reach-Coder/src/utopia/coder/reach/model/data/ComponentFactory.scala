package utopia.coder.reach.model.data

import utopia.coder.model.data.{Name, NamingRules}
import utopia.coder.model.scala.Package
import utopia.coder.model.scala.datatype.Reference
import utopia.coder.reach.model.enumeration.{ContainerStyle, ContextType, ReachFactoryTrait}
import utopia.flow.collection.immutable.Empty

/**
  * Used for declaring component factories and related classes
  * @author Mikko Hilpinen
  * @since 19.5.2023, v1.0
  * @constructor Declares a new component factory
  * @param pck Package within which this component / file appears
  * @param componentName Name of the created component. E.g. "Label"
  * @param contextType The type of context utilized by this factory, if applicable
  * @param parentTraits Traits extended by this factory type (including all settings classes)
  * @param containerType The type of container this component (factory) represents.
  *                      None if this component is not a container.
  * @param properties Properties used by all settings and factory classes
  * @param nonContextualProperties Properties that appear only in non-contextual factory variants
  * @param contextualProperties Properties that only appear in contextual factory variants
  * @param author Author of this component (may be empty)
  * @param onlyContextual Whether only the contextual factories are used (default = false)
  */
case class ComponentFactory(pck: Package, componentName: Name, contextType: Option[ContextType] = None,
                            parentTraits: Seq[ReachFactoryTrait] = Empty,
                            containerType: Option[ContainerStyle] = None,
                            properties: Seq[Property] = Empty,
                            nonContextualProperties: Seq[Property] = Empty,
                            contextualProperties: Seq[Property] = Empty,
                            author: String = "", onlyContextual: Boolean = false)
{
	/**
	  * @return Whether this factory creates containers
	  */
	def isContainer = containerType.isDefined
	/**
	  * @return Whether this factory creates regular components
	  */
	def isNotContainer = !isContainer
	
	/**
	  * @param naming Naming rules to apply
	  * @return Reference to this component
	  */
	def reference(implicit naming: NamingRules) = Reference(pck, componentName.className)
	
	/**
	  * @return All properties declared / used by this factory
	  */
	def allProperties = parentTraits.map { _.property } ++ properties
}