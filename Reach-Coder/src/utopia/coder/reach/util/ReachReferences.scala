package utopia.coder.reach.util

import utopia.coder.model.scala.datatype.Reference

/**
  * Used for accessing references from Reach and from other related modules
  * @author Mikko Hilpinen
  * @since 19.5.2023, v1.0
  */
object ReachReferences
{
	// COMPUTED -------------------
	
	def paradigm = Paradigm
	def firmament = Firmament
	def reach = Reach
	
	
	// NESTED   -------------------
	
	object Paradigm
	{
		import ReachPackages.Paradigm._
		
		lazy val alignment = Reference(enumerations, "Alignment")
	}
	
	object Firmament
	{
		import ReachPackages.Firmament._
		
		lazy val stackInsets = Reference(stackModels, "StackInsets")
		lazy val baseContextProps = Reference(baseContext, "BaseContextPropsView")
		lazy val staticBaseContext = Reference(baseContext, "StaticBaseContext")
		lazy val variableBaseContext = Reference(baseContext, "VariableBaseContext")
		lazy val colorContextProps = Reference(colorContext, "ColorContextPropsView")
		lazy val staticColorContext = Reference(colorContext, "StaticColorContext")
		lazy val variableColorContext = Reference(colorContext, "VariableColorContext")
		lazy val textContextProps = Reference(textContext, "TextContextPropsView")
		lazy val staticTextContext = Reference(textContext, "StaticTextContext")
		lazy val variableTextContext = Reference(textContext, "VariableTextContext")
		lazy val customDrawer = Reference(templateDrawers, "CustomDrawer")
	}
	
	object Reach
	{
		import ReachPackages.Reach._
		
		lazy val staticContentWindowContext = Reference(context, "StaticReachContentWindowContext")
		lazy val variableContentWindowContext = Reference(context, "VariableReachContentWindowContext")
		
		lazy val component = Reference(template, "ReachComponent")
		lazy val partOfHierarchy = Reference(template, "PartOfComponentHierarchy")
		lazy val componentHierarchy = Reference(hierarchies, "ComponentHierarchy")
		
		lazy val cff = Reference(factories, "ComponentFactoryFactory")
		lazy val ccff = Reference(factories, "FromContextComponentFactoryFactory")
		lazy val gccff = Reference(factories, "FromGenericContextComponentFactoryFactory")
		lazy val fromContextFactory = Reference(factories, "FromContextFactory")
		lazy val fromGenericContextFactory = Reference(factories, "FromGenericContextFactory")
		lazy val framedFactory = Reference(factories, "FramedFactory")
		lazy val focusListenableFactory = Reference(factories, "FocusListenableFactory")
		lazy val fromAlignmentFactory = Reference(factories, "FromAlignmentFactory")
		lazy val customDrawableFactory = Reference(factories, "CustomDrawableFactory")
		
		lazy val contextualFactory = Reference(contextualFactories, "ContextualFactory")
		lazy val baseContextualFactory = Reference(contextualFactories, "BaseContextualFactory")
		lazy val colorContextualFactory = Reference(contextualFactories, "ColorContextualFactory")
		lazy val textContextualFactory = Reference(contextualFactories, "TextContextualFactory")
		lazy val contentWindowContextualFactory = Reference(contextualFactories, "ReachContentWindowContextualFactory")
		
		lazy val wrapperContainerFactory = Reference(wrapperContainers, "WrapperContainerFactory")
		lazy val contextualWrapperContainerFactory = Reference(wrapperContainers, "ContextualWrapperContainerFactory")
		lazy val nonContextualWrapperContainerFactory = Reference(wrapperContainers, "NonContextualWrapperContainerFactory")
		lazy val combiningContainerFactory = Reference(multiContainers, "CombiningContainerFactory")
		lazy val contextualCombiningContainerFactory = Reference(multiContainers, "ContextualCombiningContainerFactory")
		lazy val nonContextualCombiningContainerFactory = Reference(multiContainers, "NonContextualCombiningContainerFactory")
		lazy val viewContainerFactory = Reference(multiContainers, "ViewContainerFactory")
		lazy val contextualViewContainerFactory = Reference(multiContainers, "ContextualViewContainerFactory")
		lazy val nonContextualViewContainerFactory = Reference(multiContainers, "NonContextualViewContainerFactory")
		
		lazy val focusListener = Reference(focus, "FocusListener")
	}
}
