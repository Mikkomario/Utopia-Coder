package utopia.coder.reach.util

import utopia.coder.model.scala.Package._

/**
  * Used for accessing packages in the Reach module and other related modules
  * @author Mikko Hilpinen
  * @since 19.5.2023, v1.0
  */
object ReachPackages
{
	// COMPUTED -----------------------
	
	def paradigm = Paradigm
	def firmament = Firmament
	def reach = Reach
	
	
	// NESTED   -----------------------
	
	object Paradigm
	{
		val base = utopia/"paradigm"
		
		lazy val enumerations = base/"model.enumeration"
	}
	
	object Firmament
	{
		val base = utopia/"firmament"
		
		lazy val stackModels = base/"model.stack"
		lazy val context = base/"context"
		lazy val baseContext = context/"base"
		lazy val colorContext = context/"color"
		lazy val textContext = context/"text"
		lazy val windowContext = context/"window"
		lazy val templateDrawers = base/"drawing.template"
	}
	
	object Reach
	{
		val base = utopia/"reach"
		
		lazy val context = base/"context"
		lazy val components = base/"component"
		lazy val template = components/"template"
		lazy val hierarchies = components/"hierarchy"
		lazy val factories = components/"factory"
		lazy val contextualFactories = factories/"contextual"
		lazy val containers = base/"container"
		lazy val wrapperContainers = containers/"wrapper"
		lazy val multiContainers = containers/"multi"
		lazy val focus = base/"focus"
	}
}
