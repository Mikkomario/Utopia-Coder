package utopia.coder.model.scala.template

import utopia.coder.model.scala.doc.ScalaDoc

/**
  * Common trait for instances that can be converted to scaladoc lines
  * @author Mikko Hilpinen
  * @since 3.9.2021, v0.1
  */
trait Documented
{
	// ABSTRACT --------------------------
	
	/**
	  * @return scaladoc based on this item
	  */
	def scalaDoc: ScalaDoc
}
