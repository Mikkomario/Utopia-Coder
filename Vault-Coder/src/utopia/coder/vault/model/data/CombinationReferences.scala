package utopia.coder.vault.model.data

import utopia.coder.model.scala.datatype.Reference

/**
  * Contains references used when writing combined models
  * @author Mikko Hilpinen
  * @since 14.10.2021, v1.2
  * @param parent Reference to the parent class
  * @param child Reference to the child class
  * @param combined Reference to the combination of child and parent class
  */
case class CombinationReferences(parent: Reference, child: Reference, combined: Reference)
