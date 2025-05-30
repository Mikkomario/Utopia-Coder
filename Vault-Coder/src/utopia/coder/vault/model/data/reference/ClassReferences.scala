package utopia.coder.vault.model.data.reference

import utopia.coder.model.scala.datatype.Reference
import utopia.coder.vault.model.data.Class
import utopia.flow.view.template.Extender

/**
  * Contains references concerning a single class
  * @author Mikko Hilpinen
  * @since 14.10.2021, v1.2
  */
case class ClassReferences(targetClass: Class, model: ClassModelReferences, dbFactory: Reference, dbModel: Reference,
                           genericUniqueAccessTrait: Option[Reference], genericManyAccessTrait: Option[Reference],
                           targeting: Option[TargetingReferences], generic: Option[GenericClassReferences])
	extends Extender[ClassModelReferences]
{
	override def wrapped: ClassModelReferences = model
}
