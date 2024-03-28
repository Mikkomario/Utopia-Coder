package utopia.coder.vault.controller.writer.model

import utopia.coder.model.data.NamingRules
import utopia.coder.model.scala.Visibility.Protected
import utopia.flow.util.StringExtensions._
import utopia.coder.vault.model.data.{CombinationData, CombinationReferences, VaultProjectSetup}
import utopia.coder.model.scala.{DeclarationDate, Parameter}
import utopia.coder.model.scala.datatype.{Extension, Reference, ScalaType}
import utopia.coder.model.scala.declaration.PropertyDeclarationType.ComputedProperty
import utopia.coder.model.scala.declaration.{ClassDeclaration, File, MethodDeclaration}
import utopia.coder.vault.util.VaultReferences._

import scala.io.Codec

/**
  * Used for writing combining models
  * @author Mikko Hilpinen
  * @since 14.10.2021, v1.2
  */
object CombinedModelWriter
{
	/**
	  * Writes the combination of two model classes
	  * @param data Combination building instructions
	  * @param parentRef Reference to the combination parent part (stored model)
	  * @param parentDataRef Reference to the data model of the combination parent
	  * @param childRef Reference to the combination child part (stored model)
	  * @param setup Implicit project setup
	  * @param codec Implicit codec used when writing the file
	  * @return Combination related references. Failure if file writing failed.
	  */
	def apply(data: CombinationData, parentRef: Reference, parentDataRef: Reference, childRef: Reference,
	          parentFactoryWrapperRef: Reference)
	         (implicit setup: VaultProjectSetup, codec: Codec, naming: NamingRules) =
	{
		val combinedClassName = data.name.className
		val combinedClassType = ScalaType.basic(combinedClassName)
		
		val extender: Extension = Reference.flow.extender(parentDataRef)
		val factory: Extension = parentFactoryWrapperRef(parentRef, combinedClassType)
		// Extends the HasId trait only if Vault references are enabled
		val parents = {
			if (setup.modelCanReferToDB)
				Vector[Extension](extender, vault.hasId, factory)
			else
				Vector(extender, factory)
		}
		
		val parentName = data.parentName
		val constructorParams = data.combinationType.applyParamsWith(parentName, data.childName, parentRef, childRef)
		val parentPropName = constructorParams.head.name
		
		File(setup.combinedModelPackage/data.packageName,
			ClassDeclaration(combinedClassName,
				constructionParams = constructorParams,
				// Provides implicit access to the data model (because that's where most of the properties are)
				extensions = parents,
				properties = Vector(
					// Provides direct access to parent.id
					ComputedProperty("id", description = s"Id of this ${data.parentName} in the database")(
						s"$parentPropName.id"),
					ComputedProperty("wrapped", isOverridden = true)(s"$parentPropName.data"),
					ComputedProperty("wrappedFactory", visibility = Protected, isOverridden = true)(parentPropName)
				),
				methods = Set(
					MethodDeclaration("mapWrapped", visibility = Protected, isOverridden = true)(
						Parameter("f", Reference.flow.mutate(parentRef)))(s"copy($parentPropName = f($parentPropName))")
				),
				description = data.description.notEmpty
					.getOrElse(s"Combines ${data.parentName} with ${data.childName} data"),
				author = data.author, since = DeclarationDate.versionedToday, isCaseClass = true
			)
		).write().map { comboRef => CombinationReferences(parentRef, childRef, comboRef) }
	}
}
