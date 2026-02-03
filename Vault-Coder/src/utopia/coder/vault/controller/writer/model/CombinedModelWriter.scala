package utopia.coder.vault.controller.writer.model

import utopia.coder.model.data.{Name, NamingRules}
import utopia.coder.model.enumeration.NamingConvention.CamelCase
import utopia.coder.model.scala.Visibility.{Private, Protected}
import utopia.coder.model.scala.datatype.{Extension, GenericType, Reference, ScalaType}
import utopia.coder.model.scala.declaration.PropertyDeclarationType.ComputedProperty
import utopia.coder.model.scala.declaration._
import utopia.coder.model.scala.{DeclarationDate, Parameter, Parameters}
import utopia.coder.vault.model.data
import utopia.coder.vault.model.data.reference.{ClassModelReferences, CombinationReferences}
import utopia.coder.vault.model.data.{CombinationData, VaultProjectSetup}
import utopia.flow.collection.immutable.{Pair, Single}
import utopia.flow.util.StringExtensions._

import scala.io.Codec

/**
  * Used for writing combining models
  * @author Mikko Hilpinen
  * @since 14.10.2021, v1.2
  */
object CombinedModelWriter
{
	// ATTRIBUTES   ----------------------
	
	private val combinedPrefix = Name("Combined", "Combined", CamelCase.capitalized)
	
	
	// OTHER    --------------------------
	
	/**
	 * Writes a common trait for all combined classes.
	 * This trait contains the commonly shared features between these combinations.
	 * @param parent The primary component (class) within the combinations
	 * @param modelRefs References to various files generated for 'parent'
	 * @param setup Implicit project setup
	 * @param codec Implicit codec used when writing the file
	 * @param naming Implicit naming rules applied
	 * @return Combination related references. Failure if file writing failed.
	 */
	@deprecated("With the new implementation style (the stored version being a trait), this method is no longer needed", "v1.13.1")
	def writeGeneralCombinationTrait(parent: data.Class, modelRefs: ClassModelReferences)
	                                (implicit setup: VaultProjectSetup, codec: Codec, naming: NamingRules) =
	{
		val repr = GenericType.covariant("Repr", description = "Type of the implementing class")
		val reprType = repr.toScalaType
		
		File(setup.combinedModelPackage/parent.packageName,
			TraitDeclaration(
				name = (combinedPrefix +: parent.name).className,
				genericTypes = Single(repr),
				extensions = standardExtensions(modelRefs, reprType),
				properties = standardProperties(parent.localName, modelRefs.stored),
				description = s"Common trait for combinations that add additional data to ${ parent.name.pluralDoc }",
				author = parent.author,
				since = DeclarationDate.versionedToday
			)
		).write()
	}
	
	/**
	 * Writes the combination of two model classes
	 * @param data Combination building instructions
	 * @param parentRefs References to various combo parent classes
	 * @param childRef Reference to the combination child part (stored model)
	 * @param setup Implicit project setup
	 * @param codec Implicit codec used when writing the file
	 * @return Combination related references. Failure if file writing failed.
	 */
	def apply(data: CombinationData, parentRefs: ClassModelReferences, childRef: Reference)
	          (implicit setup: VaultProjectSetup, codec: Codec, naming: NamingRules) =
	{
		// The combinations are written in two parts:
		//      1) Trait format, which may be extended
		//      2) Concrete implementation within the companion object
		
		val traitName = data.name.className
		val traitType = ScalaType.basic(traitName)
		
		val parentName = data.parentName
		
		val childParam = data.combinationType.childParamWith(data.childName, childRef)
		val (childDocName, beVerb) = {
			if (data.combinationType.isOneToMany)
				data.childName.pluralDoc -> "are"
			else
				s"the ${ data.childName.doc }" -> "is"
		}
		
		// Generates the trait first
		
		val childProp = ComputedProperty.newAbstract(childParam.name, childParam.dataType,
			description = s"${ childDocName.capitalize } that $beVerb attached to this ${ parentName.doc }")
		
		val comboTrait = TraitDeclaration(
			name = traitName,
			extensions = standardExtensions(parentRefs, traitType),
			properties = standardProperties(parentName, parentRefs.stored) :+ childProp,
			description = data.description
				.nonEmptyOrElse(s"Combines ${data.parentName} with ${data.childName} data"),
			author = data.author,
			since = DeclarationDate.versionedToday
		)
		
		// Next generates the companion object, including the concrete trait implementation
		
		val constructorParams = Parameters(
			Parameter("id", data.parentClass.idType.toScala, description = s"ID of this $parentName in the DB"),
			Parameter("data", parentRefs.data, description = s"The wrapped $parentName data"),
			childParam)
		
		val concreteImplementationName = s"_$traitName"
		val concreteImplementation = ClassDeclaration(
			name = concreteImplementationName,
			visibility = Private,
			constructionParams = constructorParams,
			extensions = Single(traitType),
			methods = Set(MethodDeclaration("wrap", visibility = Protected, isOverridden = true)(
				Parameter("factory", parentRefs.data))("copy(data = factory)")),
			isCaseClass = true
		)
		
		val parentParam = Parameter(parentName.prop, parentRefs.stored, description = s"The ${ parentName.doc } to wrap")
		
		val companionObject = ObjectDeclaration(
			name = traitName,
			methods = Set(
				MethodDeclaration("apply", explicitOutputType = Some(traitType),
					returnDescription = s"Combination of the specified ${ parentName.doc } and ${ data.childName.doc }")(
					Pair(parentParam, childParam))(
					s"apply(${ parentParam.name }.id, ${ parentParam.name }.data, ${ childParam.name })"),
				MethodDeclaration("apply", explicitOutputType = Some(traitType),
					returnDescription = s"A new ${ parentName.doc } with the specified ${ data.childName.doc } included")(
					constructorParams)(
					s"$concreteImplementationName(${ constructorParams.map { _.name }.mkString(", ") })")
			),
			nested = Set(concreteImplementation)
		)
		
		File(setup.combinedModelPackage/data.packageName, companionObject, comboTrait)
			.write().map { comboRef => CombinationReferences(parentRefs.stored, childRef, comboRef) }
	}
	
	// Generates the extensions applied to the highest level combined model -trait
	private def standardExtensions(parentRefs: ClassModelReferences, reprType: ScalaType) = {
		// Extends the stored version, and the factory trait with a custom Repr type
		Pair[Extension](parentRefs.stored, parentRefs.factory(reprType))
	}
	
	// Generates the properties placed to the highest level combined model -trait
	private def standardProperties(parentName: Name, parentType: ScalaType)
	                              (implicit naming: NamingRules, setup: VaultProjectSetup) =
	{
		val parentPropName = parentName.prop
		Vector(
			parentProp(parentPropName, parentType, parentName.doc),
			// Provides direct access to parent.id
			ComputedProperty("id", description = s"ID of this ${ parentName.doc } in the database",
				isOverridden = setup.modelCanReferToDB)(s"$parentPropName.id"),
			ComputedProperty("data", isOverridden = true)(s"$parentPropName.data"),
			ComputedProperty("wrappedFactory", visibility = Protected, isOverridden = true)(parentPropName))
	}
	
	private def parentProp(name: String, dataType: ScalaType, docName: String) =
		ComputedProperty.newAbstract(name, dataType, description = s"Wrapped $docName")
}
