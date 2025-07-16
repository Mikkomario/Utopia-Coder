package utopia.coder.vault.controller.writer.database

import utopia.coder.model.data.NamingRules
import utopia.coder.model.scala.DeclarationDate
import utopia.coder.model.scala.code.CodePiece
import utopia.coder.model.scala.datatype.{Extension, Reference}
import utopia.coder.model.scala.declaration.PropertyDeclarationType.{ComputedProperty, ImmutableValue}
import utopia.coder.model.scala.declaration.{File, ObjectDeclaration}
import utopia.coder.vault.model.data.reference.CombinationReferences
import utopia.coder.vault.model.data.{CombinationData, VaultProjectSetup}
import utopia.coder.vault.util.VaultReferences.Vault._
import utopia.flow.collection.immutable.{Empty, Pair, Single}

import scala.io.Codec

/**
  * Used for writing a factory for reading combined model data
  * @author Mikko Hilpinen
  * @since 14.10.2021, v1.2
  */
object CombinedFactoryWriter
{
	// OTHER    ---------------------------
	
	def generateReference(combo: CombinationData, targeting: Boolean = false)
	                     (implicit setup: VaultProjectSetup, naming: NamingRules) =
		Reference(packageFor(combo, targeting), factoryNameFor(combo, targeting))
	
	/**
	  * Writes a combined model factory object file
	  * @param data Combination instructions / data
	  * @param references Combination-related references
	  * @param parentFactoryRef Reference to the parent class factory
	  * @param childFactoryRef Reference to the child class factory
	  * @param setup Implicit project setup
	  * @param codec Implicit codec used when writing the file
	  * @return Reference to the factory object. Failure if file writing failed
	  */
	def apply(data: CombinationData, references: CombinationReferences,
	          parentFactoryRef: Reference, childFactoryRef: Reference, targeting: Boolean = false)
	         (implicit setup: VaultProjectSetup, codec: Codec, naming: NamingRules) =
	{
		val (constructorAssignments, extraParents, baseProps) = {
			// Case: Targeting mode => Requires a constructor for the parent class
			if (targeting) {
				val constructorAssignments = Vector[CodePiece](
					parentFactoryRef.targetCode, childFactoryRef.targetCode)
				val alwaysLinkedAssignment = {
					if (data.combinationType.shouldSpecifyWhetherAlwaysLinked)
						Some(CodePiece(s"neverEmptyRight = ${ data.isAlwaysLinked }"))
					else
						None
				}
				
				(constructorAssignments ++ alwaysLinkedAssignment, None, Empty)
			}
			// Case: Writing a DbFactory => Implements factory properties, the linking property and timestamp, where applicable.
			//                              May extend FromTimelineRowFactory.
			else {
				val parentProp = ImmutableValue("parentFactory", Set(parentFactoryRef),
					isOverridden = true)(parentFactoryRef.target)
				val childProp = ImmutableValue("childFactory", Set(childFactoryRef), isOverridden = true)(
					childFactoryRef.target)
				
				// Some factory implementations require the isAlwaysLinked -property
				val linkingProperty = {
					if (data.combinationType.shouldSpecifyWhetherAlwaysLinked)
						Some(ImmutableValue("isAlwaysLinked", isOverridden = true)(data.isAlwaysLinked.toString))
					else
						None
				}
				// If the parent type uses row time indexing, and this is not a one-to-many combination,
				// Utilizes the same indexing
				val creation = {
					if (data.combinationType.isOneToOne && data.parentClass.recordsIndexedCreationTime) {
						val extension: Extension = fromTimelineRowFactory(references.combined)
						val function = ComputedProperty("timestamp", Set(parentFactoryRef), isOverridden = true)(
							"parentFactory.timestamp")
						Some(extension -> function)
					}
					else
						None
				}
				
				(Empty, creation.map { _._1 }, Pair(parentProp, childProp) ++ linkingProperty ++ creation.map { _._2 })
			}
		}
		
		val deprecation = {
			// Deprecation is not supported in targeting mode
			if (targeting)
				None
			else {
				val parentDeprecates = data.parentClass.isDeprecatable
				val childDeprecates = data.childClass.isDeprecatable
				// If either parent or child type supports deprecation, so does this factory
				if (parentDeprecates || childDeprecates) {
					val condition = {
						if (parentDeprecates) {
							if (childDeprecates)
								"parentFactory.nonDeprecatedCondition && childFactory.nonDeprecatedCondition"
							else
								"parentFactory.nonDeprecatedCondition"
						}
						else
							"childFactory.nonDeprecatedCondition"
					}
					Some(Extension(deprecatable) ->
						ComputedProperty("nonDeprecatedCondition", isOverridden = true)(condition))
				}
				else
					None
			}
		}
		
		File(packageFor(data, targeting),
			ObjectDeclaration(
				name = factoryNameFor(data, targeting),
				extensions = Single(data.combinationType.extensionWith(references, targeting)
					.withConstructor(constructorAssignments)) ++
					extraParents ++ deprecation.map { _._1 },
				properties = baseProps ++ deprecation.map { _._2 },
				methods = Set(data.combinationType.factoryApplyMethodWith(data.parentName, data.childName, references,
					targeting)),
				description = s"Used for reading ${data.name.pluralDoc} from the database", author = data.author,
				since = DeclarationDate.versionedToday
			)
		).write()
	}
	
	private def packageFor(data: CombinationData, targeting: Boolean)(implicit setup: VaultProjectSetup) =
		setup.factoryPackageFor(targeting)/data.packageName
	
	private def factoryNameFor(data: CombinationData, targeting: Boolean)(implicit naming: NamingRules) =
		(data.name + (if (targeting) DbFactoryWriter.readerSuffix else DbFactoryWriter.factorySuffix)).className
}
