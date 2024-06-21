package utopia.coder.vault.controller.writer.database

import utopia.coder.model.data
import utopia.coder.model.data.{Name, NamingRules}
import utopia.coder.model.enumeration.NamingConvention.CamelCase
import utopia.coder.model.scala.{DeclarationDate, Parameter}
import utopia.coder.model.scala.code.CodePiece
import utopia.coder.model.scala.datatype.{Extension, GenericType, Reference, ScalaType}
import utopia.coder.model.scala.declaration.PropertyDeclarationType.ComputedProperty
import utopia.coder.model.scala.declaration.{ClassDeclaration, File, MethodDeclaration, ObjectDeclaration, PropertyDeclaration, TraitDeclaration}
import utopia.coder.vault.model.data.{Class, ClassModelReferences, VaultProjectSetup}
import utopia.coder.vault.util.ClassMethodFactory
import utopia.coder.vault.util.VaultReferences.Vault._
import Reference._
import utopia.coder.model.scala.Visibility.{Private, Protected}
import utopia.flow.collection.immutable.{Pair, Single}
import utopia.flow.util.Mutate

import scala.collection.immutable.VectorBuilder
import scala.io.Codec

/**
  * Used for writing standard model (from DB) factory objects
  * @author Mikko Hilpinen
  * @since 1.9.2021, v0.1
  */
object DbFactoryWriter
{
	// ATTRIBUTES   --------------------
	
	/**
	  * A suffix added to class names in order to make them factory class names
	  */
	val factorySuffix = Name("DbFactory", "DbFactories", CamelCase.capitalized)
	
	private val likeSuffix = Name("Like", "Likes", CamelCase.capitalized)
	
	
	// OTHER    -------------------------
	
	/**
	  * Writes a factory used for processing database object data
	  * @param classToWrite Class data based on which the factory is created
	  * @param modelRefs    References to the model classes & traits
	  * @param dbModelRef   Reference to the database model class
	  * @param codec        Implicit codec to use when writing the document
	  * @param setup        Implicit project-specific setup
	  * @return Reference to the new written factory object.
	 *         Followed by a reference to the generated XDbFactoryLike trait, if applicable.
	 *         Failure if writing failed.
	  */
	def apply(classToWrite: Class, modelRefs: ClassModelReferences, dbModelRef: Reference,
	          dbPropsRef: Option[Reference] = None)
	         (implicit codec: Codec, setup: VaultProjectSetup, naming: NamingRules) =
	{
		val factoryPackage = setup.factoryPackage / classToWrite.packageName
		val factoryName = (classToWrite.name + factorySuffix).className
		
		// For abstract traits, writes an additional XFactoryLike trait
		dbPropsRef match {
			// Case: Abstract trait => Implements XDbFactoryLike, XDbFactory trait
			//                         and XDbFactory companion object containing a concrete implementation
			case Some(dbPropsRef) =>
				// Starts with the XDbFactoryLike trait
				val aGenericType = GenericType.covariant("A")
				val aType = aGenericType.toScalaType
				
				val dbProps = ComputedProperty.newAbstract("dbProps", dbPropsRef,
					description = "Database properties used when parsing column data")
				val customApplyParams = classToWrite.properties.map { prop =>
					Parameter(prop.name.prop, prop.dataType.toScala,
						description = s"${ prop.name } to assign to the new ${ classToWrite.name }")
				}
				val allApplyParams = Pair(
					Parameter("model", flow.anyModel, description = "Model from which additional data may be read"),
					Parameter("id", classToWrite.idType.toScala,
						description = s"Id to assign to the read/parsed ${ classToWrite.name }")
				) ++ customApplyParams
				val applyMethod = MethodDeclaration.newAbstract("apply", aType,
					returnDescription = s"A ${ classToWrite.name } with the specified data", isProtected = true)(
					allApplyParams)
				val fromModelImplementation = fromModelMethodFor(classToWrite) { propAssignments =>
					"apply(valid, " +: idFromValidModel(classToWrite).append(propAssignments, ", ").append(")")
				}
				val factoryDescription = s"Common trait for factories which parse ${
					classToWrite.name } data from database-originated models"
				
				File(factoryPackage, TraitDeclaration(
					name = (classToWrite.name + factorySuffix + likeSuffix).className,
					genericTypes = Single(aGenericType),
					extensions = extensionsFor(classToWrite, aType),
					properties = Single(dbProps),
					methods = Set(applyMethod, fromModelImplementation),
					description = factoryDescription,
					author = classToWrite.author,
					since = DeclarationDate.versionedToday
				)).write().flatMap { factoryLikeRef =>
					// Next writes the XDbFactory trait + companion object
					val factoryType = ScalaType.basic(factoryName)
					
					val traitDeclaration = TraitDeclaration(
						name = factoryName,
						extensions = Single(factoryLikeRef(modelRefs.stored)),
						description = factoryDescription,
						author = classToWrite.author,
						since = DeclarationDate.versionedToday
					)
					
					val concreteImplementationName = s"_$factoryName"
					val constructorParams = Pair(
						Parameter("table", table, description = "Table from which data is read"),
						Parameter("dbProps", dbPropsRef,
							description = "Database properties used when reading column data"))
					val concreteFactoryImplementation = ClassDeclaration(
						visibility = Private,
						name = concreteImplementationName,
						extensions = Single(factoryType),
						constructionParams = constructorParams,
						methods = Set(MethodDeclaration("apply", Set(modelRefs.stored, modelRefs.data),
							visibility = Protected, isOverridden = true)(
							allApplyParams)(
							s"${ modelRefs.stored.target }(id, ${ modelRefs.data.target }(${
								customApplyParams.map { _.name }.mkString(", ") }))"))
					)
					
					val constructConcrete = MethodDeclaration("apply", explicitOutputType = Some(factoryType),
						returnDescription = s"A factory used for parsing ${
							classToWrite.name.pluralDoc } from database model data")(constructorParams)(
						s"$concreteImplementationName(${ constructorParams.map { _.name }.mkString(", ") })")
					
					val companionObject = ObjectDeclaration(
						name = factoryName,
						methods = Set(constructConcrete),
						nested = Set(concreteFactoryImplementation)
					)
					
					File(factoryPackage, companionObject, traitDeclaration).write().map { _ -> Some(factoryLikeRef) }
				}
			
			// Case: Concrete factory => Just implements the XDbFactory object
			case None =>
				File(factoryPackage,
					ObjectDeclaration(factoryName, extensionsFor(classToWrite, modelRefs.stored),
						properties = propertiesFor(classToWrite, dbModelRef),
						methods = methodsFor(classToWrite, modelRefs.stored, modelRefs.data),
						description = s"Used for reading ${ classToWrite.name.doc } data from the DB",
						author = classToWrite.author, since = DeclarationDate.versionedToday
					)
				).write().map { _ -> None }
		}
	}
	
	private def extensionsFor(classToWrite: Class, modelType: ScalaType): Vector[Extension] = {
		val builder = new VectorBuilder[Extension]()
		
		// If no enumerations are included, the inheritance is more specific (=> uses automatic validation)
		if (classToWrite.fromDbModelConversionMayFail)
			builder += fromRowModelFactory(modelType)
		else
			builder += fromValidatedRowModelFactory(modelType)
		
		// For tables which contain a creation time index, additional inheritance is added
		if (classToWrite.recordsIndexedCreationTime)
			builder += fromTimelineRowFactory(modelType)
		
		// If the class supports deprecation, it is reflected in this factory also
		if (classToWrite.isDeprecatable)
			builder += deprecatable
		
		builder.result()
	}
	
	private def propertiesFor(classToWrite: Class, dbModelRef: Reference)
	                         (implicit naming: NamingRules) =
	{
		val builder = new VectorBuilder[PropertyDeclaration]()
		
		// All objects define a model property, which is used in other functions
		builder += ComputedProperty("model", Set(dbModelRef),
			description = "Model that specifies the how data is read")(
			dbModelRef.target)
		
		// All objects define the table property (implemented)
		builder += ComputedProperty("table", isOverridden = true)("model.table")
		// Timestamp-based factories also specify a creation time property name
		if (classToWrite.recordsIndexedCreationTime)
			classToWrite.timestampProperty.foreach { createdProp =>
				builder += ComputedProperty("timestamp", isOverridden = true)(s"model.${createdProp.name.prop}")
			}
		// Non-timestamp-based factories need to specify default ordering
		else
			builder += ComputedProperty("defaultOrdering", explicitOutputType = Some(ScalaType.option(orderBy)),
				isOverridden = true, isLowMergePriority = true)("None")
		// Deprecatable factories specify the deprecation condition (read from the database model)
		if (classToWrite.isDeprecatable) {
			builder += ComputedProperty("nonDeprecatedCondition", Set(dbModelRef), isOverridden = true)(
				"model.nonDeprecatedCondition")
		}
		
		builder.result()
	}
	
	private def methodsFor(classToWrite: Class, modelRef: Reference, dataRef: Reference)
	                      (implicit naming: NamingRules) =
	{
		// Contains the apply / from model -method
		Set(fromModelMethodFor(classToWrite) { assignments =>
			modelRef.targetCode +
				idFromValidModel(classToWrite)
					.append(dataRef.targetCode + assignments.withinParenthesis, ", ")
					.withinParenthesis
		})
	}
	
	private def fromModelMethodFor(classToWrite: Class)(fromAssignments: Mutate[CodePiece])
	                              (implicit naming: NamingRules) =
	{
		if (classToWrite.fromDbModelConversionMayFail)
			ClassMethodFactory
				.classFromModel(classToWrite, "table.validate(model)")(fromAssignments)
		else
			ClassMethodFactory
				.classFromValidatedModel(classToWrite)(fromAssignments)
	}
	
	private def idFromValidModel(classToWrite: Class) =
		classToWrite.idType.fromValueCode("valid(this.model.id.name)")
}
