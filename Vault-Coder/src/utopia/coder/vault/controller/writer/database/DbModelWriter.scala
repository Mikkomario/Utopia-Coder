package utopia.coder.vault.controller.writer.database

import utopia.coder.model.data
import utopia.coder.model.data.{Name, Named, NamingRules}
import utopia.coder.model.enumeration.NamingConvention.CamelCase
import utopia.coder.model.scala.Visibility.Protected
import utopia.coder.model.scala.datatype.Reference._
import utopia.coder.model.scala.datatype.TypeVariance.Covariance
import utopia.coder.model.scala.datatype.{Extension, GenericType, Reference, ScalaType}
import utopia.coder.model.scala.declaration.PropertyDeclarationType.{ComputedProperty, ImmutableValue, LazyValue}
import utopia.coder.model.scala.declaration.{ClassDeclaration, File, MethodDeclaration, ObjectDeclaration, PropertyDeclaration, TraitDeclaration}
import utopia.coder.model.scala.{DeclarationDate, Package, Parameter}
import utopia.flow.util.StringExtensions._
import utopia.coder.vault.model.data.{Class, ClassModelReferences, DbProperty, Property, VaultProjectSetup}
import utopia.coder.vault.util.VaultReferences.Vault._
import utopia.coder.vault.util.VaultReferences._
import utopia.flow.collection.immutable.{Empty, Single}

import scala.collection.immutable.VectorBuilder
import scala.io.Codec

/**
  * Used for writing database model scala files
  * @author Mikko Hilpinen
  * @since 1.9.2021, v0.1
  */
object DbModelWriter
{
	// ATTRIBUTES   -------------------------------------
	
	/**
	  * Suffix added to class name in order to make it a database model class name
	  */
	private val modelSuffix = Name("Model", "Models", CamelCase.capitalized)
	private val factorySuffix = Name("Factory", "Factories", CamelCase.capitalized)
	private val likeSuffix = Name("Like", "Like", CamelCase.capitalized)
	
	private val configSuffix = Name("config", "configurations", CamelCase.lower)
	
	private val copyPrefix = Name("copy", "copy", CamelCase.lower)
	private val withPrefix = Name("with", "with", CamelCase.lower)
	
	
	// OTHER    -----------------------------------------
	
	/**
	  * Generates the DB model class and the associated companion object
	  * @param classToWrite The base class
	  * @param modelRefs    References to various model-related classes and traits
	  * @param codec        Implicit codec used when writing the file
	  * @param setup        Target project -specific setup (implicit)
	  * @return Reference to the generated class. Failure if writing failed.
	  */
	def apply(classToWrite: Class, modelRefs: ClassModelReferences,
	          tablesRef: Reference)
	         (implicit codec: Codec, setup: VaultProjectSetup, naming: NamingRules) =
	{
		val parentPackage = setup.dbModelPackage / classToWrite.packageName
		val className = (classToWrite.name + modelSuffix).className
		val classType = ScalaType.basic(className)
		val deprecation = DeprecationStyle.of(classToWrite)
		
		val optionalIdType = classToWrite.idType.optional
		
		// Contains the following properties:
		//      1) An id property
		//      2) Properties for each class property
		//      3) A table property
		//      4) Deprecation property (optional)
		val propertiesBuilder = new VectorBuilder[PropertyDeclaration]()
		propertiesBuilder += LazyValue("id", Set(vault.dbProp),
			description = s"Property that acts as the primary ${ classToWrite.name } row index")(
			s"DbPropertyDeclaration(${classToWrite.idDatabasePropName.quoted}, index)")
		propertiesBuilder ++= classToWrite.dbProperties
			.map { prop =>
				LazyValue(prop.name.prop,
					description = s"Property that contains ${ classToWrite.name.doc } ${ prop.name.doc }")(
					s"property(${ prop.modelName.quoted })")
			}
		propertiesBuilder += ComputedProperty("table", Set(tablesRef), isOverridden = true)(
			s"${ tablesRef.target }.${ classToWrite.name.prop }")
		propertiesBuilder ++= deprecation.iterator.flatMap { _.properties }
		
		// Converts each property to the "intermediate" state
		val applyParametersCode = ("None" +: classToWrite.properties.flatMap { prop =>
			val propAccessCode = s"data.${prop.name.prop}"
			prop.dataType.sqlConversions
				.map { conversion => conversion.midConversion(propAccessCode) }
		}).mkString(", ")
		
		// The generated file contains the model class and the associated companion object
		File(parentPackage,
			ObjectDeclaration(className,
				factoryExtensionsFor(className, modelRefs, deprecation),
				// Contains an access property for each property, as well as a table -property
				properties = propertiesBuilder.result(),
				// Implements .apply(...) and .complete(id, data)
				methods = Set(
					MethodDeclaration("apply", isOverridden = true)(Parameter("data", modelRefs.data))(
						s"apply($applyParametersCode)"),
					MethodDeclaration("complete", Set(modelRefs.stored), visibility = Protected, isOverridden = true)(
						Vector(Parameter("id", flow.value), Parameter("data", modelRefs.data)))(
						s"${ modelRefs.stored.target }(id.get${ if (classToWrite.useLongId) "Long" else "Int" }, data)"),
					withIdMethod(classToWrite, "apply")
					// Also includes withX(...) methods for each property
				) ++ classToWrite.properties.flatMap { withPropertyMethods(_) } ++
					deprecation.iterator.flatMap { _.methods },
				description = s"Used for constructing $className instances and for inserting ${
					classToWrite.name.pluralDoc
				} to the database", author = classToWrite.author, since = DeclarationDate.versionedToday
			),
			ClassDeclaration(className,
				// Accepts a copy of all properties where each appears in the "intermediate "(db property) state
				constructionParams = Parameter("id", optionalIdType.toScala, optionalIdType.emptyValue,
					description = s"${ classToWrite.name.doc } database id") +:
					classToWrite.dbProperties.map { prop =>
						val inputType = prop.conversion.intermediate
						val defaultValue = inputType.emptyValue
						Parameter(prop.name.prop, inputType.scalaType, defaultValue)
					}.toVector,
				// Extends Storable with the factory traits
				extensions = Vector(storable, modelRefs.factory(classType), fromIdFactory(ScalaType.int, classType)),
				// Implements the required properties: factory & valueProperties
				properties = Vector(
					ComputedProperty("table", isOverridden = true)(s"$className.table"),
					valuePropertiesPropertyFor(classToWrite, className)
				),
				// adds withX(...) -methods for convenience
				methods = classToWrite.properties.flatMap { withPropertyMethods(_, "copy",
					"A new copy of this model with the specified ") }.toSet +
					MethodDeclaration("withId", isOverridden = true)(
						Parameter("id", classToWrite.idType.toScala))("copy(id = Some(id))"),
				description = s"Used for interacting with ${ classToWrite.name.plural } in the database",
				author = classToWrite.author, since = DeclarationDate.versionedToday, isCaseClass = true)
		).write()
	}
	
	// Writes XModelLike trait used with generic traits
	private def writeModelLike(classToWrite: Class, storablePackage: Package,
	                           modelRefs: ClassModelReferences, configRef: Reference)
	                          (implicit codec: Codec, setup: VaultProjectSetup, naming: NamingRules) =
	{
		val repr = GenericType.covariant("Repr")
		val reprType = repr.toScalaType
		
		val hasId = vault.hasId(classToWrite.idType.optional.toScala)
		val factory = modelRefs.factory(reprType)
		val fromIdFactory = vault.fromIdFactory(classToWrite.idType.toScala, reprType)
		
		val configName = (classToWrite.name + configSuffix).prop
		val config = ComputedProperty.newAbstract(configName, configRef,
			description = "Configurations used to determine how database interactions are performed")
		val table = ComputedProperty("table", isOverridden = true)(s"$configName.table")
		val optionalProps = classToWrite.dbProperties
			.map { prop => ComputedProperty.newAbstract(prop.name.prop, prop.conversion.intermediate.scalaType) }
			.toVector
		val valueProps = valuePropertiesPropertyFor(classToWrite, configName)
		
		val buildCopyName = (copyPrefix +: classToWrite.name).function
		val buildCopyParams = classToWrite.dbProperties
			.map { prop =>
				val propType = prop.conversion.intermediate
				Parameter(prop.name.prop, propType.scalaType, propType.emptyValue,
					description = s"${ prop.name } to assign to the new model (default = currently assigned value)")
			}
			.toVector
		val buildCopy = MethodDeclaration.newAbstract(buildCopyName, reprType,
			returnDescription = s"Copy of this model with the specified ${ classToWrite.name } properties",
			isProtected = true)(buildCopyParams)
		val withMethods = classToWrite.properties.flatMap { withPropertyMethods(_, buildCopyName,
			"A new copy of this model with the specified ") }
		val withId = withIdMethod(classToWrite, buildCopyName)
		
		val modelLike = TraitDeclaration(
			name = (classToWrite.name + modelSuffix + likeSuffix).className,
			genericTypes = Single(repr),
			extensions = Vector(storable, hasId, factory, fromIdFactory),
			properties = optionalProps ++ Vector(config, table, valueProps),
			methods = withMethods.toSet ++ Set(withId, buildCopy),
			description = s"Common trait for database models used for interacting with ${
				classToWrite.name } data in the database",
			author = classToWrite.author,
			since = DeclarationDate.versionedToday
		)
		
		File(storablePackage, modelLike).write()
	}
	
	// XModel trait, which extends XModelLike
	private def writeModelTrait(classToWrite: Class, storablePackage: Package,
	                            modelLikeRef: Reference, configRef: Reference)
	                           (implicit codec: Codec, setup: VaultProjectSetup, naming: NamingRules) =
	{
		val className = (classToWrite.name + modelSuffix).className
		val classType = ScalaType.basic(className)
		
		val modelTrait = TraitDeclaration(
			name = className,
			extensions = Single(modelLikeRef(classType)),
			description = s"Common trait for database interaction models dealing with ${ classToWrite.name.pluralDoc }",
			author = classToWrite.author,
			since = DeclarationDate.versionedToday
		)
		val companion = ObjectDeclaration(
			name = className,
			methods = Set(MethodDeclaration("factory",
				returnDescription = s"A factory used for constructing ${
					classToWrite.name } models using the specified configuration")(
				Parameter("config", configRef, description = "Configuration used in determining database interactions"))(
				s"${ (classToWrite.name + modelSuffix + factorySuffix).className }(config)"))
		)
		
		File(storablePackage, companion, modelTrait).write()
	}
	
	// Writes XModelFactoryLike used for constructing XModels when generic traits are used
	private def writeModelFactoryLike(classToWrite: Class, storablePackage: Package, factoryRef: Reference)
	                                 (implicit codec: Codec, setup: VaultProjectSetup, naming: NamingRules) =
	{
		val dbModel = GenericType.childOf("DbModel", storable, Covariance)
		val dbModelType = dbModel.toScalaType
		val stored = GenericType.covariant("A")
		val data = GenericType.contravariant("Data")
		
		val factoryLike = TraitDeclaration(
			name = (classToWrite.name + factorySuffix + likeSuffix).className,
			genericTypes = Vector(dbModel, stored, data),
			extensions = Vector(
				storableFactory(dbModelType, stored.toScalaType, data.toScalaType),
				factoryRef(dbModelType),
				fromIdFactory(classToWrite.idType.toScala, dbModelType)
			),
			description = s"Common trait for factories used for constructing ${ classToWrite.name } database models",
			author = classToWrite.author,
			since = DeclarationDate.versionedToday
		)
		
		File(storablePackage, factoryLike).write()
	}
	
	private def companionObjectOrFactoryTraitFor(classToWrite: Class)
	                                            (implicit setup: VaultProjectSetup, naming: NamingRules) =
	{
		
		
		???
	}
	
	// TODO: Utilize above and add support for generic type
	private def classDeclarationFor(classToWrite: Class, className: String, classType: ScalaType,
	                                modelRefs: ClassModelReferences)
	                               (implicit setup: VaultProjectSetup, naming: NamingRules) =
	{
		// Prepares class data
		val optionalIdType = classToWrite.idType.optional
		
		ClassDeclaration(className,
			// Accepts a copy of all properties where each appears in the "intermediate "(db property) state
			constructionParams = Parameter("id", optionalIdType.toScala, optionalIdType.emptyValue,
				description = s"${ classToWrite.name.doc } database id") +:
				classToWrite.dbProperties.map { prop =>
					val inputType = prop.conversion.intermediate
					val defaultValue = inputType.emptyValue
					Parameter(prop.name.prop, inputType.scalaType, defaultValue)
				}.toVector,
			// Extends Storable with the factory traits
			extensions = Vector(storable, modelRefs.factory(classType),
				fromIdFactory(classToWrite.idType.toScala, classType)),
			// Implements the required properties: factory & valueProperties
			properties = Vector(
				ComputedProperty("table", isOverridden = true)(s"$className.table"),
				valuePropertiesPropertyFor(classToWrite, className)
			),
			// adds withX(...) -methods for convenience
			methods = classToWrite.properties.flatMap { withPropertyMethods(_, "copy",
				"A new copy of this model with the specified ") }.toSet + withIdMethod(classToWrite, "copy"),
			description = s"Used for interacting with ${ classToWrite.name.plural } in the database",
			author = classToWrite.author, since = DeclarationDate.versionedToday, isCaseClass = true)
	}
	
	private def factoryExtensionsFor(className: String, modelRefs: ClassModelReferences,
	                                 deprecation: Option[DeprecationStyle]): Vector[Extension] =
	{
		// The class itself doesn't need to be imported (same file)
		val classType = ScalaType.basic(className)
		// All factories extend the StorableFactory trait, the factory trait and FromIdFactory trait
		val baseExtensions = Vector[Extension](
			vault.storableFactory(classType, modelRefs.stored, modelRefs.data),
			modelRefs.factory(classType),
			fromIdFactory(ScalaType.int, classType)
		)
		// They may also extend a deprecation-related trait
		deprecation match {
			case Some(deprecation) => baseExtensions :+ deprecation.extensionFor(classType)
			case None => baseExtensions
		}
	}
	
	private def valuePropertiesPropertyFor(classToWrite: Class, className: String)
	                                      (implicit naming: NamingRules) =
	{
		val quotedId = classToWrite.idDatabasePropName.quoted
		if (classToWrite.properties.isEmpty)
			ComputedProperty("valueProperties", Set(flow.valueConversions), isOverridden = true)(
				s"Vector($quotedId -> id)"
			)
		else {
			val propsPart = classToWrite.dbProperties
				.map { prop => prop.toValueCode.withPrefix(s"$className.${ prop.name.prop }.name -> ") }
				.reduceLeft { _.append(_, ", ") }
			ComputedProperty("valueProperties", propsPart.references + flow.valueConversions, isOverridden = true)(
				s"Vector($className.id.name -> id, $propsPart)"
			)
		}
	}
	
	private def withIdMethod(classToWrite: Class, assignFunctionName: String) =
		MethodDeclaration("withId", isOverridden = true)(
			Parameter("id", classToWrite.idType.toScala))(s"$assignFunctionName(id = Some(id))")
	
	private def withPropertyMethods(property: Property, calledMethodName: String = "apply",
	                                returnDescriptionStart: String = "A model containing only the specified ")
	                               (implicit naming: NamingRules) =
	{
		val concreteProp = property.concrete
		concreteProp.oneOrManyDbVariants match {
			// Case: The property matches a single column => generates one withX -method
			case Left(dbProp) =>
				Vector(withDbPropertyMethod(dbProp, concreteProp.description, calledMethodName = calledMethodName,
					returnDescriptionStart = returnDescriptionStart))
			// Case: The property matches multiple columns => generates partial and full withX method
			// variants
			case Right(dbProps) =>
				val extraDescription = s", which is part of the property ${ concreteProp.name }"
				val partMethods = dbProps.map {
					// NB: The accepted parameter type may be incorrect
					withDbPropertyMethod(_, returnDescriptionAppendix = extraDescription,
						calledMethodName = calledMethodName, returnDescriptionStart = returnDescriptionStart)
				}
				partMethods :+ withMethod(concreteProp, dbProps, concreteProp.dataType.toScala, concreteProp.description,
					s" (sets all ${dbProps.size} values)", calledMethodName, returnDescriptionStart)
		}
	}
	
	private def withDbPropertyMethod(property: DbProperty, paramDescription: String = "",
	                                 returnDescriptionAppendix: String = "", calledMethodName: String = "apply",
	                                 returnDescriptionStart: String = "A model containing only the specified ")
	                                (implicit naming: NamingRules) =
		withMethod(property, Vector(property), property.conversion.origin, paramDescription, returnDescriptionAppendix,
			calledMethodName, returnDescriptionStart)
			
	private def withMethod(source: Named, properties: Seq[DbProperty], parameterType: ScalaType,
	                       paramDescription: String = "", returnDescriptionAppendix: String = "",
	                       calledMethodName: String = "apply",
	                       returnDescriptionStart: String = "A model containing only the specified ")
	                      (implicit naming: NamingRules) =
	{
		val paramName = source.name.prop
		val constructionParamsCode = properties
			.map { prop => s"${prop.name.prop} = " +: prop.conversion.midConversion(paramName) }
			.reduceLeft { _.append(_, ", ") }
		MethodDeclaration(withMethodNameFor(source), constructionParamsCode.references,
			returnDescription = s"$returnDescriptionStart${ source.name.doc }$returnDescriptionAppendix",
			isLowMergePriority = true)(
			Parameter(paramName, parameterType, description = paramDescription))(
			s"$calledMethodName($constructionParamsCode)")
	}
	
	private def withMethodNameFor(prop: Named)(implicit naming: NamingRules): String = withMethodNameFor(prop.name)
	private def withMethodNameFor(name: Name)(implicit naming: NamingRules) = (withPrefix + name).prop
	
	
	// NESTED   --------------------------------------
	
	private sealed trait DeprecationStyle
	{
		def extensionFor(dbModelClass: ScalaType): Extension
		
		def properties(implicit naming: NamingRules): Seq[PropertyDeclaration]
		
		def methods: Set[MethodDeclaration]
	}
	
	private object DeprecationStyle
	{
		def of(c: Class) =
			c.deprecationProperty
				.map { deprecationProp =>
					c.expirationProperty match {
						case Some(expirationProp) =>
							CombinedDeprecation(expirationProp.name, deprecationProp.name)
						case None => NullDeprecates(deprecationProp)
					}
				}
				.orElse { c.expirationProperty.map { prop => Expires(prop) } }
	}
	
	private case class NullDeprecates(prop: Property) extends DeprecationStyle
	{
		// ATTRIBUTES   -----------------------------
		
		private val camelPropName = prop.name.to(CamelCase.lower).singular
		// Whether this deprecation matches the expected default
		val isDefault = camelPropName == "deprecatedAfter"
		
		
		// IMPLEMENTED  -----------------------------
		
		override def extensionFor(dbModelClass: ScalaType) =
			(if (isDefault) deprecatableAfter else nullDeprecatable)(dbModelClass)
		
		override def properties(implicit naming: NamingRules) =
			if (isDefault) Empty else Vector(
				ImmutableValue("deprecationAttName", isOverridden = true)(prop.dbProperties.head.modelName.quoted))
		// withDeprecatedAfter(...) must be provided separately for custom property names
		override def methods = if (isDefault) Set() else Set(
			MethodDeclaration("withDeprecatedAfter", isOverridden = true)(
				Parameter("deprecationTime", instant))(s"with${ camelPropName.capitalize }(deprecationTime)")
		)
	}
	
	private case class Expires(prop: Property) extends DeprecationStyle
	{
		override def extensionFor(dbModelClass: ScalaType) = expiring
		
		override def properties(implicit naming: NamingRules) =
			Vector(ImmutableValue("deprecationAttName", isOverridden = true)(prop.dbProperties.head.modelName.quoted))
		override def methods = Set()
	}
	
	private case class CombinedDeprecation(expirationPropName: Name, deprecationPropName: Name)
		extends DeprecationStyle
	{
		override def extensionFor(dbModelClass: ScalaType) = deprecatable
		
		override def properties(implicit naming: NamingRules) = Vector(
			ComputedProperty("nonDeprecatedCondition", Set(flow.valueConversions, flow.now),
				isOverridden = true)(
				s"${deprecationPropName.prop}.column.isNull && ${expirationPropName.prop}.column > Now")
		)
		override def methods = Set()
	}
}
