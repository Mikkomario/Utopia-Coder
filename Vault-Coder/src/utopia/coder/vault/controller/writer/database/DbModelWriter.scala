package utopia.coder.vault.controller.writer.database

import utopia.coder.model.data.{Name, Named, NamingRules}
import utopia.coder.model.enumeration.NamingConvention.CamelCase
import utopia.coder.model.scala.Visibility.{Private, Protected, Public}
import utopia.coder.model.scala.datatype.Reference._
import utopia.coder.model.scala.datatype.TypeVariance.Covariance
import utopia.coder.model.scala.datatype.{Extension, GenericType, Reference, ScalaType}
import utopia.coder.model.scala.declaration.PropertyDeclarationType.{ComputedProperty, ImmutableValue, LazyValue}
import utopia.coder.model.scala.declaration._
import utopia.coder.model.scala.{DeclarationDate, Package, Parameter, Parameters}
import utopia.coder.vault.model.data.{Class, ClassModelReferences, DbProperty, GenericDbModelRefs, Property, VaultProjectSetup}
import utopia.coder.vault.util.VaultReferences.Vault._
import utopia.coder.vault.util.VaultReferences._
import utopia.flow.collection.immutable.{Empty, Pair, Single}
import utopia.flow.util.StringExtensions._

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
	
	private val copyPrefix = Name("copy", "copy", CamelCase.lower)
	private val withPrefix = Name("with", "with", CamelCase.lower)
	
	
	// OTHER    -----------------------------------------
	
	/**
	  * Generates the DB model class and the associated companion object
	  * @param classToWrite The base class
	  * @param modelRefs    References to various model-related classes and traits
	  * @param tablesRef Reference to the Tables object utilized in this project
	 * @param dbPropsData References to 1) XDbProps trait and 2) XDbPropsWrapper trait,
	 *                    plus name of the abstract XDbProps property,
	 *                    in case these are used (i.e. in case writing an abstract trait).
	 *                    Default = None = Writing a concrete (standard) class.
	 * @param codec        Implicit codec used when writing the file
	  * @param setup        Target project -specific setup (implicit)
	  * @return Success or failure containing either:
	 *              Right) Reference to the generated XModel class, or
	 *              Left) References to the various traits generated for generic classes
	  */
	def apply(classToWrite: Class, modelRefs: ClassModelReferences,
	          tablesRef: Reference, dbPropsData: Option[(Pair[Reference], String)])
	         (implicit codec: Codec, setup: VaultProjectSetup, naming: NamingRules) =
	{
		val storablePackage = setup.dbModelPackage / classToWrite.packageName
		
		// The implementation differs greatly between abstract traits and concrete classes
		dbPropsData match {
			// Case: Writing an abstract trait => Writes multiple traits
			case Some((Pair(dbPropsRef, dbPropsWrapperRef), dbPropsName)) =>
				// Writes XModelLike
				writeModelLike(classToWrite, storablePackage, modelRefs, dbPropsRef)
					.flatMap { case (modelLikeRef, buildCopy) =>
						// Writes XModel
						writeModelTrait(classToWrite, storablePackage, modelLikeRef, dbPropsRef)
							.flatMap { dbModelTraitRef =>
								// Writes XModelFactoryLike
								writeModelFactoryLike(classToWrite, storablePackage, modelRefs.factory)
									.flatMap { factoryLikeRef =>
										// Writes XModelFactory
										writeModelFactory(classToWrite, storablePackage, modelRefs, tablesRef,
											dbPropsRef, dbPropsWrapperRef, factoryLikeRef, dbModelTraitRef,
											dbPropsName, buildCopy)
											.map { factoryRef =>
												Left(GenericDbModelRefs(modelLikeRef, factoryLikeRef, dbModelTraitRef,
													factoryRef))
											}
									}
							}
					}
			// Case: Writing a concrete class
			//       => Writes the XModel case class and its companion object, which functions as a XModelFactory
			case None =>
				val className = (classToWrite.name + modelSuffix).className
				val classType = ScalaType.basic(className)
				val (classDeclaration, applyParams) = modelClassDeclarationFor(classToWrite, classType, modelRefs)
				val objectDeclaration = concreteFactoryFor(classToWrite, className, classType, applyParams,
					modelRefs, tablesRef)
				
				File(storablePackage, objectDeclaration, classDeclaration).write().map { Right(_) }
		}
	}
	
	// Writes XModelLike trait used with generic traits
	// Returns a reference + buildCopy function
	private def writeModelLike(classToWrite: Class, storablePackage: Package,
	                           modelRefs: ClassModelReferences, dbPropsRef: Reference)
	                          (implicit codec: Codec, setup: VaultProjectSetup, naming: NamingRules) =
	{
		val repr = GenericType.covariant("Repr")
		val reprType = repr.toScalaType
		
		val hasId = vault.hasId(classToWrite.idType.optional.toScala)
		val factory = modelRefs.factory(reprType)
		val fromIdFactory = vault.fromIdFactory(classToWrite.idType.toScala, reprType)
		
		val dbPropsProp = ComputedProperty.newAbstract("dbProps", dbPropsRef,
			description = "Access to the database properties which are utilized in this model")
		val optionalProps = classToWrite.dbProperties
			.map { prop => ComputedProperty.newAbstract(prop.name.prop, prop.conversion.intermediate.scalaType) }
			.toVector
		val valueProps = valuePropertiesPropertyFor(classToWrite, "dbProps")
		
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
			extensions = Vector(storable, hasId, factory, fromIdFactory, hasIdProperty),
			properties = optionalProps ++ Pair(dbPropsProp, valueProps),
			methods = withMethods.toSet ++ Set(withId, buildCopy),
			description = s"Common trait for database models used for interacting with ${
				classToWrite.name } data in the database",
			author = classToWrite.author,
			since = DeclarationDate.versionedToday
		)
		
		// Includes the buildCopy method in the return value, since it will be referred to down the line
		File(storablePackage, modelLike).write().map { _ -> buildCopy }
	}
	
	// XModel trait, which extends XModelLike
	private def writeModelTrait(classToWrite: Class, storablePackage: Package,
	                            modelLikeRef: Reference, dbPropsRef: Reference)
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
				Pair(
					Parameter("table", table, description = "The primarily targeted table"),
					Parameter("props", dbPropsRef, description = "Targeted database properties")))(
				s"${ (classToWrite.name + modelSuffix + factorySuffix).className }(table, props)"))
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
	
	private def writeModelFactory(classToWrite: Class, storablePackage: Package, modelRefs: ClassModelReferences,
	                              tablesRef: Reference, dbPropsRef: Reference, dbPropsWrapperRef: Reference,
	                              factoryLikeRef: Reference, dbModelRef: Reference, dbPropsName: String,
	                              buildCopy: MethodDeclaration)
	                             (implicit codec: Codec, setup: VaultProjectSetup, naming: NamingRules) =
	{
		// Contains multiple elements:
		//      1) The XModelFactory trait which extends XModelFactoryLike, removing generic parameters
		//      2) The XModelFactory companion object
		//          2.1) Concrete XModel implementation
		//          2.2) Concrete XModelFactory implementation
		val traitName = (classToWrite.name + modelSuffix + factorySuffix).className
		
		val traitDeclaration = TraitDeclaration(
			name = traitName,
			extensions = Single(factoryLikeRef(dbModelRef, modelRefs.stored, modelRefs.data)),
			description = s"Common trait for factories yielding ${ classToWrite.name } database models",
			author = classToWrite.author,
			since = DeclarationDate.versionedToday
		)
		
		val (concreteClass, applyParams) = modelClassDeclarationFor(classToWrite, ScalaType.basic(s"_$traitName"),
			modelRefs, Some(Pair(ScalaType.referenceToType(dbPropsRef), ScalaType.basic(traitName)) -> buildCopy))
		val concreteFactoryName = s"_$traitName"
		val concreteFactory = concreteFactoryFor(classToWrite, concreteFactoryName, dbModelRef, applyParams, modelRefs,
			tablesRef, Some(Pair(dbPropsRef, dbPropsWrapperRef) -> dbPropsName))
		
		val companionObject = ObjectDeclaration(
			name = traitName,
			methods = Set(
				MethodDeclaration("apply", explicitOutputType = Some(ScalaType.basic(traitName)),
					returnDescription = s"A factory for constructing ${ classToWrite.name } database models")(
					Pair(Parameter("table", table), Parameter("dbProps", dbPropsRef)))(
					s"$concreteFactoryName(table, dbProps)")),
			nested = Set(concreteFactory, concreteClass)
		)
		
		File(storablePackage, companionObject, traitDeclaration).write()
	}
	
	// Writes either the XModel object or the concrete _XFactory(...) class.
	// Both of these perform a similar function,
	// although the latter requires parameters and an external model trait/class to do so.
	private def concreteFactoryFor(classToWrite: Class, factoryName: String, modelClassType: ScalaType,
	                               modelApplyParams: Parameters,
	                               modelRefs: ClassModelReferences, tablesRef: Reference,
	                               dbPropsData: Option[(Pair[Reference], String)] = None)
	                              (implicit setup: VaultProjectSetup, naming: NamingRules) =
	{
		// Prepares the object data
		// val modelClassName = (classToWrite.name + modelSuffix).className
		
		val deprecation = DeprecationStyle.of(classToWrite)
		
		val idProp = LazyValue("id", Set(vault.dbProp), isOverridden = true)(
			s"DbPropertyDeclaration(${classToWrite.idDatabasePropName.quoted}, index)")
		
		// When converting from XData, converts each property to the "intermediate" state
		val passApplyDataParams = ("None" +: classToWrite.properties.flatMap { prop =>
			val propAccessCode = s"data.${prop.name.prop}"
			prop.dataType.sqlConversions
				.map { conversion => conversion.midConversion(propAccessCode) }
		}).mkString(", ")
		val applyFromData = MethodDeclaration("apply", isOverridden = true)(Parameter("data", modelRefs.data))(
			s"apply($passApplyDataParams)")
		
		val complete = MethodDeclaration("complete", Set(modelRefs.stored), visibility = Protected, isOverridden = true)(
			Vector(Parameter("id", flow.value), Parameter("data", modelRefs.data)))(
			s"${ modelRefs.stored.target }(id.get${ if (classToWrite.useLongId) "Long" else "Int" }, data)")
		
		val withId = withIdMethod(classToWrite, "apply")
		val withMethods = classToWrite.properties.flatMap { withPropertyMethods(_) }
		
		// Certain properties differ between concrete classes and abstract traits
		val (constructionParams, customExtensions, customProps, customMethods) = dbPropsData match {
			// Case: Extension if a generic trait => Needs to accept the missing properties as parameters
			//                                       and to implement a custom apply function
			case Some((Pair(dbPropsRef, dbPropsWrapperRef), dbPropsName)) =>
				val params = Pair(
					Parameter("table", table, description = "Table targeted by these models"),
					Parameter(dbPropsName, dbPropsRef,
						description = "Properties which specify how the database interactions are performed")
				)
				// Extends the XModelFactory trait (which contains this concrete implementation)
				val factory = ScalaType.basic(factoryName)(modelClassType, modelRefs.stored, modelRefs.data)
				
				// Implements a custom apply function since case class shortcut is not available
				val applyMethod = MethodDeclaration("apply", explicitOutputType = Some(modelClassType),
					returnDescription = s"Constructs a new ${
						classToWrite.name } database model with the specified properties")(modelApplyParams)(
					s"$modelClassType(table, $dbPropsName, ${ modelApplyParams.lists.flatten.map { _.name }.mkString(", ") })")
				
				(params, Pair[Extension](factory, dbPropsWrapperRef), Empty, Single(applyMethod))
				
			// Case: Standard implementation => Defines and implements the concrete properties as well as extensions
			//                                  Also, (kind of) obviously doesn't accept any construction parameters
			case None =>
				val storableFactory = vault.storableFactory(modelClassType, modelRefs.stored, modelRefs.data)
				val factory = modelRefs.factory(modelClassType)
				val fromIdFactory = vault.fromIdFactory(ScalaType.int, modelClassType)
				
				val tableProp = ComputedProperty("table", Set(tablesRef), isOverridden = true)(
					s"${ tablesRef.target }.${ classToWrite.name.prop }")
				val dbPropImplementations = classToWrite.dbProperties
					.map { prop =>
						LazyValue(prop.name.prop,
							description = s"Database property used for interacting with ${ prop.name.pluralDoc }")(
							s"property(${ prop.modelName.quoted })")
					}
					.toVector
				
				(Empty, Vector[Extension](storableFactory, factory, fromIdFactory), tableProp +: dbPropImplementations,
					Empty)
		}
		
		val extensions = customExtensions ++ deprecation.map { _.extensionFor(modelClassType) }
		val properties = (idProp +: customProps) ++ deprecation.view.flatMap { _.properties }
		val methods = withMethods.toSet ++ customMethods + withId + applyFromData + complete ++
			deprecation.view.flatMap { _.methods }
		val description = s"Used for constructing $modelClassType instances and for inserting ${
			classToWrite.name.pluralDoc } to the database"
		
		// For more generic implementations, writes a class
		// For low level / concrete implementations, writes an object instead
		if (constructionParams.isEmpty)
			ObjectDeclaration(
				name = factoryName,
				extensions = extensions,
				properties = properties,
				methods = methods,
				description = description,
				author = classToWrite.author,
				since = DeclarationDate.versionedToday
			)
		else
			ClassDeclaration(
				name = factoryName,
				visibility = Private,
				constructionParams = constructionParams,
				extensions = extensions,
				properties = properties,
				methods = methods,
				description = description,
				author = classToWrite.author,
				since = DeclarationDate.versionedToday
			)
	}
	
	private def modelClassDeclarationFor(classToWrite: Class, reprType: ScalaType, modelRefs: ClassModelReferences,
	                                     dbPropsAndModelTypesAndBuildCopy: Option[(Pair[ScalaType], MethodDeclaration)] = None)
	                                    (implicit setup: VaultProjectSetup, naming: NamingRules) =
	{
		// Prepares class data
		val optionalIdType = classToWrite.idType.optional
		
		val idParam =  Parameter("id", optionalIdType.toScala, optionalIdType.emptyValue,
			description = s"${ classToWrite.name.doc } database id")
		// Accepts a copy of all properties where each appears in the "intermediate "(db property) state
		val classApplyParams = classToWrite.dbProperties.map { prop =>
			val inputType = prop.conversion.intermediate
			val defaultValue = inputType.emptyValue
			Parameter(prop.name.prop, inputType.scalaType, defaultValue)
		}.toVector
		
		val withMethods = classToWrite.properties.flatMap { withPropertyMethods(_, "copy",
			"A new copy of this model with the specified ") }
		val withId = withIdMethod(classToWrite, "copy")
		
		// Some data differs between trait-based and class based implementations
		val (className, visibility, customConstructionParams, customExtensions, customProps, customMethods) =
			dbPropsAndModelTypesAndBuildCopy match {
				// Case: Abstract trait
				case Some((Pair(dbPropsRef, modelRef), buildCopy)) =>
					// The concrete implementation requires 2 additional construction parameters
					val table = Parameter("table", vault.table,
						description = "Table interacted with when using this model")
					val dbPropsParam = Parameter("dbProps", dbPropsRef,
						description = "Configurations of the interacted database properties")
					
					// Needs to implement the buildCopy function
					val buildCopyImplementation = buildCopy.copy(isOverridden = true,
						bodyCode = s"copy(${
							buildCopy.parameters.lists.flatten.map { p => s"${ p.name } = ${ p.name }" } })")
					
					// Only needs to extend the predefined XModel trait
					(s"_$reprType", Private, Pair(table, dbPropsParam), Single[Extension](modelRef),
						Empty, Single(buildCopyImplementation))
					
				// Case: Concrete / simple class
				case None =>
					// Implements 1) Storable, 2) XFactory[Self] and 3) FromIdFactory[Id, Self], 4) HasIdProperty
					val factory = modelRefs.factory(reprType)
					val fromIdFactory = vault.fromIdFactory(classToWrite.idType.toScala, reprType)
					
					// Defines table by referring to the companion object
					val table = ComputedProperty("table", isOverridden = true)(s"$reprType.table")
					// Same thing with the value properties => The property names are acquired using the companion object
					val valueProps = valuePropertiesPropertyFor(classToWrite, reprType.toString)
					
					(reprType.toString, Public, Empty,
						Vector[Extension](storable, factory, fromIdFactory, hasIdProperty),
						Pair(table, valueProps), Empty)
			}
			
		val constructionParams = customConstructionParams ++ (idParam +: classApplyParams)
		val classDeclaration = ClassDeclaration(
			name = className,
			visibility = visibility,
			constructionParams = constructionParams,
			extensions = customExtensions,
			properties = customProps,
			methods = withMethods.toSet + withId ++ customMethods,
			description = s"Used for interacting with ${ classToWrite.name.plural } in the database",
			author = classToWrite.author, since = DeclarationDate.versionedToday, isCaseClass = true)
		
		classDeclaration -> constructionParams
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
	
	private def valuePropertiesPropertyFor(classToWrite: Class, dbPropsName: String)
	                                      (implicit naming: NamingRules) =
	{
		val quotedId = classToWrite.idDatabasePropName.quoted
		if (classToWrite.properties.isEmpty)
			ComputedProperty("valueProperties", Set(flow.valueConversions), isOverridden = true)(
				s"Vector($quotedId -> id)"
			)
		else {
			val propsPart = classToWrite.dbProperties
				.map { prop => prop.toValueCode.withPrefix(s"$dbPropsName.${ prop.name.prop }.name -> ") }
				.reduceLeft { _.append(_, ", ") }
			ComputedProperty("valueProperties", propsPart.references + flow.valueConversions, isOverridden = true)(
				s"Vector($dbPropsName.id.name -> id, $propsPart)"
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
