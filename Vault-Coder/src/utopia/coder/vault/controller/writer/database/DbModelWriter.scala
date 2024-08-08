package utopia.coder.vault.controller.writer.database

import utopia.coder.model.data.{Name, Named, NamingRules}
import utopia.coder.model.enumeration.NamingConvention.CamelCase
import utopia.coder.model.scala.Visibility.{Private, Protected, Public}
import utopia.coder.model.scala.code.CodePiece
import utopia.coder.model.scala.datatype.Reference._
import utopia.coder.model.scala.datatype.TypeVariance.Covariance
import utopia.coder.model.scala.datatype.{Extension, GenericType, Reference, ScalaType}
import utopia.coder.model.scala.declaration.PropertyDeclarationType.{ComputedProperty, ImmutableValue, LazyValue}
import utopia.coder.model.scala.declaration._
import utopia.coder.model.scala.{DeclarationDate, Package, Parameter, Parameters}
import utopia.flow.collection.CollectionExtensions._
import utopia.coder.vault.model.data.{Class, ClassModelReferences, ClassReferences, DbProperty, GenericDbModelRefs, Property, VaultProjectSetup}
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
	private val modelSuffix = Name("DbModel", "DbModels", CamelCase.capitalized)
	private val factorySuffix = Name("Factory", "Factories", CamelCase.capitalized)
	private val likeSuffix = Name("Like", "Like", CamelCase.capitalized)
	private val implementationSuffix = Name("Impl", "Implementations", CamelCase.capitalized)
	
	private val copyPrefix = Name("copy", "copy", CamelCase.lower)
	private val withPrefix = Name("with", "with", CamelCase.lower)
	
	
	// OTHER    -----------------------------------------
	
	/**
	  * Generates the DB model class and the associated companion object
	  * @param classToWrite The base class
	  * @param parentClassReferences References generated for classes which this class inherits
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
	def apply(classToWrite: Class, parentClassReferences: Seq[ClassReferences], modelRefs: ClassModelReferences,
	          tablesRef: Reference, dbPropsData: Option[(Pair[Reference], String)])
	         (implicit codec: Codec, setup: VaultProjectSetup, naming: NamingRules) =
	{
		val storablePackage = setup.dbModelPackage / classToWrite.packageName
		
		// The implementation differs greatly between abstract traits and concrete classes
		dbPropsData match {
			// Case: Writing an abstract trait => Writes multiple traits
			case Some((Pair(dbPropsRef, dbPropsWrapperRef), dbPropsName)) =>
				// Writes XModelLike
				writeModelLike(classToWrite, storablePackage, parentClassReferences, modelRefs, dbPropsRef)
					.flatMap { case (modelLikeRef, buildCopy) =>
						// Writes XModel
						writeModelTrait(classToWrite, storablePackage, parentClassReferences, modelLikeRef, dbPropsRef)
							.flatMap { dbModelTraitRef =>
								// Writes XModelFactoryLike
								writeModelFactoryLike(classToWrite, storablePackage, parentClassReferences,
									modelRefs.factory)
									.flatMap { factoryLikeRef =>
										// Writes XModelFactory
										writeModelFactory(classToWrite, storablePackage, parentClassReferences,
											modelRefs, tablesRef, dbPropsRef, dbPropsWrapperRef, factoryLikeRef,
											dbModelTraitRef, dbPropsName, buildCopy)
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
				val (classDeclaration, applyParams) = modelClassDeclarationFor(classToWrite, classType,
					parentClassReferences, modelRefs)
				val objectDeclaration = concreteFactoryFor(classToWrite, parentClassReferences, className, classType,
					applyParams, modelRefs, tablesRef)
				
				File(storablePackage, objectDeclaration, classDeclaration).write().map { Right(_) }
		}
	}
	
	// Writes XModelLike trait used with generic traits
	// Returns a reference + buildCopy function
	private def writeModelLike(classToWrite: Class, storablePackage: Package,
	                           parentClassReferences: Seq[ClassReferences],
	                           modelRefs: ClassModelReferences, dbPropsRef: Reference)
	                          (implicit codec: Codec, setup: VaultProjectSetup, naming: NamingRules) =
	{
		// Utilizes a generic type +Repr when creating copies of self
		val repr = GenericType.covariant("Repr", description = "Type of this DB model")
		val reprType = repr.toScalaType
		
		// Some extensions are different when inheriting other YModelLike[Repr] traits
		val factory = modelRefs.factory(reprType)
		val customExtensions: Seq[Extension] = parentClassReferences
			.flatMap[Extension] { _.generic.map { _.dbModel.modelLike(reprType) } }
			.notEmpty match
		{
			case Some(parentExtensions) => parentExtensions
			// Case: Highest level trait => Extends Storable, HasId[...] and FromIdFactory[..., Repr]
			case None =>
				val hasId = vault.hasId(classToWrite.idType.optional.toScala)
				val fromIdFactory = vault.fromIdFactory(classToWrite.idType.toScala, reprType)
				Vector(storable, hasId, fromIdFactory)
		}
		
		// Contains a dbProps -property for accessing property names
		val dbPropsProp = ComputedProperty.newAbstract("dbProps", dbPropsRef,
			description = "Access to the database properties which are utilized in this model",
			isOverridden = classToWrite.isExtension)
		// Defines the properties passed to the constructor
		// When extending a parent YModelLike[Repr],
		// skips redefining the same properties and adds rename implementations, where applicable
		val optionalProps = classToWrite.properties.view.filterNot { _.isDirectExtension }.flatMap { _.dbProperties }
			.map { prop =>
				ComputedProperty.newAbstract(prop.name.prop, prop.conversion.intermediate.scalaType)
			}
			.toVector
		val propRenameImplementations = classToWrite.properties.flatMap { prop =>
			prop.rename.map { case (original, implementation) =>
				ComputedProperty(original.prop, isOverridden = true)(implementation.prop)
			}
		}
		val valueProps = valuePropertiesPropertyFor(classToWrite,
			parentClassReferences.flatMap { _.generic.map { _.dbModel.modelLike } }.map { _.targetCode }, "dbProps")
		
		// Defines a method for creating copies of this instance, modifying some properties
		val buildCopyName = (copyPrefix +: classToWrite.name).function
		val buildCopy = MethodDeclaration.newAbstract(buildCopyName, reprType,
			returnDescription = s"Copy of this model with the specified ${ classToWrite.name } properties",
			isProtected = true)(buildCopyParamsFor(classToWrite))
		// When extending another ModelLike, implements their buildCopy
		val parentBuildCopyImplementations = classToWrite.parents.map { parent =>
			val params = buildCopyParamsFor(parent)
			val renames = classToWrite.propertyRenames
			MethodDeclaration((copyPrefix +: parent.name).function, isOverridden = true)(params)(
				s"$buildCopyName(${ params.map { param => s"${ renames(param.name) } = ${ param.name }" } })")
		}
		
		// Generates withX for all class properties, except for those already defined in parents
		val withMethods = classToWrite.properties.view.filterNot { _.isDirectExtension }
			.flatMap { withPropertyMethods(_, buildCopyName, "A new copy of this model with the specified ") }.toSet
		val withRenameImplementations = classToWrite.properties.flatMap { prop =>
			prop.rename.map { case (original, implementation) =>
				val paramName = original.prop
				MethodDeclaration((withPrefix +: original).function, isOverridden = true)(
					Parameter(paramName, prop.dataType.concrete.toScala))(
					s"${ (withPrefix +: implementation).function }($paramName)")
			}
		}
		// Implements withId(...), except when one has already been defined in a parent
		val withId = if (classToWrite.isExtension) None else Some(withIdMethod(classToWrite, buildCopyName))
		
		val modelLike = TraitDeclaration(
			name = (classToWrite.name + modelSuffix + likeSuffix).className,
			genericTypes = Single(repr),
			extensions = customExtensions :+ factory,
			properties = optionalProps ++ Pair(dbPropsProp, valueProps) ++ propRenameImplementations,
			methods = (withMethods + buildCopy) ++ parentBuildCopyImplementations ++
				withRenameImplementations ++ withId,
			description = s"Common trait for database models used for interacting with ${
				classToWrite.name } data in the database",
			author = classToWrite.author,
			since = DeclarationDate.versionedToday
		)
		
		// Includes the buildCopy method in the return value, since it will be referred to down the line
		File(storablePackage, modelLike).write().map { _ -> buildCopy }
	}
	
	// XModel trait, which extends XModelLike
	// Does not declare new features, simply acts as an alternative to XModelLike without the generic Repr type
	private def writeModelTrait(classToWrite: Class, storablePackage: Package,
	                            parentClassReferences: Seq[ClassReferences],
	                            modelLikeRef: Reference, dbPropsRef: Reference)
	                           (implicit codec: Codec, setup: VaultProjectSetup, naming: NamingRules) =
	{
		val className = (classToWrite.name + modelSuffix).className
		val classType = ScalaType.basic(className)
		
		// When inheriting, extends the YModel trait(s)
		val inheritedExtensions = parentClassReferences.flatMap { _.generic.map[Extension] { _.dbModel.model } }
		
		val modelTrait = TraitDeclaration(
			name = className,
			extensions = modelLikeRef(classType) +: inheritedExtensions,
			description = s"Common trait for database interaction models dealing with ${ classToWrite.name.pluralDoc }",
			author = classToWrite.author,
			since = DeclarationDate.versionedToday
		)
		// The companion object contains a factory function
		// for creating concrete DB model factories based on table & property configurations
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
	// Functions as a combination of traits and does not define new functionality
	private def writeModelFactoryLike(classToWrite: Class, storablePackage: Package,
	                                  parentClassReferences: Seq[ClassReferences], factoryRef: Reference)
	                                 (implicit codec: Codec, setup: VaultProjectSetup, naming: NamingRules) =
	{
		// Defines the generic type parameters (DbModel, A (i.e. the Stored instance) & Data)
		val dbModel = GenericType.childOf("DbModel", storable, Covariance,
			description = "Type of database interaction models constructed")
		val stored = GenericType.covariant("A", description = "Type of read instances")
		val data = GenericType.contravariant("Data", description = "Supported data-part type")
		
		val factoryLike = TraitDeclaration(
			name = (classToWrite.name + modelSuffix + factorySuffix + likeSuffix).className,
			genericTypes = Vector(dbModel, stored, data),
			extensions = extensionsForFactory(classToWrite, parentClassReferences,
				factoryRef, dbModel.toScalaType, stored.toScalaType, data.toScalaType),
			description = s"Common trait for factories used for constructing ${ classToWrite.name } database models",
			author = classToWrite.author,
			since = DeclarationDate.versionedToday
		)
		
		File(storablePackage, factoryLike).write()
	}
	
	// Writes the XModelFactory trait + object
	// Used only with generic traits
	// (for concrete implementations, the XModel companion object acts as this kind of factory)
	private def writeModelFactory(classToWrite: Class, storablePackage: Package,
	                              parentClassReferences: Seq[ClassReferences], modelRefs: ClassModelReferences,
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
		val factoryName = classToWrite.name + modelSuffix + factorySuffix
		val traitName = factoryName.className
		
		// When inheriting other classes, extends their YModelFactory traits, also
		val parentExtensions = parentClassReferences.flatMap[Extension] { _.generic.map { _.dbModel.factory } }
		
		// The XModelFactory trait itself doesn't add new functionality.
		// It only removes the generic Repr type used in XModelFactoryLike.
		val traitDeclaration = TraitDeclaration(
			name = traitName,
			extensions = factoryLikeRef(dbModelRef, modelRefs.stored, modelRefs.data) +: parentExtensions,
			description = s"Common trait for factories yielding ${ classToWrite.name } database models",
			author = classToWrite.author,
			since = DeclarationDate.versionedToday
		)
		
		// The companion object contains a concrete XModel implementation
		val (concreteClass, applyParams) = modelClassDeclarationFor(classToWrite, ScalaType.basic(s"_$dbModelRef"),
			parentClassReferences, modelRefs,
			Some(Pair(ScalaType.referenceToType(dbPropsRef), ScalaType.referenceToType(dbModelRef)) -> buildCopy))
		
		// The companion object also contains a concrete XModelFactory implementation
		val concreteFactoryName = (factoryName + implementationSuffix).className
		val concreteFactory = concreteFactoryFor(classToWrite, parentClassReferences, concreteFactoryName,
			dbModelRef, applyParams, modelRefs, tablesRef,
			Some((Pair(dbPropsRef, dbPropsWrapperRef), dbPropsName, ScalaType.basic(traitName))))
		
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
	// dbPropsData contains 3 values:
	//      1. A pair containing 1) reference to XDbProps and to 2) XDbPropsWrapper
	//      2. Db properties prop name
	//      3. Parent trait type (i.e. the XFactory type)
	private def concreteFactoryFor(classToWrite: Class, parentClassReferences: Seq[ClassReferences],
	                               factoryName: String, modelClassType: ScalaType, modelApplyParams: Parameters,
	                               modelRefs: ClassModelReferences, tablesRef: Reference,
	                               dbPropsData: Option[(Pair[Reference], String, ScalaType)] = None)
	                              (implicit setup: VaultProjectSetup, naming: NamingRules) =
	{
		// Prepares the object data
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
		
		// Implements the complete(...) method from DataInserter
		val complete = MethodDeclaration("complete", Set(modelRefs.stored), visibility = Protected, isOverridden = true)(
			Vector(Parameter("id", flow.value), Parameter("data", modelRefs.data)))(
			s"${ modelRefs.stored.target }(id.get${ if (classToWrite.useLongId) "Long" else "Int" }, data)")
		
		// Implements withId(...) & withX(...) methods
		val withId = withIdMethod(classToWrite, "apply")
		val withMethods = classToWrite.properties.flatMap { withPropertyMethods(_) }
		
		// Certain properties differ between concrete classes and abstract traits
		val (constructionParams, customExtensions, customProps, customMethods) = dbPropsData match {
			// Case: Extension if a generic trait => Needs to accept the missing properties as parameters
			//                                       and to implement a custom apply function
			case Some((Pair(dbPropsRef, dbPropsWrapperRef), dbPropsName, parentTrait)) =>
				val params = Pair(
					Parameter("table", table, description = "Table targeted by these models"),
					Parameter(dbPropsName, dbPropsRef,
						description = "Properties which specify how the database interactions are performed")
				)
				// Extends the XModelFactory trait (from the same file)
				val factory = parentTrait(modelClassType, modelRefs.stored, modelRefs.data)
				
				// Implements a custom apply function since case class shortcut is not available
				val applyParams = modelApplyParams.drop(2)
				val applyMethod = MethodDeclaration("apply", explicitOutputType = Some(modelClassType),
					returnDescription = s"Constructs a new ${
						classToWrite.name } database model with the specified properties")(applyParams)(
					s"$modelClassType(table, $dbPropsName, ${ applyParams.map { _.name }.mkString(", ") })")
				
				(params, Pair[Extension](factory, dbPropsWrapperRef), Empty, Single(applyMethod))
				
			// Case: Standard implementation => Defines and implements the concrete DB properties, as well as extensions.
			//                                  Also, obviously doesn't accept any construction parameters
			case None =>
				val factoryExtensions = extensionsForFactory(classToWrite, parentClassReferences, modelRefs.factory,
					modelClassType, modelRefs.stored, modelRefs.data)
				
				val tableProp = ComputedProperty("table", Set(tablesRef), isOverridden = true)(
					s"${ tablesRef.target }.${ classToWrite.name.prop }")
				val dbPropImplementations = classToWrite.dbProperties
					.map { prop =>
						// TODO: Add overridden keyword where applies, if necessary
						LazyValue(prop.name.prop,
							description = s"Database property used for interacting with ${ prop.name.pluralDoc }")(
							s"property(${ prop.modelName.quoted })")
					}
					.toVector
				
				(Empty, factoryExtensions, tableProp +: dbPropImplementations, Empty)
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
				constructionParams = constructionParams,
				extensions = extensions,
				properties = properties,
				methods = methods,
				description = description,
				author = classToWrite.author,
				since = DeclarationDate.versionedToday
			)
	}
	
	// Writes the XModel class
	// For generic traits this is a private class within a factory companion object
	private def modelClassDeclarationFor(classToWrite: Class, reprType: ScalaType,
	                                     parentClassReferences: Seq[ClassReferences], modelRefs: ClassModelReferences,
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
		
		val withMethods = classToWrite.properties.flatMap { prop =>
			withPropertyMethods(prop, "copy", "A new copy of this model with the specified ")
		}
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
							buildCopy.parameters.lists.flatten.map { p => s"${ p.name } = ${ p.name }" }
								.mkString(", ") })")
					
					// Only needs to extend the predefined XModel trait
					(s"_$reprType", Private, Pair(table, dbPropsParam), Single[Extension](modelRef),
						Empty, Single(buildCopyImplementation))
					
				// Case: Concrete / simple class
				case None =>
					val className = reprType.toString
					val classType = ScalaType.basic(className)
					
					// Implements 1) Storable, 2) XFactory[Self] and 3) FromIdFactory[Id, Self], 4) HasIdProperty
					// (and/or parent YModel trait(s))
					val factory = modelRefs.factory(reprType)
					// TODO: It looks like these parent references are not getting applied (same issue as in Stored / ModelWriter)
					val customExtensions = parentClassReferences
						.flatMap[Extension] { refs =>
							refs.generic.view.flatMap { genericRefs =>
								Pair[Extension](genericRefs.dbModel.modelLike(classType), genericRefs.dbModel.model)
							}
						}
						.notEmpty
						.getOrElse {
							Pair[Extension](
								storable,
								vault.fromIdFactory(classToWrite.idType.toScala, reprType)
							)
						}
					
					// Defines table by referring to the companion object
					val table = ComputedProperty("table", isOverridden = true)(s"$reprType.table")
					// Same thing with the value properties
					// => The property names are acquired using the companion object
					val valueProps = valuePropertiesPropertyFor(classToWrite,
						parentClassReferences.map { _.dbModel.targetCode }, reprType.toString)
					
					(className, Public, Empty, customExtensions :+[Extension] factory, Pair(table, valueProps), Empty)
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
	
	private def extensionsForFactory(classToWrite: Class, parentClassReferences: Seq[ClassReferences],
	                                 factoryRef: Reference,
	                                 dbModelType: ScalaType, storedType: ScalaType, dataType: ScalaType): Seq[Extension] =
	{
		// When extending other YModelFactoryLike traits,
		// will not repeat the StorableFactory & FromIdFactory inheritance
		// TODO: Same issue here as above: Parent class references are not getting applied
		val customExtensions = parentClassReferences
			.flatMap[Extension] { _.generic.map { _.dbModel.factoryLike(dbModelType, storedType, dataType) } }
			.notEmpty
			.getOrElse {
				Pair[Extension](
					storableFactory(dbModelType, storedType, dataType),
					fromIdFactory(classToWrite.idType.toScala, dbModelType)
				)
			}
		
		// Also always extends the XFactory for the withX(...) functions
		customExtensions :+ factoryRef(dbModelType)
	}
	
	private def valuePropertiesPropertyFor(classToWrite: Class, parentReferences: Seq[CodePiece], dbPropsName: String)
	                                      (implicit naming: NamingRules) =
	{
		// Utilizes 'valueProperties' defined in parent traits when inheriting
		val parentValuePropsCode = parentReferences.map { _.mapText { parent => s"super[$parent].valueProperties" } }
			.reduceLeftOption { _.append(_, " ++ ") }.getOrElse(CodePiece.empty)
		// The other properties are defined here manually
		val remainingProperties = classToWrite.properties.filterNot { _.isExtension }
		
		def quotedId = s"$dbPropsName.id.name"
		// The implementation is different when inheriting parents
		val implementation = {
			// Case: No more properties left to define
			if (remainingProperties.isEmpty) {
				// Case: Topmost trait => Only defines the id property
				if (parentValuePropsCode.isEmpty)
					CodePiece(s"Single($quotedId -> id)", Set(flow.valueConversions, flow.single))
				// Case: Inheriting another trait => Only refers to the parent(s)
				else
					parentValuePropsCode
			}
			// Case: Properties must be defined manually
			else {
				val propsPart = remainingProperties.view.flatMap { _.dbProperties }
					.map { prop => prop.toValueCode.withPrefix(s"$dbPropsName.${ prop.name.prop }.name -> ") }
					.reduceLeft { _.append(_, ", ") }
				
				// Case: No inheritance applied => Wraps the properties in a collection and includes the id property
				if (parentValuePropsCode.isEmpty)
					CodePiece.collection(remainingProperties.size + 1) +
						(s"$quotedId -> id, " +: propsPart).withinParenthesis
				// Case: Inheritance applied => Appends these properties to those defined in parents
				else
					parentValuePropsCode
						.append(CodePiece.collection(remainingProperties.size) + propsPart.withinParenthesis, " ++ ")
			}
		}
		
		ComputedProperty("valueProperties", implementation.references, isOverridden = true)(implementation.text)
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
				Single(withDbPropertyMethod(dbProp, concreteProp.description, calledMethodName = calledMethodName,
					returnDescriptionStart = returnDescriptionStart, isOverridden = true))
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
					s" (sets all ${dbProps.size} values)", calledMethodName, returnDescriptionStart,
					isOverridden = true)
		}
	}
	
	private def withDbPropertyMethod(property: DbProperty, paramDescription: String = "",
	                                 returnDescriptionAppendix: String = "", calledMethodName: String = "apply",
	                                 returnDescriptionStart: String = "A model containing only the specified ",
	                                 isOverridden: Boolean = false)
	                                (implicit naming: NamingRules) =
		withMethod(property, Vector(property), property.conversion.origin, paramDescription, returnDescriptionAppendix,
			calledMethodName, returnDescriptionStart, isOverridden)
			
	private def withMethod(source: Named, properties: Seq[DbProperty], parameterType: ScalaType,
	                       paramDescription: String = "", returnDescriptionAppendix: String = "",
	                       calledMethodName: String = "apply",
	                       returnDescriptionStart: String = "A model containing only the specified ",
	                       isOverridden: Boolean = false)
	                      (implicit naming: NamingRules) =
	{
		val paramName = source.name.prop
		val constructionParamsCode = properties
			.map { prop => s"${prop.name.prop} = " +: prop.conversion.midConversion(paramName) }
			.reduceLeft { _.append(_, ", ") }
		MethodDeclaration(withMethodNameFor(source), constructionParamsCode.references,
			returnDescription = s"$returnDescriptionStart${ source.name.doc }$returnDescriptionAppendix",
			isOverridden = isOverridden, isLowMergePriority = true)(
			Parameter(paramName, parameterType, description = paramDescription))(
			s"$calledMethodName($constructionParamsCode)")
	}
	
	private def buildCopyParamsFor(classToWrite: Class)(implicit naming: NamingRules) =
		classToWrite.dbProperties
			.map { prop =>
				val propType = prop.conversion.intermediate
				Parameter(prop.name.prop, propType.scalaType, propType.emptyValue,
					description = s"${ prop.name } to assign to the new model (default = currently assigned value)")
			}
			.toVector
	
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
