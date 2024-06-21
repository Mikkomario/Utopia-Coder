package utopia.coder.vault.controller.writer.model

import utopia.coder.model.data
import utopia.coder.model.data.{Name, NamingRules}
import utopia.coder.model.enumeration.NamingConvention.{CamelCase, UnderScore}
import utopia.coder.model.scala.Visibility.{Private, Protected, Public}
import utopia.coder.model.scala.code.CodePiece
import utopia.coder.model.scala.datatype.Reference.Flow._
import utopia.coder.model.scala.datatype.{Extension, GenericType, Reference, ScalaType, TypeRequirement}
import utopia.coder.model.scala.declaration.PropertyDeclarationType.{ComputedProperty, LazyValue}
import utopia.coder.model.scala.declaration.{ClassDeclaration, File, MethodDeclaration, ObjectDeclaration, PropertyDeclaration, TraitDeclaration}
import utopia.coder.model.scala.{DeclarationDate, Package, Parameter}
import utopia.flow.util.StringExtensions._
import utopia.coder.vault.controller.writer.database.AccessWriter
import utopia.coder.vault.model.data.{Class, ClassModelReferences, DbProperty, GenericClassModelReferences, Property, VaultProjectSetup}
import utopia.coder.vault.util.ClassMethodFactory
import utopia.coder.vault.util.VaultReferences._
import utopia.flow.collection.immutable.{Empty, Pair, Single}

import scala.io.Codec
import scala.util.Success

/**
  * Used for writing model data from class data
  * @author Mikko Hilpinen
  * @since 30.8.2021, v0.1
  */
// TODO: Add supports for trait extensions
object ModelWriter
{
	// ATTRIBUTES   -------------------------
	
	private val dataClassSuffix = data.Name("Data", "Data", CamelCase.capitalized)
	private val factoryTraitSuffix = data.Name("Factory", "Factories", CamelCase.capitalized)
	private val wrapperSuffix = Name("Wrapper", "Wrappers", CamelCase.capitalized)
	private val traitPropsPrefix = Name("Has", "Has", CamelCase.capitalized)
	private val traitPropsSuffix = Name("Props", "Props", CamelCase.capitalized)
	private val storedPrefix = Name("Stored", "Stored", CamelCase.capitalized)
	private val likeSuffix = Name("Like", "Like", CamelCase.capitalized)
	
	private val copyPrefix = Name("copy", "copy", CamelCase.lower)
	
	/**
	  * Prefix to apply to "withX" functions
	  */
	val withPrefix = Name("with", "with", CamelCase.lower)
	
	
	// OTHER    -----------------------------
	
	/**
	  * Writes stored and partial model classes for a class template
	  * @param classToWrite class being written
	  * @param codec        Implicit codec used when writing files (implicit)
	  * @param setup        Target project -specific settings (implicit)
	  * @return References to generated classes
	  */
	def apply(classToWrite: Class)
	         (implicit codec: Codec, setup: VaultProjectSetup, naming: NamingRules) =
	{
		// Writes the factory traits
		val factoryPackage = setup.modelPackage / s"factory.${ classToWrite.packageName }"
		writeFactoryTrait(classToWrite, factoryPackage).flatMap { factoryRef =>
			writeFactoryWrapperTrait(classToWrite, factoryRef, factoryPackage).flatMap { factoryWrapperRef =>
				val dataPackage = setup.modelPackage / s"partial.${ classToWrite.packageName }"
				lazy val storePackage = setup.modelPackage / s"stored.${ classToWrite.packageName }"
				
				// For generic classes / traits, writes some traits
				// Will contain: 1) XDataLike, 2) XLike and 3) name of the buildCopy function used
				val traitWriteResult = {
					if (classToWrite.isGeneric)
						writeHasProps(classToWrite, dataPackage).flatMap { hasPropsRef =>
								writeDataLikeTrait(classToWrite, dataPackage, hasPropsRef, factoryRef)
									.flatMap { case (dataLikeRef, buildCopyName) =>
										writeStoredLike(classToWrite, storePackage, dataLikeRef, factoryWrapperRef)
											.map { storedLikeRef =>
												Some(GenericClassModelReferences(
													hasPropsRef, dataLikeRef, storedLikeRef) -> buildCopyName)
											}
									}
						}
					else
						Success(None)
				}
				
				traitWriteResult.flatMap { traitData =>
					val (genericRefs, buildCopyName) = traitData match {
						case Some((genericRefs, buildCopyName)) => (Some(genericRefs), buildCopyName)
						case None => (None, "")
					}
					// Writes the data class & stored class
					writeDataClass(classToWrite, dataPackage, factoryRef, genericRefs.map { _.dataLike }, buildCopyName)
						.flatMap { dataRef =>
							writeStored(classToWrite, storePackage, factoryWrapperRef, dataRef,
								genericRefs.map { _.storedLike }, buildCopyName)
								.map { storedRef =>
									// Returns references to generated classes
									ClassModelReferences(dataRef, storedRef, factoryRef, factoryWrapperRef, genericRefs)
								}
						}
				}
			}
		}
	}
	
	private def writeFactoryTrait(classToWrite: Class, factoryPackage: Package)
	                             (implicit codec: Codec, setup: VaultProjectSetup, naming: NamingRules) =
	{
		val factoryTraitName = (classToWrite.name + factoryTraitSuffix).className
		val genericType = GenericType.covariant("A")
		
		File(factoryPackage,
			TraitDeclaration(name = factoryTraitName,
				genericTypes = Single(genericType),
				// Contains a withX(x) function for each data property
				methods = classToWrite.properties.map { prop =>
					MethodDeclaration.newAbstract((withPrefix + prop.name).function, genericType.toScalaType,
						returnDescription = s"Copy of this item with the specified ${prop.name}")(
						Parameter(prop.name.prop, prop.dataType.concrete.toScala,
							description = s"New ${prop.name} to assign"))
				}.toSet,
				description = s"Common trait for ${
					classToWrite.name}-related factories which allow construction with individual properties",
				author = classToWrite.author,
				since = DeclarationDate.versionedToday
			)
		).write()
	}
	
	private def writeFactoryWrapperTrait(classToWrite: Class, factoryRef: Reference, factoryPackage: Package)
	                                    (implicit codec: Codec, setup: VaultProjectSetup, naming: NamingRules) =
	{
		val wrapped = GenericType("A", requirement = Some(TypeRequirement.childOf(factoryRef(ScalaType.basic("A")))))
		val repr = GenericType.covariant("Repr")
		
		File(factoryPackage,
			TraitDeclaration(
				name = (classToWrite.name + factoryTraitSuffix + wrapperSuffix).className,
				genericTypes = Pair(wrapped, repr),
				extensions = Single(factoryRef(repr.toScalaType)),
				properties = Vector(
					PropertyDeclaration.newAbstract("wrappedFactory", wrapped.toScalaType,
						description = "The factory wrapped by this instance",
						isProtected = true
					)
				),
				methods = withMethodsFor(classToWrite) { (prop, propName) =>
					s"mapWrapped { _.${ withMethodNameFor(prop) }($propName) }"
				} +
					MethodDeclaration.newAbstract(
						"wrap", repr.toScalaType,
						description = "Mutates this item by wrapping a mutated instance",
						returnDescription = "Copy of this item with the specified wrapped factory",
						isProtected = true)(
						Parameter("factory", wrapped.toScalaType, description = "The new factory instance to wrap")) +
					MethodDeclaration("mapWrapped",
						visibility = Protected,
						description = "Modifies this item by mutating the wrapped factory instance",
						returnDescription = "Copy of this item with a mutated wrapped factory")(
						Parameter("f", mutate(wrapped.toScalaType),
							description = "A function for mutating the wrapped factory instance"))(
						"wrap(f(wrappedFactory))")
				,
				description = s"Common trait for classes that implement ${factoryRef.target} by wrapping a ${
					factoryRef.target } instance",
				author = classToWrite.author,
				since = DeclarationDate.versionedToday
			)
		).write()
	}
	
	// HasXProps for generic traits
	private def writeHasProps(classToWrite: Class, dataPackage: Package)
	                         (implicit codec: Codec, setup: VaultProjectSetup, naming: NamingRules) =
	{
		val traitName = (traitPropsPrefix +: classToWrite.name) + traitPropsSuffix
		
		// Defines the properties as abstract
		val propertyDeclarations = classToWrite.properties.map { prop =>
			ComputedProperty.newAbstract(prop.name.prop, prop.dataType.scalaType, description = prop.description)
		}
		// Also defines an abstract property for additional model properties
		// - Removed because this may cause issues in multiple inheritance
		/*
		val additionalModelProp = ComputedProperty.newAbstract("additionalPropsModel", model,
			description = s"A model which contains toModel implementation outside of the properties defined in this trait ($traitName)",
			isProtected = true)
		 */
		val toModelCode = toModelImplementationFor(classToWrite) // + " ++ additionalPropsModel"
		
		File(dataPackage, TraitDeclaration(
			name = traitName.className,
			extensions = Single(modelConvertible),
			properties = propertyDeclarations :+
				ComputedProperty("toModel", toModelCode.references, isOverridden = true)(toModelCode.text),
			description = s"Common trait for classes which provide access to ${ classToWrite.name } properties",
			author = classToWrite.author,
			since = DeclarationDate.versionedToday
		)).write()
	}
	
	// XDataLike for generic traits
	private def writeDataLikeTrait(classToWrite: Class, dataPackage: Package,
	                               hasPropsRef: Reference, factoryRef: Reference)
	                              (implicit codec: Codec, setup: VaultProjectSetup, naming: NamingRules) =
	{
		val repr = GenericType.covariant("Repr")
		// Introduces a function for creating copies
		val buildCopy = MethodDeclaration.newAbstract((copyPrefix +: classToWrite.name).function, repr.toScalaType,
			description = s"Builds a modified copy of this ${ classToWrite.name }",
			returnDescription = s"A copy of this ${ classToWrite.name } with the specified properties")(
			classToWrite.properties
				.map { prop =>
					val propName = prop.name.prop
					Parameter(propName, prop.dataType.toScala, propName,
						description = s"New ${ prop.name } to assign. Default = current value.")
				})
		// Utilizes that function in the withX functions
		val withMethods = concreteWithMethodsFor(classToWrite, buildCopy.name)
		
		File(dataPackage, TraitDeclaration(
			name = (classToWrite.name + dataClassSuffix + likeSuffix).className,
			genericTypes = Single(repr),
			extensions = Vector(hasPropsRef, factoryRef(repr.toScalaType)),
			methods = withMethods + buildCopy,
			description = s"Common trait for classes which provide read and copy access to ${
				classToWrite.name } properties",
			author = classToWrite.author,
			since = DeclarationDate.versionedToday
		)).write()
			// Includes the buildCopy function name in the return value since it is needed later
			.map { _ -> buildCopy.name }
	}
	
	// Writes the XData model, including its companion object
	private def writeDataClass(classToWrite: Class, dataPackage: Package,
	                           factoryRef: Reference, dataLikeRef: Option[Reference], buildCopyName: String)
	                          (implicit codec: Codec, setup: VaultProjectSetup, naming: NamingRules) =
	{
		// Prepares common data
		val dataClassName = (classToWrite.name + dataClassSuffix).className
		val dataClassType = ScalaType.basic(dataClassName)
		
		// Prepares class data
		// Accepts a copy of each property. Uses default values where possible.
		val constructionParams = classToWrite.properties.map { prop =>
			Parameter(prop.name.prop, prop.dataType.toScala, prop.defaultValue,
				description = prop.description)
		}
		val deprecationProps = deprecationPropertiesFor(classToWrite)
		
		// Some of the data differs based on whether the class is generic or not
		val (extensions, customProps, customMethods) = dataLikeRef match {
			// Case: Abstract class => Extends XDataLike trait
			case Some(dataLikeRef) =>
				val extensions: Seq[Extension] = Single(dataLikeRef(dataClassType))
				(extensions, Empty, Set[MethodDeclaration]())
				
			// Case: Concrete class => Extends XFactory, implements toModel and withX methods
			case None =>
				val extensions: Seq[Extension] = Pair(factoryRef(dataClassType), modelConvertible)
				val toModelCode = toModelImplementationFor(classToWrite)
				val withMethods = concreteWithMethodsFor(classToWrite, "copy")
				(extensions,
					Single(ComputedProperty("toModel", toModelCode.references, isOverridden = true)(toModelCode.text)),
					withMethods)
		}
		
		// Defines either a trait or a class
		val dataClass = {
			if (classToWrite.isGeneric)
				TraitDeclaration(
					name = dataClassName,
					extensions = extensions,
					properties = deprecationProps ++ customProps,
					methods = customMethods,
					description = classToWrite.description,
					author = classToWrite.author,
					since = DeclarationDate.versionedToday
				)
			else
				ClassDeclaration(
					name = dataClassName,
					constructionParams = constructionParams,
					extensions = extensions,
					properties = deprecationProps ++ customProps,
					methods = customMethods,
					description = classToWrite.description,
					author = classToWrite.author,
					since = DeclarationDate.versionedToday,
					isCaseClass = true
				)
		}
		
		// Prepares the companion object
		val fromModelMayFail = classToWrite.fromDbModelConversionMayFail
		val dataFactoryExtension: Extension = {
			if (fromModelMayFail)
				fromModelFactory(dataClassType)
			else
				fromModelFactoryWithSchema(dataClassType)
		}
		
		val modelDeclarationCode = modelDeclaration.targetCode +
			(CodePiece("Vector") +
				classToWrite.properties.map(propertyDeclarationFrom).reduceLeftOption { _.append(_, ", ") }
					.getOrElse(CodePiece.empty).withinParenthesis
				).withinParenthesis
		val schema = LazyValue("schema", modelDeclarationCode.references,
			isOverridden = !fromModelMayFail, isLowMergePriority = true)(modelDeclarationCode.text)
		
		val fromModel = fromModelFor(classToWrite, dataClassName).copy(isLowMergePriority = true)
		
		// Some of the implementation differs between abstract and concrete classes
		val (additionalCompanionMethod, nestedInCompanion) = {
			// Case: Generic trait => Generates a private concrete implementation and an apply function for it
			if (classToWrite.isGeneric) {
				val concreteClassName = s"_$dataClassName"
				val constructConcrete = s"$concreteClassName(${ constructionParams.map { _.name }.mkString(", ") })"
				val nestedClass = ClassDeclaration(
					name = concreteClassName,
					constructionParams = constructionParams,
					extensions = Vector(dataClassType),
					methods = Set(
						MethodDeclaration(buildCopyName, visibility = Protected, isOverridden = true)(
							constructionParams)(constructConcrete)
					),
					visibility = Private,
					description = s"Concrete implementation of the ${ classToWrite.name } data trait",
					author = classToWrite.author,
					isCaseClass = true)
				
				val applyFunction = MethodDeclaration("apply", explicitOutputType = Some(dataClassType),
					description = s"Creates a new ${ classToWrite.name }",
					returnDescription = s"${ classToWrite.name } with the specified properties")(
					constructionParams)(constructConcrete)
				
				Some(applyFunction) -> Some(nestedClass)
			}
			// Case: Concrete class => No additional features required
			else
				None -> None
		}
		
		// Writes the file
		File(dataPackage,
			ObjectDeclaration(dataClassName,
				extensions = Single(dataFactoryExtension),
				properties = Single(schema),
				methods = Set(fromModel) ++ additionalCompanionMethod,
				nested = nestedInCompanion.toSet
			),
			dataClass
		).write()
	}
	
	// Writes XLike trait for generic implementations
	private def writeStoredLike(classToWrite: Class, storedPackage: Package,
	                            dataLikeRef: Reference, factoryWrapperRef: Reference)
	                           (implicit codec: Codec, setup: VaultProjectSetup, naming: NamingRules) =
	{
		val dataType = ScalaType.basic("Data")
		val data = GenericType.childOf("Data", dataLikeRef(dataType))
		val repr = GenericType.covariant("Repr")
		val reprType = repr.toScalaType
		
		// Certain extensions are different if Vault references are enabled
		val (customExtensions, accessor, withId) = {
			if (setup.modelCanReferToDB) {
				val stored = vault.stored(dataType, classToWrite.idType.toScala)
				val fromIdFactory = vault.fromIdFactory(ScalaType.int, reprType)
				val accessor = accessorFor(classToWrite)
				val withId = withIdFor(classToWrite)
				
				(Pair[Extension](stored, fromIdFactory), Some(accessor), Some(withId))
			}
			else {
				val stored = metropolis.stored(dataType)
				(Single[Extension](stored), None, None)
			}
		}
		
		val wrappedFactory = ComputedProperty("wrappedFactory", visibility = Protected, isOverridden = true)("data")
		val propertyAccessors = classToWrite.properties.map { prop =>
			val propName = prop.name.prop
			ComputedProperty(propName, isOverridden = true)(s"data.$propName")
		}
		
		File(storedPackage, TraitDeclaration(
			name = ((storedPrefix +: classToWrite.name) + likeSuffix).className,
			genericTypes = Pair(data, repr),
			extensions = customExtensions ++
				Pair[Extension](factoryWrapperRef(dataType, reprType), dataLikeRef(reprType)),
			properties = (propertyAccessors :+ wrappedFactory) ++ accessor,
			methods = withId.toSet,
			description = s"Common trait for ${ classToWrite.name.pluralDoc } which have been stored in the database",
			author = classToWrite.author,
			since = DeclarationDate.versionedToday
		)).write()
	}
	
	// Writes the stored model version
	// Returns a reference
	private def writeStored(classToWrite: Class, storedPackage: Package, factoryWrapperRef: Reference,
	                        dataClassRef: Reference, storedLikeRef: Option[Reference], buildCopyName: String)
	                       (implicit codec: Codec, setup: VaultProjectSetup, naming: NamingRules) =
	{
		// Prepares common data
		val className = classToWrite.name.className
		val classType = ScalaType.basic(className)
		
		val idType = classToWrite.idType.toScala
		// Accepts id and data -parameters
		val constructionParams = Vector(
			Parameter("id", idType, description = s"id of this ${ classToWrite.name.doc } in the database"),
			Parameter("data", dataClassRef, description = s"Wrapped ${ classToWrite.name.doc } data")
		)
		val wrap = MethodDeclaration("wrap", visibility = Protected, isOverridden = true)(
			Parameter("data", dataClassRef))("copy(data = data)")
		
		// Prepares class data
		val storedClass = {
			// Some of the implementation differs greatly between abstract and concrete implementations
			val (extensions, properties, methods) = storedLikeRef match {
				// Case: Abstract trait => Only extends the "like" trait and the data trait, supplying the Repr type
				case Some(storedLikeRef) =>
					val storedLike = storedLikeRef(dataClassRef, classType)
					
					(Pair[Extension](storedLike, dataClassRef), Empty, Set[MethodDeclaration]())
				
				// Case: Concrete class => Specifies the implementation required for a stored class
				case None =>
					val factoryWrapper = factoryWrapperRef(dataClassRef, classType)
					val wrappedFactory = ComputedProperty("wrappedFactory", visibility = Protected,
						isOverridden = true)("data")
					
					// If Vault references are allowed, provides certain custom features
					val (customExtensions, customProperties, withId) = {
						if (setup.modelCanReferToDB) {
							val fromIdFactory = vault.fromIdFactory(ScalaType.int, classType)
							val (stored, toModel) = {
								if (classToWrite.useLongId) {
									val stored = vault.stored(dataClassRef, idType)
									val toModel = ComputedProperty("toModel", Set(valueConversions, constant),
										isOverridden = true)("Constant(\"id\", id) + data.toModel")
									
									Pair[Extension](stored, modelConvertible) -> Some(toModel)
								}
								else
									Single[Extension](vault.storedModelConvertible(dataClassRef)) -> None
							}
							val accessor = accessorFor(classToWrite)
							val withId = withIdFor(classToWrite)
							
							(stored :+[Extension] fromIdFactory, Single(accessor) ++ toModel, Some(withId))
						}
						else {
							val stored = metropolis.storedModelConvertible(dataClassRef)
							(Single[Extension](stored), Empty, None)
						}
					}
					
					(customExtensions :+[Extension] factoryWrapper, wrappedFactory +: customProperties,
						Set(wrap) ++ withId)
			}
			val description = s"Represents a ${ classToWrite.name.doc } that has already been stored in the database"
			
			// Writes either a class or a trait
			if (classToWrite.isGeneric)
				TraitDeclaration(
					name = className,
					extensions = extensions,
					properties = properties,
					methods = methods,
					description = description,
					author = classToWrite.author,
					since = DeclarationDate.versionedToday
				)
			else
				ClassDeclaration(
					name = className,
					constructionParams = constructionParams,
					extensions = extensions,
					properties = properties,
					methods = methods,
					description = description,
					author = classToWrite.author,
					since = DeclarationDate.versionedToday,
					isCaseClass = true
				)
		}
		
		// Prepares the companion object
		val companionObject = {
			// The type of model parsing used depends on whether Vault references are enabled
			val dataFactory = ComputedProperty("dataFactory", Set(dataClassRef),
				visibility = if (setup.modelCanReferToDB) Public else Protected, isOverridden = true)(
				dataClassRef.target)
			val (fromModelFactory, complete) = {
				if (setup.modelCanReferToDB) {
					val storedFromModelFactory = vault.storedFromModelFactory(dataClassRef, classType)
					val idExtractor = if (classToWrite.useLongId) "Long" else "Int"
					val complete = MethodDeclaration("complete", visibility = Protected, isOverridden = true)(
						Pair(Parameter("model", anyModel), Parameter("data", dataClassRef)))(
						s"model(\"id\").try$idExtractor.map { apply(_, data) }")
					
					storedFromModelFactory -> Some(complete)
				}
				else
					metropolis.storedFromModelFactory(dataClassRef, classType) -> None
			}
			
			val (applyToConcrete, nestedConcrete) = {
				// Case: Generic trait => Provides access to a concrete implementation + from model parsing
				if (classToWrite.isGeneric) {
					val concreteClassName = s"_$className"
					val constructConcrete = s"$concreteClassName(${
						constructionParams.map { _.name }.mkString(", ") })"
					
					val copyParams = classToWrite.properties
						.map { prop => Parameter(prop.name.prop, prop.dataType.toScala) }
					val buildCopy = MethodDeclaration(buildCopyName, visibility = Protected, isOverridden = true)(
						copyParams)(s"copy(data = data.$buildCopyName(${ copyParams.map { _.name }.mkString(", ") }))")
					
					val nestedClass = ClassDeclaration(
						name = concreteClassName,
						constructionParams = constructionParams,
						extensions = Single(classType),
						methods = Set(buildCopy, wrap),
						visibility = Private,
						description = s"Concrete implementation of the ${ classToWrite.name } trait",
						author = classToWrite.author,
						isCaseClass = true)
					
					val applyFunction = MethodDeclaration("apply", explicitOutputType = Some(classType),
						description = s"Creates a new ${ classToWrite.name }",
						returnDescription = s"${ classToWrite.name } with the specified id and wrapped data")(
						constructionParams)(constructConcrete)
					
					Some(applyFunction) -> Some(nestedClass)
				}
				// Case: Concrete class => Only implements from model parsing
				else
					None -> None
			}
			
			ObjectDeclaration(
				name = className,
				extensions = Single(fromModelFactory),
				properties = Single(dataFactory),
				methods = complete.toSet ++ applyToConcrete,
				nested = nestedConcrete.toSet
			)
		}
		
		// Writes the file
		File(storedPackage, companionObject, storedClass).write()
	}
	
	// Deprecation-supporting classes can have custom properties
	private def deprecationPropertiesFor(classToWrite: Class)(implicit naming: NamingRules) =
	{
		classToWrite.deprecationProperty match {
			case Some(prop) =>
				Vector(
					ComputedProperty("isDeprecated",
						description = s"Whether this ${ classToWrite.name.doc } has already been deprecated")(
						s"${ prop.name.prop }.isDefined"),
					ComputedProperty("isValid",
						description = s"Whether this ${ classToWrite.name.doc } is still valid (not deprecated)")(
						"!isDeprecated")
				)
			case None =>
				classToWrite.expirationProperty match {
					case Some(prop) =>
						Vector(
							ComputedProperty("hasExpired", Set(timeExtensions, now),
								description = s"Whether this ${
									classToWrite.name
								} is no longer valid because it has expired")(
								s"${ prop.name.prop } <= Now"),
							ComputedProperty("isValid",
								description = s"Whether this ${ classToWrite.name.doc } is still valid (hasn't expired yet)")(
								"!hasExpired")
						)
					case None => Empty
				}
		}
	}
	
	private def accessorFor(classToWrite: Class)(implicit naming: NamingRules, setup: VaultProjectSetup) = {
		val singleAccessRef = AccessWriter.singleIdReferenceFor(classToWrite)
		ComputedProperty("access", Set(singleAccessRef),
			description = s"An access point to this ${ classToWrite.name.doc } in the database")(
			s"${ singleAccessRef.target }(id)")
	}
	
	// Defines the withX methods in data class context
	private def concreteWithMethodsFor(classToWrite: Class, copyFunctionName: String)(implicit naming: NamingRules) =
		withMethodsFor(classToWrite) { (prop, propName) =>
			prop.dataType.fromConcreteCode(propName)
				.mapText { wrappedValue => s"$copyFunctionName($propName = $wrappedValue)" }
		}
	
	// code accepts a property and parameter name and returns the implementing code
	private def withMethodsFor(classToWrite: Class)(writeCode: (Property, String) => CodePiece)(implicit naming: NamingRules) =
	{
		classToWrite.properties.map { prop =>
			val propName = prop.name.prop
			val code = writeCode(prop, propName)
			MethodDeclaration(withMethodNameFor(prop), code.references, isOverridden = true, isLowMergePriority = true)(
				Parameter(propName, prop.dataType.concrete.toScala))(code.text)
		}.toSet
	}
	
	private def withIdFor(classToWrite: Class) =
		MethodDeclaration("withId", isOverridden = true)(Parameter("id", classToWrite.idType.toScala))("copy(id = id)")
	
	private def fromModelFor(classToWrite: Class, dataClassName: String)(implicit naming: NamingRules) = {
		def _modelFromAssignments(assignments: CodePiece) =
			assignments.withinParenthesis.withPrefix(dataClassName)
		
		if (classToWrite.fromJsonMayFail)
			ClassMethodFactory.classFromModel(classToWrite, "schema.validate(model)",
				isFromJson = true)(_modelFromAssignments)
		else
			ClassMethodFactory.classFromValidatedModel(classToWrite, isFromJson = true)(_modelFromAssignments)
	}
	
	private def toModelImplementationFor(classToWrite: Class)(implicit naming: NamingRules) = {
		// Code pieces for writing properties into a Model
		val propWrites = classToWrite.properties.map { prop =>
			val propNameInModel = prop.jsonPropName.quoted
			prop.toJsonValueCode.withPrefix(s"$propNameInModel -> ")
		}
		if (propWrites.isEmpty)
			CodePiece("Model.empty", Set(model))
		else {
			val collection = propWrites.size match {
				case 1 => CodePiece("Single", Set(single))
				case 2 => CodePiece("Pair", Set(pair))
				case _ => CodePiece("Vector")
			}
			CodePiece("Model", Set(model)) +
				(collection + propWrites.reduceLeft { _.append(_, ", ") }.withinParenthesis).withinParenthesis
		}
	}
	
	// Writes a property declaration for the model schema
	private def propertyDeclarationFrom(prop: Property)(implicit naming: NamingRules): CodePiece = {
		val name = prop.jsonPropName
		// Supports some alternative names
		val altNames = (Set(CamelCase.lower, UnderScore)
			.flatMap { style =>
				(prop.name +: prop.dbProperties.map { p: DbProperty => p.name }).toSet
					.map { name: Name => name.singularIn(style) }
			} - name)
			.toVector.sorted
		// May specify a default value
		val default = prop.customDefaultValue.notEmpty.orElse {
			val dt = prop.dataType
			if (dt.supportsDefaultJsonValues) dt.nonEmptyDefaultValue.notEmpty else None
		}.map { v => prop.dataType.toJsonValueCode(v.text).referringTo(v.references) }
		
		// Writes only the necessary code parts (i.e. omits duplicate default parameters)
		var paramsCode = CodePiece(name.quoted).append(prop.dataType.valueDataType.targetCode, ", ")
		if (altNames.nonEmpty || default.isDefined)
			paramsCode = paramsCode
				.append(s"Vector(${ altNames.map { _.quoted }.mkString(", ") })", ", ")
		default.foreach { default => paramsCode = paramsCode.append(default, ", ") }
		if (prop.dataType.isOptional || !prop.dataType.supportsDefaultJsonValues)
			paramsCode = paramsCode.append("isOptional = true", ", ")
		
		propertyDeclaration.targetCode + paramsCode.withinParenthesis
	}
	
	private def withMethodNameFor(prop: Property)(implicit naming: NamingRules) = (withPrefix + prop.name).function
}
