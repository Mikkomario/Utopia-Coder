package utopia.coder.vault.controller.writer.model

import utopia.coder.model.data
import utopia.coder.model.data.{Name, NamingRules}
import utopia.coder.model.enumeration.NamingConvention.{CamelCase, UnderScore}
import utopia.coder.model.scala.Visibility.{Private, Protected}
import utopia.coder.model.scala.code.CodePiece
import utopia.coder.model.scala.datatype.Reference.Flow._
import utopia.coder.model.scala.datatype.TypeVariance.Covariance
import utopia.coder.model.scala.datatype._
import utopia.coder.model.scala.declaration.PropertyDeclarationType.{ComputedProperty, ImmutableValue, LazyValue}
import utopia.coder.model.scala.declaration._
import utopia.coder.model.scala.{DeclarationDate, Package, Parameter, Parameters}
import utopia.flow.collection.CollectionExtensions._
import utopia.coder.vault.controller.writer.database.{AccessWriter, TargetingWriter}
import utopia.coder.vault.model.data.reference.{ClassModelReferences, ClassReferences, GenericClassModelReferences}
import utopia.coder.vault.model.data.{Class, Property, VaultProjectSetup}
import utopia.coder.vault.util.VaultReferences._
import utopia.flow.collection.immutable.{Empty, OptimizedIndexedSeq, Pair, Single}
import utopia.flow.util.StringExtensions._

import scala.io.Codec
import scala.util.Success

/**
  * Used for writing model data from class data
  * @author Mikko Hilpinen
  * @since 30.8.2021, v0.1
  */
object ModelWriter
{
	// ATTRIBUTES   -------------------------
	
	private val factoryPackageName = "factory"
	private val storedPackageName = "stored"
	private val dataPackageName = "partial"
	
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
	  * @param parentClassReferences References to class & trait files generated for this class' parents
	 * @param targeted Whether to utilize targeted database access points
	 *                 (default = false = utilize older access traits)
	 * @param codec        Implicit codec used when writing files (implicit)
	  * @param setup        Target project -specific settings (implicit)
	  * @return References to generated classes
	  */
	def apply(classToWrite: Class, parentClassReferences: Seq[ClassReferences], targeted: Boolean = false)
	         (implicit codec: Codec, setup: VaultProjectSetup, naming: NamingRules) =
	{
		// Writes the factory traits
		val factoryPackage = setup.modelPackage / s"$factoryPackageName.${ classToWrite.packageName }"
		writeFactoryTrait(classToWrite, factoryPackage, parentClassReferences).flatMap { factoryRef =>
			writeFactoryWrapperTrait(classToWrite, factoryRef, factoryPackage, parentClassReferences)
				.flatMap { factoryWrapperRef =>
					val dataPackage = setup.modelPackage / s"$dataPackageName.${ classToWrite.packageName }"
					lazy val storePackage = setup.modelPackage / s"$storedPackageName.${ classToWrite.packageName }"
					
					// For generic classes / traits, writes some traits
					// Will contain: 1) XDataLike, 2) XLike and 3) name of the buildCopy function used
					val traitWriteResult = {
						if (classToWrite.isGeneric)
							writeHasProps(classToWrite, dataPackage, parentClassReferences).flatMap { hasPropsRef =>
									writeDataLikeTrait(classToWrite, dataPackage, parentClassReferences, hasPropsRef,
										factoryRef)
										.flatMap { case (dataLikeRef, buildCopyName) =>
											writeGenericStoredLike(classToWrite, storePackage, parentClassReferences,
												dataLikeRef, factoryWrapperRef, buildCopyName)
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
						writeDataClass(classToWrite, dataPackage, parentClassReferences, factoryRef,
							genericRefs.map { _.dataLike }, buildCopyName)
							.flatMap { dataRef =>
								writeStored(classToWrite, storePackage, parentClassReferences, factoryWrapperRef,
									dataRef, genericRefs.map { _.storedLike }, targeted)
									.map { case (storedLikeRef, storedRef) =>
										// Returns references to generated classes
										ClassModelReferences(dataRef, storedRef, factoryRef, factoryWrapperRef,
											storedLikeRef, genericRefs)
									}
							}
					}
				}
		}
	}
	
	/**
	 * Generates references for a class without generating any files
	 * @param modelPackage Package where all model packages and files would have been written
	 * @param classToWrite Class for which references are written
	 * @param naming Implicit naming convention
	 * @return References to that class's model files
	 */
	def generateReferences(modelPackage: Package, classToWrite: Class)(implicit naming: NamingRules) = {
		val pName = classToWrite.packageName
		val factoryPackage = modelPackage / s"$factoryPackageName.$pName"
		val storedPackage = modelPackage / s"$storedPackageName.$pName"
		val dataPackage = modelPackage / s"$dataPackageName.$pName"
		
		val cName = classToWrite.name
		val dataName = cName + dataClassSuffix
		val storedName = classToWrite.storedPrefix +: cName
		val generic = {
			if (classToWrite.isGeneric) {
				val hasProps = Reference(dataPackage, ((traitPropsPrefix +: cName) + traitPropsSuffix).className)
				val dataLike = Reference(dataPackage, (dataName + likeSuffix).className)
				val storedLike = Reference(storedPackage, ((storedPrefix +: storedName) + likeSuffix).className)
				Some(GenericClassModelReferences(hasProps, dataLike, storedLike))
			}
			else
				None
		}
		
		val factoryName = cName + factoryTraitSuffix
		val factory = Reference(factoryPackage, factoryName.className)
		val factoryWrapper = Reference(factoryPackage, (factoryName + wrapperSuffix).className)
		
		val data = Reference(dataPackage, dataName.className)
		val storedLike = {
			if (classToWrite.hasCombos)
				Some(Reference(storedPackage, (storedName + likeSuffix).className))
			else
				None
		}
		val stored = Reference(storedPackage, storedName.className)
		
		ClassModelReferences(data, stored, factory, factoryWrapper, storedLike, generic)
	}
	
	private def writeFactoryTrait(classToWrite: Class, factoryPackage: Package,
	                              parentClassReferences: Seq[ClassReferences])
	                             (implicit codec: Codec, setup: VaultProjectSetup, naming: NamingRules) =
	{
		val factoryTraitName = (classToWrite.name + factoryTraitSuffix).className
		val genericType = GenericType.covariant("A", description = "Type of constructed instances")
		val aType = genericType.toScalaType
		
		// When extending another trait:
		//      1) Extends the parent class XFactory[A] traits
		//      2) Skips withX functions that have the same name,
		//      3) Adds withX implementations for renamed properties
		val withXImplementations = classToWrite.properties.flatMap { prop =>
			prop.rename.map { case (original, implementation) =>
				val paramName = original.prop
				MethodDeclaration(withMethodNameFor(original), isOverridden = true)(
					Parameter(paramName, prop.dataType.toScala))(s"${ withMethodNameFor(implementation) }($paramName)")
			}
		}
		
		File(factoryPackage,
			TraitDeclaration(
				name = factoryTraitName,
				genericTypes = Single(genericType),
				extensions = parentClassReferences.map { _.factory(aType) },
				// Contains a withX(x) function for each data property
				methods = (classToWrite.properties.view.filterNot { _.isDirectExtension }.map { prop =>
					MethodDeclaration.newAbstract(withMethodNameFor(prop), aType,
						returnDescription = s"Copy of this item with the specified ${prop.name}")(
						Parameter(prop.name.prop, prop.dataType.concrete.toScala,
							description = s"New ${prop.name} to assign"))
				} ++ withXImplementations).toSet,
				description = s"Common trait for ${
					classToWrite.name }-related factories that allow construction with individual properties",
				author = classToWrite.author,
				since = DeclarationDate.versionedToday
			)
		).write()
	}
	
	private def writeFactoryWrapperTrait(classToWrite: Class, factoryRef: Reference, factoryPackage: Package,
	                                     parentClassReferences: Seq[ClassReferences])
	                                    (implicit codec: Codec, setup: VaultProjectSetup, naming: NamingRules) =
	{
		val wrapped = GenericType("Data",
			requirement = Some(TypeRequirement.childOf(factoryRef(ScalaType.basic("Data")))),
			description = "Type of the wrapped data")
		val wrappedType = wrapped.toScalaType
		val repr = GenericType.covariant("Repr", description = "Type of the implementing class")
		val reprType = repr.toScalaType
		
		// When extending other classes:
		//      1) Extends YFactoryWrapper[Data, Repr]
		//      2) Will not inherit CanCopyData[Data, Repr] again
		//      3) Overrides renamed withX functions,
		//         since both parent traits (XFactory and YFactoryWrapper) define those
		//      5) Will not redefine withX properties from directly extended properties
		val baseExtension = if (classToWrite.isExtension) None else Some(vault.canCopyData(wrappedType, reprType))
		File(factoryPackage,
			TraitDeclaration(
				name = (classToWrite.name + factoryTraitSuffix + wrapperSuffix).className,
				genericTypes = Pair(wrapped, repr),
				extensions = OptimizedIndexedSeq.concat(
					Single[Extension](factoryRef(reprType)),
					baseExtension,
					parentClassReferences.map[Extension] { _.factoryWrapper(wrappedType, reprType) }
				),
				methods = withMethodsFor(classToWrite.properties.filterNot { _.isDirectExtension }) {
					(prop, propName) =>
						prop.rename match {
							case Some((parentPropName, _)) => s"${ withMethodNameFor(parentPropName) }($propName)"
							case None => s"mapData { _.${ withMethodNameFor(prop) }($propName) }"
						}
					},
				description = s"Common trait for classes that implement ${
					factoryRef.target } by wrapping another such instance",
				author = classToWrite.author,
				since = DeclarationDate.versionedToday
			)
		).write()
	}
	
	// HasXProps for generic traits
	private def writeHasProps(classToWrite: Class, dataPackage: Package, parentClassReferences: Seq[ClassReferences])
	                         (implicit codec: Codec, setup: VaultProjectSetup, naming: NamingRules) =
	{
		val traitName = (traitPropsPrefix +: classToWrite.name) + traitPropsSuffix
		
		// Extends ModelConvertible, or another HasProps
		val extensions = parentClassReferences.flatMap[Extension] { _.generic.map { _.hasProps } }.notEmpty
			.getOrElse { Single[Extension](modelConvertible) }
		
		// Defines the properties as abstract, but not those already defined in parent classes
		val propertyDeclarations = classToWrite.properties.filterNot { _.isDirectExtension }.map { prop =>
			ComputedProperty.newAbstract(prop.name.prop, prop.dataType.scalaType, description = prop.description)
		}
		// When extending other traits, implements the renames
		val renamedImplementations = classToWrite.properties.flatMap { prop =>
			prop.rename.map { case (original, implementation) =>
				ComputedProperty(original.prop, isOverridden = true)(implementation.prop)
			}
		}
		val toModelCode = toModelImplementationFor(classToWrite,
			parentClassReferences.flatMap { _.generic.map { _.hasProps } })
		val toModel = toModelCode.map { code =>
			ComputedProperty("toModel", code.references, isOverridden = true)(code.text)
		}
		
		File(dataPackage, TraitDeclaration(
			name = traitName.className,
			extensions = extensions,
			properties = propertyDeclarations ++ toModel ++ renamedImplementations,
			description = s"Common trait for classes that provide access to ${ classToWrite.name } properties",
			author = classToWrite.author,
			since = DeclarationDate.versionedToday
		)).write()
	}
	
	/*
		FIXME: When extending another generic trait, the copy function implementation doesn't account for renames
		E.g.
		override def copyTextPlacement(parentId: Int, placedId: Int, orderIndex: Int) =
			copyStatementPlacement(parentId = parentId, placedId = placedId, orderIndex = orderIndex)
			
		-- Should be statementId = placedId
		
		Also, there are unnecessary overrides for with functions when extending another trait
	 */
	
	// XDataLike for generic traits
	private def writeDataLikeTrait(classToWrite: Class, dataPackage: Package,
	                               parentClassReferences: Seq[ClassReferences],
	                               hasPropsRef: Reference, factoryRef: Reference)
	                              (implicit codec: Codec, setup: VaultProjectSetup, naming: NamingRules) =
	{
		// When extending other classes, the Repr type is more restricted
		val parentDataTraitReferences = parentClassReferences.map { _.data }
		val repr = GenericType.covariant("Repr",
			requirement = parentDataTraitReferences.headOption.map { dataRef => TypeRequirement.childOf(dataRef) },
			description = "Implementing data class or data wrapper class")
		val reprType = repr.toScalaType
		
		// Introduces a function for creating copies
		val buildCopy = MethodDeclaration.newAbstract((copyPrefix +: classToWrite.name).function, reprType,
			description = s"Builds a modified copy of this ${ classToWrite.name }",
			returnDescription = s"A copy of this ${ classToWrite.name } with the specified properties")(
			classToWrite.properties
				.map { prop =>
					val propName = prop.name.prop
					Parameter(propName, prop.dataType.toScala, propName,
						description = s"New ${ prop.name } to assign. Default = current value.")
				})
		// Utilizes that function in the withX functions
		val withMethods = concreteWithMethodsFor(classToWrite.properties, buildCopy.name)
		
		// When extending other YDataLike[Repr] traits, implements their buildCopy functions
		val parentBuildCopies = classToWrite.parents.map { parent =>
			val parameters = parent.properties.map { prop => Parameter(prop.name.prop, prop.dataType.toScala) }
			MethodDeclaration((copyPrefix +: parent.name).function, isOverridden = true)(parameters)(
				s"${ buildCopy.name }(${ parameters.map { p => s"${ p.name } = ${ p.name }" }.mkString(", ") })")
		}
		
		File(dataPackage, TraitDeclaration(
			name = (classToWrite.name + dataClassSuffix + likeSuffix).className,
			genericTypes = Single(repr),
			// Extends HasXProps and XFactory[Repr]
			extensions = Pair[Extension](hasPropsRef, factoryRef(reprType)) ++
				// As well as possible parent traits
				parentClassReferences.flatMap[Extension] { _.generic.map { _.dataLike(reprType) } } ++
				parentDataTraitReferences.map(Extension.fromReference),
			methods = withMethods + buildCopy ++ parentBuildCopies,
			description = s"Common trait for classes that provide read and copy access to ${
				classToWrite.name } properties",
			author = classToWrite.author,
			since = DeclarationDate.versionedToday
		)).write()
			// Includes the buildCopy function name in the return value since it is needed later
			.map { _ -> buildCopy.name }
	}
	
	// FIXME: When writing inherited properties, where data type has been specified again, applies a wrong data type (e.g. original = Option[Int], extending = Reference with Option[Int], results in Int type param)
	// Writes the XData model, including its companion object
	private def writeDataClass(classToWrite: Class, dataPackage: Package, parentClassReferences: Seq[ClassReferences],
	                           factoryRef: Reference, dataLikeRef: Option[Reference], buildCopyName: String)
	                          (implicit codec: Codec, setup: VaultProjectSetup, naming: NamingRules) =
	{
		// Prepares common data
		val dataClassName = (classToWrite.name + dataClassSuffix).className
		val dataClassType = ScalaType.basic(dataClassName)
		
		// Prepares class data
		// Accepts a copy of each property. Uses default values where possible.
		val constructionParams = classToWrite.properties.map { prop =>
			Parameter(prop.name.prop, prop.dataType.toScala, prop.defaultValue, description = prop.description)
		}
		val deprecationProps = deprecationPropertiesFor(classToWrite)
		
		// Extends the YData & YDataLike[XData] traits from the parent classes
		val parentDataClassReferences = parentClassReferences.map { _.data }
		val inheritedExtensions: Seq[Extension] = parentDataClassReferences.map(Extension.fromReference) ++
			parentClassReferences.flatMap { _.generic.map[Extension] { _.dataLike(dataClassType) } }
		
		// Defines renamed parent properties
		val renameImplementations = classToWrite.properties.flatMap { prop =>
			prop.rename.map { case (nameInParent, localName) =>
				ComputedProperty(nameInParent.prop, isOverridden = true)(localName.prop)
			}
		}
		
		// Some of the data differs based on whether the class is generic or not
		val (customExtensions, customProps, customMethods) = dataLikeRef match {
			// Case: Abstract class => Extends XDataLike[XData] trait
			case Some(dataLikeRef) =>
				val extensions: Seq[Extension] = Single[Extension](dataLikeRef(dataClassType))
				
				(extensions, None, Set[MethodDeclaration]())
				
			// Case: Concrete class => Extends XFactory[XData] & ModelConvertible, implements toModel and withX methods
			//                         (except when inheriting another trait)
			case None =>
				val factory = factoryRef(dataClassType)
				// ModelConvertible extension is skipped when inheriting other Data traits
				val extensions: Seq[Extension] = {
					if (inheritedExtensions.isEmpty)
						Pair(factory, modelConvertible)
					else
						Single(factory)
				}
				val toModel = toModelImplementationFor(classToWrite, parentDataClassReferences).map { code =>
					ComputedProperty("toModel", code.references, isOverridden = true)(code.text)
				}
				
				// Only implements the withX methods that have not yet been implemented in a generic parent
				val withMethods = concreteWithMethodsFor(classToWrite.properties.filterNot { _.isDirectExtension },
					"copy")
				
				// When inheriting abstract traits, also implements their buildCopy function
				val copyMethods = parentClassReferences.map { parentRefs =>
					val parent = parentRefs.targetClass
					val copyMethodName = (copyPrefix + parent.name).function
					val buildCopyParams = parent.properties.map { prop =>
						Parameter(prop.name.prop, prop.dataType.toScala)
					}
					
					val parentPropNames = parent.properties.view.map { _.name }.toSet
					val assignments = classToWrite.properties.view
						.flatMap { prop =>
							prop.parents.view.map { _.name }.find(parentPropNames.contains).map { parentName =>
								s"${ prop.name.prop } = ${ parentName.prop }"
							}
						}
						.mkString(", ")
					
					MethodDeclaration(copyMethodName, isOverridden = true)(buildCopyParams)(s"copy($assignments)")
				}
				
				(extensions, toModel, withMethods ++ copyMethods)
		}
		
		// Defines either a trait or a class
		val dataClass = InstanceDeclaration.classOrTrait(
			name = dataClassName,
			constructionParams = constructionParams,
			extensions = customExtensions ++ inheritedExtensions,
			properties = renameImplementations ++ deprecationProps ++ customProps,
			methods = customMethods,
			description = classToWrite.description,
			author = classToWrite.author,
			since = DeclarationDate.versionedToday,
			isTrait = classToWrite.isGeneric,
			isCase = true)
		
		// Writes the file
		File(dataPackage,
			writeDataCompanionObject(classToWrite, dataClassName, dataClassType, constructionParams, buildCopyName),
			dataClass
		).write()
	}
	
	private def writeDataCompanionObject(classToWrite: Class, name: String, dataClassType: ScalaType,
	                                     constructionParams: Seq[Parameter], buildCopyName: String)
	                                    (implicit naming: NamingRules) =
	{
		val fromModelMayFail = classToWrite.fromDbModelConversionMayFail
		val dataFactoryExtension: Extension =
			if (fromModelMayFail) fromModelFactory(dataClassType) else fromModelFactoryWithSchema(dataClassType)
		
		val schemaCode = modelDeclaration.targetCode +
			CodePiece
				.collection(classToWrite.properties.size)(
					classToWrite.properties.map(propertyDeclarationFrom).reduceLeftOption { _.append(_, ", ") }
						.getOrElse(CodePiece.empty)
				)
				.withinParenthesis
		val schema = LazyValue("schema", schemaCode.references, isOverridden = !fromModelMayFail,
			isLowMergePriority = classToWrite.isExtension || classToWrite.properties.exists { _.isMultiColumn })(
			schemaCode.text)
		
		val fromModel = {
			val mayFail = classToWrite.fromJsonMayFail
			val code = ParseModelCode(classToWrite, name,
				openParentBlock = if (mayFail) "validate(model).flatMap { model => " else CodePiece.empty,
				yieldTry = mayFail, json = true)
			
			MethodDeclaration.usingCode("apply", code, isOverridden = true)(Parameter("model", hasProperties))
		}
		
		// Some of the implementation differs between abstract and concrete classes
		val (additionalCompanionMethod, nestedInCompanion) = {
			// Case: Generic trait => Generates a private concrete implementation and an apply function for it
			if (classToWrite.isGeneric) {
				val concreteClassName = s"_$name"
				val constructConcrete = s"$concreteClassName(${ constructionParams.map { _.name }.mkString(", ") })"
				val nestedClass = ClassDeclaration(
					name = concreteClassName,
					constructionParams = constructionParams,
					extensions = Vector(dataClassType),
					methods = Set(
						MethodDeclaration(buildCopyName, isOverridden = true)(constructionParams)(constructConcrete)
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
		
		ObjectDeclaration(name,
			extensions = Single(dataFactoryExtension),
			properties = Single(schema),
			methods = Set(fromModel) ++ additionalCompanionMethod,
			nested = nestedInCompanion.toSet
		)
	}
	
	// Writes XLike trait for generic implementations
	// FIXME: Looks like property  & copy functions are not getting written correctly
	private def writeGenericStoredLike(classToWrite: Class, storedPackage: Package,
	                                   parentClassReferences: Seq[ClassReferences], dataLikeRef: Reference,
	                                   factoryWrapperRef: Reference, buildCopyName: String)
	                                  (implicit codec: Codec, setup: VaultProjectSetup, naming: NamingRules) =
	{
		// When extending YStoredLike[Data, Repr], the Repr type must conform to YStoredData
		val parentDataRefs = parentClassReferences.map { _.data }
		
		val dataType = ScalaType.basic("Data")
		val data = GenericType.childOf("Data", dataLikeRef(dataType),
			description = "Type of the wrapped data")
		val repr = GenericType.covariant("Repr",
			requirement = parentDataRefs.headOption.map { TypeRequirement.childOf(_) },
			description = "Implementing type")
		val reprType = repr.toScalaType
		
		val inheritedExtensions = parentClassReferences
			.flatMap[Extension] { _.generic.map { _.storedLike(dataType, reprType) } }
		
		// If extending another YStoredLike trait, expects the parent traits to be defined there already
		val customExtensions = {
			if (classToWrite.isExtension)
				Empty
			else {
				val stored = vault.stored(dataType, classToWrite.idType.toScala)
				val fromIdFactory = vault.fromIdFactory(ScalaType.int, reprType)
				
				Vector[Extension](stored, vault.storedModelConvertible(dataType), fromIdFactory)
			}
		}
		
		// Implements access properties based on the wrapped data, but not for parent class properties
		val propertyAccessors = classToWrite.properties.filterNot { _.isDirectExtension }.map { prop =>
			val propName = prop.name.prop
			ComputedProperty(propName, isOverridden = true)(s"data.$propName")
		}
		
		// Implements the copyX(...) function required by XDataLike
		val buildCopy = {
			val copyParams = classToWrite.properties.map { prop => Parameter(prop.name.prop, prop.dataType.toScala) }
			MethodDeclaration(buildCopyName, isOverridden = true)(copyParams)(
				s"wrap(data.$buildCopyName(${ copyParams.iterator.map { _.name }.mkString(", ") }))")
		}
		
		File(storedPackage, TraitDeclaration(
			name = ((storedPrefix +: classToWrite.name) + likeSuffix).className,
			genericTypes = Pair(data, repr),
			extensions = customExtensions ++
				Pair[Extension](factoryWrapperRef(dataType, reprType), dataLikeRef(reprType)) ++ inheritedExtensions,
			properties = propertyAccessors,
			methods = Set(buildCopy),
			description = s"Common trait for ${ classToWrite.name.pluralDoc } that have been stored in the database",
			author = classToWrite.author,
			since = DeclarationDate.versionedToday
		)).write()
	}
	
	/**
	 * Writes the stored model version: X.
	 *
	 * May also writes StoredXLike trait that's used with combo classes.
	 * These traits are restricted to a specific XData type but specify a flexible Repr type.
	 *
	 * @param classToWrite Class for which these classes/traits are written
	 * @param storedPackage Package to which these files will be written
	 * @param parentClassReferences References to the parent classes
	 * @param factoryWrapperRef Reference to the XFactoryWrapper trait
	 * @param dataClassRef Reference to the XData class or trait
	 * @param genericStoredLikeRef Reference to the StoredXLike trait generated for a generic class, if applicable.
	 * @param targeted Whether using targeting mode when writing access classes (expected to be true at this point)
	 * @param codec Implicit file-encoding
	 * @param setup Implicit project setup
	 * @param naming Implicit naming rules applied
	 * @return References to the written traits/classes:
	 *         1. StoredXLike, if generated
	 *         1. X (= StoredX)
	 */
	private def writeStored(classToWrite: Class, storedPackage: Package, parentClassReferences: Seq[ClassReferences],
	                        factoryWrapperRef: Reference, dataClassRef: Reference,
	                        genericStoredLikeRef: Option[Reference], targeted: Boolean)
	                       (implicit codec: Codec, setup: VaultProjectSetup, naming: NamingRules) =
	{
		// Prepares the data potentially used in multiple or alternative locations
		// The main class/trait name is X or StoredX, depending on the class definition
		val className = (classToWrite.storedPrefix +: classToWrite.name).className
		val classType = ScalaType.basic(className)
		
		val idType = classToWrite.idType.toScala
		// The concrete implementation accepts ID and data -parameters
		val constructionParams = Vector(
			Parameter("id", idType, description = s"ID of this ${ classToWrite.name.doc } in the database"),
			Parameter("data", dataClassRef, description = s"Wrapped ${ classToWrite.name.doc } data")
		)
		
		// When inheriting other classes, extends their YData with StoredYLike[Data, Repr]
		val inheritedExtensions = parentClassReferences.flatMap[Extension] { refs =>
			refs.data +: refs.generic.map[Extension] { _.storedLike(dataClassRef, classType) }.emptyOrSingle
		}
		
		// The withId(...) and wrap(data) functions are only implemented at the lowest level
		val concreteMethodImplementations = {
			val withId = withIdFor(classToWrite)
			val wrap = MethodDeclaration("wrap", isOverridden = true)(Parameter("data", dataClassRef))(
				"copy(data = data)")
			Set(withId, wrap)
		}
		
		// Prepares main class or trait X
		// Also possibly prepares StoredXLike trait, if needed for combo classes
		val stored = {
			// Only XLike versions define a generic +Repr type
			val (repr, reprType) = {
				if (classToWrite.hasCombos) {
					val repr = GenericType.covariant("Repr", description = "The implementing type")
					Some(repr) -> repr.toScalaType
				}
				else
					None -> classType
			}
			// Some of the implementation differs greatly between abstract and concrete implementations
			// Abstract implementations are the StoredX traits that don't match any concrete table
			val (highestLevelCustomExtensions, highestLevelProperties) = genericStoredLikeRef match {
				// Case: Abstract trait implementation (StoredXLike)
				//       => Only extends the "StoredXLike" trait and the XData trait, supplying the Repr type
				case Some(storedLikeRef) =>
					Pair[Extension](storedLikeRef(dataClassRef, reprType), dataClassRef) -> Empty
				
				// Case: Implementing trait or class
				//       => Specifies the implementation required for an X class or StoredX trait
				case None =>
					val (customExtensions, customProperties) = {
						// A property used for accessing the database row matching this entry
						// Only used if Vault references are allowed
						val accessProp =
							if (setup.modelCanReferToDB) Some(accessorFor(classToWrite, targeted)) else None
						
						// Case: Extending a parent StoredYLike trait
						//       => No need to define the properties again, except possibly the "access" property
						if (classToWrite.isExtension)
							Empty -> accessProp.emptyOrSingle
						// Case: Extending Stored & HasId directly => Implements the inherited abstract functions
						else {
							// Extends StoredModelConvertible & FromIdFactory
							val fromIdFactory = vault.fromIdFactory(ScalaType.int, reprType)
							val (stored, toModel) = {
								// Case: Using ids of type Long => Can't utilize StoredModelConvertible
								if (classToWrite.useLongId) {
									val stored = vault.stored(dataClassRef, idType)
									val toModel = ComputedProperty("toModel", Set(valueConversions, constant),
										isOverridden = true)("Constant(\"id\", id) + data.toModel")
									
									Pair[Extension](stored, modelConvertible) -> Some(toModel)
								}
								else
									Single[Extension](vault.storedModelConvertible(dataClassRef)) -> None
							}
							
							(stored :+[Extension] fromIdFactory, OptimizedIndexedSeq.concat(accessProp, toModel))
						}
					}
					
					(customExtensions :+[Extension] factoryWrapperRef(dataClassRef, reprType), customProperties)
			}
			val highestLevelExtensions = highestLevelCustomExtensions ++ inheritedExtensions
			val description = s"Represents a ${ classToWrite.name.doc } that has already been stored in the database.${
				classToWrite.description.prependIfNotEmpty(" \n") }"
			
			// Writes the StoredXLike trait, if needed
			// Also writes the StoredX trait
			repr match {
				// Case: Writing combo classes => Includes a StoredXLike trait and inherits it in the lower trait
				case Some(repr) =>
					File(storedPackage, TraitDeclaration(
						name = ((classToWrite.storedPrefix +: classToWrite.name) + likeSuffix).className,
						genericTypes = Single(repr),
						extensions = highestLevelExtensions,
						properties = highestLevelProperties,
						description = description,
						author = classToWrite.author,
						since = DeclarationDate.versionedToday))
						.write()
						.map { storedLikeRef =>
							val stored = TraitDeclaration(
								name = className,
								extensions = Single(storedLikeRef(classType)),
								description = description,
								author = classToWrite.author, since = DeclarationDate.versionedToday)
							
							Some(storedLikeRef) -> stored
						}
				// Case: No combo classes expected
				//       => Writes a concrete case class, or a trait if the written class is generic
				case None =>
					val isTrait = classToWrite.isGeneric
					val stored = InstanceDeclaration.classOrTrait(
						name = className,
						constructionParams = if (isTrait) Parameters.empty else constructionParams,
						extensions = highestLevelExtensions,
						properties = highestLevelProperties,
						methods = if (isTrait) Set() else concreteMethodImplementations,
						description = description,
						author = classToWrite.author,
						since = DeclarationDate.versionedToday,
						isTrait = isTrait, isCase = !isTrait)
					
					Success(None -> stored)
			}
		}
		stored.flatMap { case (storedLikeRef, stored) =>
			// Prepares the companion object
			val companionObject = {
				// dataFactory property is used in fromModel -parsing
				val dataFactory = ImmutableValue("dataFactory", Set(dataClassRef), isOverridden = true)(
					dataClassRef.target)
				// The StoredFromModelFactory inheritance is simplified if Int IDs are used
				val (fromModelFactoryTrait, complete) = {
					if (classToWrite.useLongId) {
						val complete = MethodDeclaration("complete", visibility = Protected, isOverridden = true)(
							Pair(Parameter("model", hasProperties), Parameter("data", dataClassRef)))(
							s"model(\"id\").tryLong.map { apply(_, data) }")
						(vault.storedFromModelFactory, Some(complete))
					}
					else
						(vault.standardStoredFactory, None)
				}
				
				// If a trait was written, includes a concrete implementation in this companion object
				val (applyToConcrete, nestedConcrete) = {
					// Case: Trait was written => Provides access to a concrete implementation
					if (classToWrite.isGeneric || classToWrite.hasCombos) {
						val concreteClassName = s"_$className"
						val constructConcrete =
							s"$concreteClassName(${ constructionParams.map { _.name }.mkString(", ") })"
						
						val nestedClass = ClassDeclaration(
							name = concreteClassName,
							constructionParams = constructionParams,
							extensions = Single(classType),
							methods = concreteMethodImplementations,
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
					// Case: The concrete class was already written => Only implements from model parsing
					else
						None -> None
				}
				
				ObjectDeclaration(
					name = className,
					extensions = Single(fromModelFactoryTrait(dataClassRef, classType)),
					properties = Single(dataFactory),
					methods = Set.concat(complete, applyToConcrete),
					nested = nestedConcrete.toSet
				)
			}
			
			// Writes the file
			File(storedPackage, companionObject, stored).write().map { storedLikeRef -> _ }
		}
	}
	
	/**
	 * Generates the "isDeprecated" (or "hasExpired") & "isValid" functions for Deprecatable classes.
	 * Generates no functions for other types of classes.
	 * @param classToWrite Class for which these functions are written
	 * @param naming Implicit naming conventions
	 * @return isDeprecated & isValid functions, if appropriate.
	 */
	private def deprecationPropertiesFor(classToWrite: Class)(implicit naming: NamingRules) =
	{
		// However, won't include any properties in situations where the deprecation is inherited
		// (in these situations, these properties have already been defined in the parent trait)
		classToWrite.nullDeprecationProperty.filterNot { _.isExtension } match {
			case Some(prop) =>
				Pair(
					ComputedProperty("isDeprecated",
						description = s"Whether this ${ classToWrite.name.doc } has already been deprecated")(
						s"${ prop.name.prop }.isDefined"),
					ComputedProperty("isValid",
						description = s"Whether this ${ classToWrite.name.doc } is still valid (not deprecated)")(
						"!isDeprecated")
				)
			case None =>
				classToWrite.expirationProperty.filterNot { _.isExtension } match {
					case Some(prop) =>
						Pair(
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
	
	private def accessorFor(classToWrite: Class, targeted: Boolean)
	                       (implicit naming: NamingRules, setup: VaultProjectSetup) =
	{
		val code = {
			if (targeted)
				TargetingWriter.singleAccessCodeFor(classToWrite, "id")
			else {
				val singleAccessRef = AccessWriter.singleIdReferenceFor(classToWrite)
				CodePiece(s"${ singleAccessRef.target }(id)", Set(singleAccessRef))
			}
		}
		ComputedProperty("access", code.references,
			description = s"An access point to this ${ classToWrite.name.doc } in the database")(code.text)
	}
	
	// Defines the withX methods in data class context
	private def concreteWithMethodsFor(properties: Iterable[Property], copyFunctionName: String)(implicit naming: NamingRules) =
		withMethodsFor(properties) { (prop, propName) =>
			prop.dataType.fromConcreteCode(propName)
				.mapText { wrappedValue => s"$copyFunctionName($propName = $wrappedValue)" }
		}
	
	// code accepts a property and parameter name and returns the implementing code
	private def withMethodsFor(properties: Iterable[Property])(writeCode: (Property, String) => CodePiece)
	                          (implicit naming: NamingRules) =
	{
		properties.map { prop =>
			val propName = prop.name.prop
			val code = writeCode(prop, propName)
			MethodDeclaration(withMethodNameFor(prop), code.references, isOverridden = true, isLowMergePriority = true)(
				Parameter(propName, prop.dataType.concrete.toScala))(code.text)
		}.toSet
	}
	
	private def withIdFor(classToWrite: Class, copyMethodName: String = "copy") =
		MethodDeclaration("withId", isOverridden = true)(Parameter("id", classToWrite.idType.toScala))(
			s"$copyMethodName(id = id)")
	
	private def toModelImplementationFor(classToWrite: Class, toModelImplementingParentReferences: Seq[Reference])
	                                    (implicit naming: NamingRules) =
	{
		// Code pieces for writing properties into a Model
		// Will not include properties from parent classes
		val propWrites = classToWrite.properties.filterNot { _.isExtension }.map { prop =>
			val propNameInModel = prop.jsonPropName.quoted
			prop.toJsonValueCode.withPrefix(s"$propNameInModel -> ")
		}
		// Utilizes toModel implementations from parents, if available
		val parentToModels = toModelImplementingParentReferences.reverseIterator
			.map { _.targetCode.mapText { parent => s"super[$parent].toModel" } }
			.reduceLeftOption { _.append(_, " ++ ") }
		
		if (propWrites.isEmpty) {
			// If toModel would just refer to super.toModel, skips it
			if (toModelImplementingParentReferences.hasSize(1))
				None
			else
				Some(parentToModels.getOrElse { CodePiece("Model.empty", Set(model)) })
		}
		else {
			val collection = CodePiece.collection(propWrites.size)(propWrites.reduceLeft { _.append(_, ", ") })
			val propsToModel = CodePiece("Model", Set(model)) + collection.withinParenthesis
			
			Some(parentToModels match {
				case Some(parentModels) => parentModels.append(propsToModel, " ++ ")
				case None => propsToModel
			})
		}
	}
	
	// Writes a property declaration for the model schema
	private def propertyDeclarationFrom(prop: Property)(implicit naming: NamingRules): CodePiece = {
		val dt = prop.dataType
		val name = prop.jsonPropName
		// Supports some alternative names:
		//      1) Different naming style used (camel case vs. underscore)
		//      2) Database property name used
		//      3) Inherited property names used
		val altNames = (((prop.alternativeNames + prop.name)
			.flatMap { name => Set(CamelCase.lower, UnderScore).map(name.singularIn) } ++
			prop.dbProperties.only.view
				.flatMap { dbProp => Set(CamelCase.lower, UnderScore).map(dbProp.name.singularIn) }) - name)
			.toVector.sorted
		// May specify a default value
		val default = prop.customDefaultValue.notEmpty.orElse {
			if (dt.supportsDefaultJsonValues) dt.nonEmptyDefaultValue.notEmpty else None
		}.map { v => dt.toJsonValueCode(v.text).referringTo(v.references) }
		
		// Writes only the necessary code parts (i.e. omits duplicate default parameters)
		var paramsCode = CodePiece(name.quoted).append(dt.valueDataType.targetCode, ", ")
		if (altNames.nonEmpty || default.isDefined)
			paramsCode = paramsCode.append(CodePiece.collection(altNames.size)(
				altNames.map { _.quoted }.mkString(", ")), ", ")
		default.foreach { default => paramsCode = paramsCode.append(default, ", ") }
		if ((dt.isOptional && (!dt.isBothOptionalAndConcrete || dt.sqlConversions.forall { _.target.isNullable })) ||
			!dt.supportsDefaultJsonValues)
			paramsCode = paramsCode.append("isOptional = true", ", ")
		
		// All property declarations start on a new line
		"\n" +: (propertyDeclaration.targetCode + paramsCode.withinParenthesis)
	}
	
	private def withMethodNameFor(prop: Property)(implicit naming: NamingRules): String = withMethodNameFor(prop.name)
	private def withMethodNameFor(propName: Name)(implicit naming: NamingRules) = (withPrefix + propName).function
}
