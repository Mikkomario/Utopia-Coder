package utopia.coder.vault.controller.writer.model

import utopia.coder.model.data
import utopia.coder.model.data.{Name, NamingRules}
import utopia.coder.model.enumeration.NamingConvention.{CamelCase, UnderScore}
import utopia.coder.model.scala.Visibility.{Private, Protected, Public}
import utopia.coder.model.scala.code.CodePiece
import utopia.coder.model.scala.datatype.Reference.Flow._
import utopia.coder.model.scala.datatype._
import utopia.coder.model.scala.declaration.PropertyDeclarationType.{ComputedProperty, LazyValue}
import utopia.coder.model.scala.declaration._
import utopia.coder.model.scala.{DeclarationDate, Package, Parameter}
import utopia.coder.vault.controller.writer.database.AccessWriter
import utopia.coder.vault.model.data.{Class, ClassModelReferences, ClassReferences, GenericClassModelReferences, Property, VaultProjectSetup}
import utopia.coder.vault.util.ClassMethodFactory
import utopia.coder.vault.util.VaultReferences._
import utopia.flow.collection.CollectionExtensions.{RichIterable, _}
import utopia.flow.collection.immutable.{Empty, Pair, Single}
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
	 * @param codec        Implicit codec used when writing files (implicit)
	  * @param setup        Target project -specific settings (implicit)
	  * @return References to generated classes
	  */
	def apply(classToWrite: Class, parentClassReferences: Seq[ClassReferences])
	         (implicit codec: Codec, setup: VaultProjectSetup, naming: NamingRules) =
	{
		// Writes the factory traits
		val factoryPackage = setup.modelPackage / s"factory.${ classToWrite.packageName }"
		writeFactoryTrait(classToWrite, factoryPackage, parentClassReferences).flatMap { factoryRef =>
			writeFactoryWrapperTrait(classToWrite, factoryRef, factoryPackage, parentClassReferences)
				.flatMap { factoryWrapperRef =>
					val dataPackage = setup.modelPackage / s"partial.${ classToWrite.packageName }"
					lazy val storePackage = setup.modelPackage / s"stored.${ classToWrite.packageName }"
					
					// For generic classes / traits, writes some traits
					// Will contain: 1) XDataLike, 2) XLike and 3) name of the buildCopy function used
					val traitWriteResult = {
						if (classToWrite.isGeneric)
							writeHasProps(classToWrite, dataPackage, parentClassReferences).flatMap { hasPropsRef =>
									writeDataLikeTrait(classToWrite, dataPackage, parentClassReferences, hasPropsRef,
										factoryRef)
										.flatMap { case (dataLikeRef, buildCopyName) =>
											writeStoredLike(classToWrite, storePackage, parentClassReferences,
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
									dataRef, genericRefs.map { _.storedLike })
									.map { storedRef =>
										// Returns references to generated classes
										ClassModelReferences(dataRef, storedRef, factoryRef, factoryWrapperRef,
											genericRefs)
									}
							}
					}
				}
		}
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
					classToWrite.name }-related factories which allow construction with individual properties",
				author = classToWrite.author,
				since = DeclarationDate.versionedToday
			)
		).write()
	}
	
	private def writeFactoryWrapperTrait(classToWrite: Class, factoryRef: Reference, factoryPackage: Package,
	                                     parentClassReferences: Seq[ClassReferences])
	                                    (implicit codec: Codec, setup: VaultProjectSetup, naming: NamingRules) =
	{
		val wrapped = GenericType("A", requirement = Some(TypeRequirement.childOf(factoryRef(ScalaType.basic("A")))),
			description = "Type of constructed instances")
		val wrappedType = wrapped.toScalaType
		val repr = GenericType.covariant("Repr", description = "Implementing type of this factory")
		val reprType = repr.toScalaType
		
		// When extending other classes:
		//      1) Extends YFactoryWrapper[A]
		//      2) Will not redefine wrappedFactory, wrap(...) or mapWrapped(...)
		//      3) Overrides renamed withX functions,
		//         since both parent traits (XFactory and YFactoryWrapper) define those
		//      4) Redefines the wrapped instance so that it extends both XFactory and YFactory
		//      5) Will not redefine withX properties from directly extended properties
		
		val (wrappedFactory, wrapMethods) = {
			if (classToWrite.isExtension)
				None -> Empty
			else {
				val wrappedFactory = PropertyDeclaration.newAbstract("wrappedFactory", wrappedType,
					description = "The factory wrapped by this instance",
					isProtected = true,
					isOverridden = classToWrite.isExtension)
				val wrap = MethodDeclaration.newAbstract(
					"wrap", reprType,
					description = "Mutates this item by wrapping a mutated instance",
					returnDescription = "Copy of this item with the specified wrapped factory",
					isProtected = true)(
					Parameter("factory", wrappedType, description = "The new factory instance to wrap"))
				val mapWrapped = MethodDeclaration("mapWrapped",
					visibility = Protected,
					description = "Modifies this item by mutating the wrapped factory instance",
					returnDescription = "Copy of this item with a mutated wrapped factory")(
					Parameter("f", mutate(wrappedType),
						description = "A function for mutating the wrapped factory instance"))(
					"wrap(f(wrappedFactory))")
				
				Some(wrappedFactory) -> Pair(wrap, mapWrapped)
			}
		}
		
		File(factoryPackage,
			TraitDeclaration(
				name = (classToWrite.name + factoryTraitSuffix + wrapperSuffix).className,
				genericTypes = Pair(wrapped, repr),
				extensions = Single[Extension](factoryRef(reprType)) ++
					parentClassReferences.map[Extension] { _.factoryWrapper(wrappedType, reprType) },
				properties = wrappedFactory.emptyOrSingle,
				methods = withMethodsFor(classToWrite.properties.filterNot { _.isDirectExtension }) { (prop, propName) =>
					prop.rename match {
						case Some((parentPropName, _)) => s"${ withMethodNameFor(parentPropName) }($propName)"
						case None => s"mapWrapped { _.${ withMethodNameFor(prop) }($propName) }"
					}
				} ++ wrapMethods,
				description = s"Common trait for classes that implement ${factoryRef.target} by wrapping a ${
					factoryRef.target } instance",
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
			description = s"Common trait for classes which provide access to ${ classToWrite.name } properties",
			author = classToWrite.author,
			since = DeclarationDate.versionedToday
		)).write()
	}
	
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
			description = s"Common trait for classes which provide read and copy access to ${
				classToWrite.name } properties",
			author = classToWrite.author,
			since = DeclarationDate.versionedToday
		)).write()
			// Includes the buildCopy function name in the return value since it is needed later
			.map { _ -> buildCopy.name }
	}
	
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
			Parameter(prop.name.prop, prop.dataType.toScala, prop.defaultValue,
				description = prop.description)
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
			// Case: Abstract class => Extends XDataLike[XData] trait and implements renamed property references
			case Some(dataLikeRef) =>
				val extensions: Seq[Extension] = Single[Extension](dataLikeRef(dataClassType))
				val renamedProperties = classToWrite.properties.flatMap { prop =>
					prop.rename.map { case (originalName, localName) =>
						ComputedProperty(originalName.prop, isOverridden = true)(localName.prop)
					}
				}
				
				(extensions, renamedProperties, Set[MethodDeclaration]())
				
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
				val toModelCode = toModelImplementationFor(classToWrite, parentDataClassReferences)
				val toModel = toModelCode.map { code =>
					ComputedProperty("toModel", code.references, isOverridden = true)(code.text)
				}
				
				// Only implements the withX methods which have not yet been implemented in a generic parent
				// (via buildCopy)
				val withMethods = concreteWithMethodsFor(
					classToWrite.properties.filterNot { _.isDirectExtension }, "copy")
				
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
				
				(extensions, toModel.emptyOrSingle, withMethods ++ copyMethods)
		}
		
		// Defines either a trait or a class
		val dataClass = {
			if (classToWrite.isGeneric)
				TraitDeclaration(
					name = dataClassName,
					extensions = customExtensions ++ inheritedExtensions,
					properties = renameImplementations ++ deprecationProps ++ customProps,
					methods = customMethods,
					description = classToWrite.description,
					author = classToWrite.author,
					since = DeclarationDate.versionedToday
				)
			else
				ClassDeclaration(
					name = dataClassName,
					constructionParams = constructionParams,
					extensions = customExtensions ++ inheritedExtensions,
					properties = renameImplementations ++ deprecationProps ++ customProps,
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
		
		val schemaCode = modelDeclaration.targetCode +
			CodePiece.collection(classToWrite.properties.size)(
				classToWrite.properties.map(propertyDeclarationFrom).reduceLeftOption { _.append(_, ", ") }
					.getOrElse(CodePiece.empty).withinParenthesis
			)
		val schema = LazyValue("schema", schemaCode.references,
			isOverridden = !fromModelMayFail, isLowMergePriority = true)(schemaCode.text)
		
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
	private def writeStoredLike(classToWrite: Class, storedPackage: Package, parentClassReferences: Seq[ClassReferences],
	                            dataLikeRef: Reference, factoryWrapperRef: Reference, buildCopyName: String)
	                           (implicit codec: Codec, setup: VaultProjectSetup, naming: NamingRules) =
	{
		// When extending YStoredLike[Repr] with YStored, the Repr type must conform to YStored
		val parentStoredRefs = parentClassReferences.map { _.stored }
		
		val dataType = ScalaType.basic("Data")
		val data = GenericType.childOf("Data", dataLikeRef(dataType), description = "Type of the wrapped data")
		val repr = GenericType.covariant("Repr",
			requirement = parentStoredRefs.headOption.map { TypeRequirement.childOf(_) },
			description = "Implementing type")
		val reprType = repr.toScalaType
		
		val inheritedExtensions = parentClassReferences.flatMap[Extension] { _.generic.map { _.storedLike(reprType) } } ++
			parentStoredRefs.map(Extension.fromReference)
		
		// Certain extensions are different if Vault references are enabled
		// However, if extending another YStoredLike trait, expects these to be defined there already
		val customExtensions = {
			if (classToWrite.isExtension)
				Empty
			else if (setup.modelCanReferToDB) {
				val stored = vault.stored(dataType, classToWrite.idType.toScala)
				val fromIdFactory = vault.fromIdFactory(ScalaType.int, reprType)
				
				Pair[Extension](stored, fromIdFactory)
			}
			else {
				val stored = metropolis.stored(dataType)
				Single[Extension](stored)
			}
		}
		
		val wrappedFactory = ComputedProperty("wrappedFactory", visibility = Protected, isOverridden = true)("data")
		// Implements access properties based on the wrapped data, but not for parent class properties
		val propertyAccessors = classToWrite.properties.filterNot { _.isDirectExtension }.map { prop =>
			val propName = prop.name.prop
			ComputedProperty(propName, isOverridden = true)(s"data.$propName")
		}
		
		// Implements the copyX(...) function required by XDataLike
		val copyParams = classToWrite.properties
			.map { prop => Parameter(prop.name.prop, prop.dataType.toScala) }
		val buildCopy = MethodDeclaration(buildCopyName, isOverridden = true)(
			copyParams)(s"wrap(data.$buildCopyName(${ copyParams.map { _.name }.mkString(", ") }))")
		
		File(storedPackage, TraitDeclaration(
			name = ((storedPrefix +: classToWrite.name) + likeSuffix).className,
			genericTypes = Pair(data, repr),
			extensions = customExtensions ++
				Pair[Extension](factoryWrapperRef(dataType, reprType), dataLikeRef(reprType)) ++ inheritedExtensions,
			properties = propertyAccessors :+ wrappedFactory,
			methods = Set(buildCopy),
			description = s"Common trait for ${ classToWrite.name.pluralDoc } which have been stored in the database",
			author = classToWrite.author,
			since = DeclarationDate.versionedToday
		)).write()
	}
	
	// Writes the stored model version
	// Returns a reference
	private def writeStored(classToWrite: Class, storedPackage: Package, parentClassReferences: Seq[ClassReferences],
	                        factoryWrapperRef: Reference, dataClassRef: Reference, storedLikeRef: Option[Reference])
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
			// When inheriting other classes, extends their YData with StoredYLike[Data, Repr]
			val inheritedExtensions = parentClassReferences.flatMap[Extension] { refs =>
				refs.data +: refs.generic.map[Extension] { _.storedLike(dataClassRef, classType) }.emptyOrSingle
			}
			
			// Some of the implementation differs greatly between abstract and concrete implementations
			val (customExtensions, properties, methods) = storedLikeRef match {
				// Case: Abstract trait => Only extends the "like" trait and the data trait, supplying the Repr type
				case Some(storedLikeRef) =>
					val storedLike = storedLikeRef(dataClassRef, classType)
					
					(Pair[Extension](storedLike, dataClassRef), Empty, Set[MethodDeclaration]())
				
				// Case: Concrete class => Specifies the implementation required for a stored class
				case None =>
					val factoryWrapper = factoryWrapperRef(dataClassRef, classType)
					
					// If Vault references are allowed, provides certain custom features
					val (customExtensions, customProperties, withId) = {
						// Case: Extending another StoredY trait => No need to define the properties again,
						// except for an accessor property and the withX function
						if (classToWrite.isExtension)
							(Empty, if (setup.modelCanReferToDB) Single(accessorFor(classToWrite)) else Empty,
								Some(withIdFor(classToWrite)))
						else {
							val wrappedFactory = ComputedProperty("wrappedFactory", visibility = Protected,
								isOverridden = true)("data")
							
							// Case: Vault references are enabled => Extends StoredModelConvertible & FromIdFactory
							// and adds a custom accessor property
							if (setup.modelCanReferToDB) {
								val fromIdFactory = vault.fromIdFactory(ScalaType.int, classType)
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
								val accessor = accessorFor(classToWrite)
								val withId = withIdFor(classToWrite)
								
								(stored :+[Extension] fromIdFactory,
									Pair(accessor, wrappedFactory) ++ toModel, Some(withId))
							}
							// Case: Vault references are not enabled => Utilizes StoredModelConvertible from Metropolis
							else {
								val stored = metropolis.storedModelConvertible(dataClassRef)
								(Single[Extension](stored), Single(wrappedFactory), None)
							}
						}
					}
					
					(customExtensions :+[Extension] factoryWrapper, customProperties, Set(wrap) ++ withId)
			}
			val description = s"Represents a ${ classToWrite.name.doc } that has already been stored in the database"
			
			// Writes either a class or a trait
			if (classToWrite.isGeneric)
				TraitDeclaration(
					name = className,
					extensions = customExtensions ++ inheritedExtensions,
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
					extensions = customExtensions ++ inheritedExtensions,
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
					
					val nestedClass = ClassDeclaration(
						name = concreteClassName,
						constructionParams = constructionParams,
						extensions = Single(classType),
						methods = Set(wrap, withIdFor(classToWrite)),
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
		// However, won't include any properties in situations where the deprecation is inherited
		// (in these situations, these properties have already been defined in the parent trait)
		classToWrite.deprecationProperty.filterNot { _.isExtension } match {
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
				classToWrite.expirationProperty.filterNot { _.isExtension } match {
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
	
	private def fromModelFor(classToWrite: Class, dataClassName: String)(implicit naming: NamingRules) = {
		def _modelFromAssignments(assignments: CodePiece) =
			assignments.withinParenthesis.withPrefix(dataClassName)
		
		if (classToWrite.fromJsonMayFail)
			ClassMethodFactory.classFromModel(classToWrite, "schema.validate(model)",
				isFromJson = true)(_modelFromAssignments)
		else
			ClassMethodFactory.classFromValidatedModel(classToWrite, isFromJson = true)(_modelFromAssignments)
	}
	
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
			val dt = prop.dataType
			if (dt.supportsDefaultJsonValues) dt.nonEmptyDefaultValue.notEmpty else None
		}.map { v => prop.dataType.toJsonValueCode(v.text).referringTo(v.references) }
		
		// Writes only the necessary code parts (i.e. omits duplicate default parameters)
		var paramsCode = CodePiece(name.quoted).append(prop.dataType.valueDataType.targetCode, ", ")
		if (altNames.nonEmpty || default.isDefined)
			paramsCode = paramsCode.append(CodePiece.collection(altNames.size)(
				s"(${ altNames.map { _.quoted }.mkString(", ") })"),
				", ")
		default.foreach { default => paramsCode = paramsCode.append(default, ", ") }
		if (prop.dataType.isOptional || !prop.dataType.supportsDefaultJsonValues)
			paramsCode = paramsCode.append("isOptional = true", ", ")
		
		propertyDeclaration.targetCode + paramsCode.withinParenthesis
	}
	
	private def withMethodNameFor(prop: Property)(implicit naming: NamingRules): String = withMethodNameFor(prop.name)
	private def withMethodNameFor(propName: Name)(implicit naming: NamingRules) = (withPrefix + propName).function
}
