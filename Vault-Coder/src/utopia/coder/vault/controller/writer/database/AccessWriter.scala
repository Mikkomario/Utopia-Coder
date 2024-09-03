package utopia.coder.vault.controller.writer.database

import utopia.coder.model.data
import utopia.coder.model.data.{Name, NamingRules}
import utopia.coder.model.enumeration.NameContext.{ClassPropName, FunctionName}
import utopia.coder.model.enumeration.NamingConvention.{CamelCase, UnderScore}
import utopia.coder.model.scala.Visibility.{Private, Protected}
import utopia.coder.model.scala.code.CodePiece
import utopia.coder.model.scala.datatype.Reference._
import utopia.coder.model.scala.datatype.{Extension, GenericType, Reference, ScalaType}
import utopia.coder.model.scala.declaration.PropertyDeclarationType.ComputedProperty
import utopia.coder.model.scala.declaration._
import utopia.coder.model.scala.{DeclarationDate, Package, Parameter, Parameters}
import utopia.coder.vault.model.data.{Class, ClassReferences, CombinationData, DbProperty, Property, VaultProjectSetup}
import utopia.coder.vault.model.datatype.PropertyType
import utopia.coder.vault.model.datatype.StandardPropertyType.BasicPropertyType.IntNumber
import utopia.coder.vault.model.datatype.StandardPropertyType.ClassReference
import utopia.coder.vault.util.VaultReferences.Vault._
import utopia.coder.vault.util.VaultReferences._
import utopia.flow.collection.CollectionExtensions._
import utopia.flow.collection.immutable.{Empty, Pair, Single}
import utopia.flow.operator.equality.EqualsExtensions._
import utopia.flow.util.StringExtensions._

import scala.io.Codec
import scala.util.Success

/**
  * Used for writing database access templates
  * @author Mikko Hilpinen
  * @since 2.9.2021, v0.1
  */
object AccessWriter
{
	// ATTRIBUTES   ------------------------------
	
	private val accessPrefix = data.Name("Db", "Db", CamelCase.capitalized)
	private val singleAccessPrefix = accessPrefix + "Single"
	
	private val manyPrefix = data.Name("Many", "Many", CamelCase.capitalized)
	
	private val accessTraitSuffix = data.Name("Access", "Access", CamelCase.capitalized)
	private val genericAccessSuffix = data.Name("Like", "Like", CamelCase.capitalized)
	private val uniqueAccessPrefix = data.Name("Unique", "Unique", CamelCase.capitalized)
	
	private val newPrefix = data.Name("new", "new", CamelCase.lower)
	
	private lazy val connectionParam = Parameter("connection", connection)
	
	private lazy val self = ComputedProperty("self", visibility = Protected, isOverridden = true)("this")
	
	
	// OTHER    ------------------------------
	
	/**
	 * Generates access point references for a class, as if files were written normally
	 * @param accessPackage Package where many & single access packages are placed
	 * @param classToWrite Class for which these references are generated
	 * @param naming implicit naming rules
	 * @return Reference to the generic single access trait + generic many access trait.
	 *         None if these traits would not have been generated.
	 */
	def generateReferences(accessPackage: Package, classToWrite: Class)(implicit naming: NamingRules) = {
		if (classToWrite.writeGenericAccess || classToWrite.isGeneric) {
			val many = Reference(packageFor(accessPackage/"many", classToWrite),
				((manyPrefix +: classToWrite.name) + accessTraitSuffix + genericAccessSuffix).className)
			val single = Reference(packageFor(accessPackage/"single", classToWrite),
				((uniqueAccessPrefix +: classToWrite.name) + accessTraitSuffix + genericAccessSuffix).className)
			
			Some(Pair(single, many))
		}
		else
			None
	}
	
	/**
	  * @param c     A class
	  * @param setup Project setup (implicit)
	  * @return Reference to the access point for unique instances of that class based on their id
	  */
	def singleIdReferenceFor(c: Class)(implicit setup: VaultProjectSetup, naming: NamingRules) =
		Reference(singleAccessPackageFor(c), singleIdAccessNameFor(c.name))
	
	/**
	  * Writes database access point objects and traits
	  * @param classToWrite          Class based on which these access points are written
	  * @param parentClassReferences References for files generated for the parent classes
	 * @param modelRef              Reference to the stored model class
	  * @param factoryRef            Reference to the from DB factory object
	  * @param dbPropsOrDbModelRef   Reference to the database properties -trait or the database model class
	  * @param descriptionReferences References to the described model version + single description link access point +
	  *                              many description links access point, if applicable for this class
	  * @param codec                 Implicit codec to use when writing files
	  * @param setup                 Implicit project-specific setup to use
	  * @return References to the UniqueXAccessLike and ManyXAccessLike -traits
	  *         (i.e. the most generic access point for unique and multiple items), if these were generated
	  */
	def apply(classToWrite: Class, parentClassReferences: Seq[ClassReferences], modelRef: Reference,
	          factoryRef: Reference, dbPropsOrDbModelRef: Reference,
	          descriptionReferences: Option[(Reference, Reference, Reference)])
	         (implicit codec: Codec, setup: VaultProjectSetup, naming: NamingRules) =
	{
		// Standard access point properties (factory, model)
		// are present in both single and many model access points
		// However, these properties are only implemented in concrete classes (i.e. not in generic classes)
		val factoryProperty = ComputedProperty("factory", Set(factoryRef), isOverridden = true)(factoryRef.target)
		val modelProperty = {
			// When writing generic traits, specifies model type explicitly
			val explicitType: Option[ScalaType] = {
				if (classToWrite.isGeneric) {
					val base = dbPropsOrDbModelRef
					if (classToWrite.isNullDeprecatable)
						Some((base: ScalaType).withOther(vault.nullDeprecatable(vault.storable)))
					else Some(base)
				}
				else
					None
			}
			ComputedProperty("model", Set(dbPropsOrDbModelRef),
				explicitOutputType = explicitType, visibility = Protected,
				description = "Model which contains the primary database properties interacted with in this access point",
				isOverridden = classToWrite.isExtension)(
				if (classToWrite.isGeneric) "" else dbPropsOrDbModelRef.target)
		}
		
		// For classes that support expiration, deprecate() -method is added for all traits
		// Null-deprecation -supporting classes already inherit this method
		// Option[Pair[method]], where first method is for individual access and second for many access
		val deprecationMethods = classToWrite.expirationProperty.map { prop =>
			Pair(prop.name.prop, prop.name.props).map { propName =>
				MethodDeclaration("deprecate", Set(flow.now, flow.valueConversions),
					description = s"Deprecates all accessible ${classToWrite.name.pluralDoc}",
					returnDescription = "Whether any row was targeted")(
					Parameters(Single(Empty), Single(connectionParam)))(s"$propName = Now")
			}
		}
		// Root access points extend either the UnconditionalView or the NonDeprecatedView -trait,
		// depending on whether deprecation is supported
		val rootViewExtension: Extension = {
			if (classToWrite.isDeprecatable) nonDeprecatedView(modelRef) else unconditionalView
		}
		
		writeSingleAccesses(classToWrite, parentClassReferences, modelRef, descriptionReferences, modelProperty,
			factoryProperty, deprecationMethods.map { _.first }, rootViewExtension)
			.flatMap { uniqueAccessLikeRef =>
				writeManyAccesses(classToWrite, parentClassReferences, modelRef, descriptionReferences, modelProperty,
					factoryProperty, deprecationMethods.map { _.second })
					.map { uniqueAccessLikeRef -> _ }
			}
	}
	
	/**
	  * Writes the access points for combined items
	  * @param combo Combo to access
	  * @param genericUniqueAccessRef A reference to the UniqueXAccessLike -trait for the combo parent
	  * @param genericManyAccessRef A reference to the ManyXAccessLike -trait for the combo parent
	  * @param modelRef A reference to the combined model class
	  * @param factoryRef A reference to the combined model from DB factory
	  * @param parentDbModelRef A reference to the combo parent database model
	  * @param childDbModelRef A reference to the combo child database model
	  * @param codec Implicit codec to use
	  * @param setup Implicit project setup to use
	  * @param naming Implicit naming rules to use
	  * @return Success or a failure
	  */
	def writeComboAccessPoints(combo: CombinationData, genericUniqueAccessRef: Reference,
	                           genericManyAccessRef: Reference, modelRef: Reference,
	                           factoryRef: Reference, parentDbModelRef: Reference, childDbModelRef: Reference)
	                          (implicit codec: Codec, setup: VaultProjectSetup, naming: NamingRules) =
	{
		// Writes many & single accesses
		writeManyComboAccesses(combo, genericManyAccessRef, modelRef, factoryRef, childDbModelRef)
			.flatMap { _ =>
				writeSingleComboAccesses(combo, genericUniqueAccessRef, modelRef, factoryRef,
					parentDbModelRef, childDbModelRef)
					// Doesn't return any references
					.map { _ => () }
			}
	}
	
	/**
	 * Generates a companion object for an access trait
	 * @param accessTraitType Type of the access trait for which this object is added
	 * @return Declaration of the companion object
	 */
	def accessCompanionObject(accessTraitType: ScalaType) = {
		val subViewName = s"_$accessTraitType"
		ObjectDeclaration(
			name = accessTraitType.toString,
			extensions = Single(viewFactory(accessTraitType)),
			methods = Set(
				MethodDeclaration("apply", explicitOutputType = Some(accessTraitType),
					returnDescription = "An access point that applies the specified filter condition (only)",
					isOverridden = true)(
					Parameter("condition", condition, description = "Condition to apply to all requests"))(
					s"$subViewName(Some(condition))")
			),
			nested = Set(subViewDeclaration(accessTraitType, subViewName))
		)
	}
	
	/**
	 * Generates the apply method used in access point filtering / construction.
	 * Typically these are placed in the semi-concrete access traits.
	 * @param traitType Type of the access trait returned by this method
	 * @return Method for constructing new access points
	 */
	// Assumes that the companion object contains an apply method
	def filteringApply(traitType: ScalaType) =
		MethodDeclaration("apply", explicitOutputType = Some(traitType), isOverridden = true)(
			Parameter("condition", condition))(s"$traitType(condition)")
	
	// Writes all single item access points
	// Returns Try[Option[UniqueAccessLikeRef]] (i.e. reference to the generic single access trait, if generated)
	private def writeSingleAccesses(classToWrite: Class, parentClassReferences: Seq[ClassReferences],
	                                modelRef: Reference,
	                                descriptionReferences: Option[(Reference, Reference, Reference)],
	                                modelProperty: PropertyDeclaration, factoryProperty: PropertyDeclaration,
	                                deprecationMethod: Option[MethodDeclaration], rootViewExtension: Extension)
	                               (implicit naming: NamingRules, codec: Codec, setup: VaultProjectSetup) =
	{
		val singleAccessPackage = singleAccessPackageFor(classToWrite)
		// Writes UniqueXAccess trait, plus possibly UniqueXAccessLike
		writeUniqueAccess(classToWrite, parentClassReferences, modelRef, singleAccessPackage, modelProperty,
			factoryProperty, deprecationMethod)
			.flatMap { case (genericUniqueTraitRef, uniqueAccessRef) =>
				uniqueAccessRef match {
					// Case: Concrete class => Writes SingleXAccess and DbX (the root access)
					case Some(uniqueAccessRef) =>
						writeSingleIdAccess(classToWrite, modelRef, uniqueAccessRef, descriptionReferences,
							singleAccessPackage)
							.flatMap { singleIdAccessRef =>
								val modifiedModelProperty = modelProperty
									.copy(isOverridden = false, visibility = Private)
								writeSingleRootAccess(classToWrite.name, classToWrite.idType, modelRef,
									singleRowModelAccess, uniqueAccessRef, singleIdAccessRef, singleAccessPackage,
									Pair(modifiedModelProperty, factoryProperty), rootViewExtension, classToWrite.author,
									isDeprecatable = classToWrite.isDeprecatable)
									.map { _ => genericUniqueTraitRef }
							}
					
					// Case: Unique access trait was not written because the class was generic
					//       => Won't write other classes either
					case None => Success(genericUniqueTraitRef)
				}
			}
	}
	
	private def writeSingleComboAccesses(combo: CombinationData, genericUniqueAccessTraitRef: Reference,
	                                     combinedModelRef: Reference, combinedFactoryRef: Reference,
	                                     parentDbModelRef: Reference, childDbModelRef: Reference)
	                                    (implicit naming: NamingRules, codec: Codec, setup: VaultProjectSetup) =
	{
		val singleAccessPackage = singleAccessPackageFor(combo.parentClass)
		// Writes the unique access trait
		val uniqueAccessName = uniqueAccessTraitNameFrom(combo.name).className
		val uniqueAccessType = ScalaType.basic(uniqueAccessName)
		val deprecationParent = deprecationReferenceFor(combo.parentClass).map[Extension] { _(uniqueAccessType) }
		val uniqueTraitParents: Seq[Extension] = {
			val base: Extension = genericUniqueAccessTraitRef(combinedModelRef, uniqueAccessType)
			if (combo.combinationType.isOneToMany)
				Single(base) ++ deprecationParent
			else {
				if (combo.parentClass.recordsIndexedCreationTime)
					Pair[Extension](base, singleChronoRowModelAccess(combinedModelRef, uniqueAccessType)) ++
						deprecationParent
				else
					Pair[Extension](base, singleRowModelAccess(combinedModelRef)) ++ deprecationParent
			}
		}
		val factoryProp = ComputedProperty("factory", Set(combinedFactoryRef), isOverridden = true)(
			combinedFactoryRef.target)
		val childModelProp = ComputedProperty((combo.childName + "Model").prop, Set(childDbModelRef),
			visibility = Protected,
			description = s"A database model (factory) used for interacting with the linked ${
				if (combo.combinationType.isOneToMany) combo.childName.pluralDoc else combo.childName.doc }")(
			childDbModelRef.target)
		val childGetters = {
			// Can't write one-to-many getters because they return a different data structure
			// (i.e. can't be written with just pullColumn(...))
			if (combo.combinationType.isOneToMany)
				Empty
			else
				propertyGettersFor(combo.childClass, childModelProp.name,
					isCombo = true) { n => (combo.childName + n).prop }
		}
		val childSetters = {
			// Won't write setters if there are no getters
			if (childGetters.isEmpty)
				Set[MethodDeclaration]()
			else
				propertySettersFor(combo.childClass, childModelProp.name) { pName =>
					(combo.childName +: pName).prop
				}
		}
		
		val traitWriteResult = File(singleAccessPackage,
			accessCompanionObject(uniqueAccessType),
			TraitDeclaration(uniqueAccessName,
				extensions = uniqueTraitParents,
				properties = Vector(self, factoryProp, childModelProp) ++ childGetters,
				methods = childSetters + filteringApply(uniqueAccessType),
				description = s"A common trait for access points that return distinct ${ combo.name.pluralDoc }",
				author = combo.author, since = DeclarationDate.versionedToday
			)
		).write()
		
		// For non-generic classes, writes the concrete access points, also
		if (combo.parentClass.isGeneric)
			traitWriteResult
		else
			traitWriteResult.flatMap { uniqueRef =>
				// Writes the single id access -class
				val idType = combo.parentClass.idType
				val (idAccessParentRef, idAccessParentProps) = {
					// TODO: WET WET (from writeSingleIdAccess)
					if (combo.parentClass.useLongId) {
						val idValueCode = idType.toValueCode("id")
						Extension(singleIdModelAccess(combinedModelRef)) -> Vector(
							ComputedProperty("idValue", idValueCode.references, isOverridden = true)(idValueCode.text))
					}
					else
						Extension(singleIntIdModelAccess(combinedModelRef)) -> Empty
				}
				File(singleAccessPackage,
					ClassDeclaration(singleIdAccessNameFor(combo.name),
						constructionParams = Single(Parameter("id", idType.toScala)),
						extensions = Pair(uniqueRef, idAccessParentRef),
						properties = idAccessParentProps,
						description = s"An access point to individual ${ combo.name.pluralDoc }, based on their ${
							combo.parentName.doc } id",
						author = combo.author, since = DeclarationDate.versionedToday, isCaseClass = true
					)
				).write().flatMap { singleIdAccessRef =>
					// Writes the root access object
					val rootParent: Extension = {
						if (combo.isDeprecatable)
							nonDeprecatedView(combinedModelRef)
						else
							unconditionalView
					}
					writeSingleRootAccess(combo.name, idType, combinedModelRef,
						if (combo.combinationType.isOneToMany) singleModelAccess else singleRowModelAccess,
						uniqueRef, singleIdAccessRef, singleAccessPackage,
						Vector(factoryProp,
							ComputedProperty("model", Set(parentDbModelRef), visibility = Private,
								description = s"A database model (factory) used for interacting with linked ${
									combo.parentName.pluralDoc }")(parentDbModelRef.target),
							childModelProp),
						rootParent, combo.author, isDeprecatable = deprecationParent.isDefined)
				}
			}
	}
	
	// Returns the more generic reference and then less generic reference
	private def writeUniqueAccess(classToWrite: Class, parentClassReferences: Seq[ClassReferences],
	                              modelRef: Reference, singleAccessPackage: Package, modelProperty: PropertyDeclaration,
	                              factoryProperty: PropertyDeclaration, deprecationMethod: Option[MethodDeclaration])
	                             (implicit naming: NamingRules, codec: Codec, setup: VaultProjectSetup) =
	{
		// TODO: WET WET (much of similar code at writeManyAccessTrait)
		val uniqueAccessTraitName = uniqueAccessTraitNameFrom(classToWrite.name)
		
		// Properties and methods that will be written to the highest trait (which may vary)
		// These are: .model, .id and various property getters and setters
		// Note: Will not override id nor getters or setters already defined in higher classes (in case of inheritance)
		val pullId = {
			if (classToWrite.isExtension)
				None
			else {
				val pullIdCode = classToWrite.idType.optional.fromValueCode(s"pullColumn(index)")
				Some(ComputedProperty("id", pullIdCode.references, implicitParams = Single(connectionParam),
					description = s"Unique id of the accessible ${ classToWrite.name }. None if no ${
						classToWrite.name } was accessible.")(pullIdCode.text))
			}
		}
		val highestTraitProperties = (modelProperty +:
			propertyGettersFor(classToWrite, modelProperty.name) { _.prop }) ++ pullId
		val highestTraitMethods = propertySettersFor(classToWrite, modelProperty.name) { _.prop } ++ deprecationMethod
		
		// Writes the more generic trait version (-Like) first, if one is requested
		val parentRef = {
			if (classToWrite.writeGenericAccess || classToWrite.isGeneric) {
				val item = GenericType.covariant("A",
					description = s"Type of read (${ classToWrite.name.pluralDoc } -like) instances")
				val itemType = item.toScalaType
				val repr = GenericType.covariant("Repr", description = "Type of this access point")
				val reprType = repr.toScalaType
				
				val extensions = {
					// Case: Extending an abstract trait => Simply refers to their UniqueYAccessLike[...] trait
					if (classToWrite.isExtension)
						parentClassReferences
							.flatMap[Extension] { _.genericUniqueAccessTrait.map { _(itemType, reprType) } }
					// Extends SingleModelAccess instead of SingleRowModelAccess because sub-traits may vary
					else
						Vector[Extension](
							singleModelAccess(itemType),
							distinctModelAccess(itemType, ScalaType.option(itemType), flow.value),
							filterableView(reprType),
							indexed
						)
				}
				
				File(singleAccessPackage,
					TraitDeclaration(
						name = (uniqueAccessTraitName + genericAccessSuffix).className,
						genericTypes = Pair(item, repr),
						extensions = extensions,
						properties = highestTraitProperties,
						methods = highestTraitMethods,
						description = s"A common trait for access points which target individual ${
							classToWrite.name.pluralDoc } or similar items at a time",
						author = classToWrite.author, since = DeclarationDate.versionedToday
					)
				).write().map { Some(_) }
			}
			else
				Success(None)
		}
		
		// Writes the actual access trait, but only for concrete classes
		if (classToWrite.isGeneric)
			parentRef.map { _ -> None }
		else
			parentRef.flatMap { parentRef =>
				val traitNameString = uniqueAccessTraitName.className
				val traitType = ScalaType.basic(traitNameString)
				
				// The parent types depend from 4 factors:
				//      1) Whether generic type is used,
				//      2) Whether other traits are extended,
				//      3) Whether row creation time is recorded, and
				//      4) Whether deprecation is used
				val deprecationParentRef = deprecationReferenceFor(classToWrite)
				val rowAccessParent: Extension = {
					if (classToWrite.recordsIndexedCreationTime)
						singleChronoRowModelAccess(modelRef, traitType)
					else
						singleRowModelAccess(modelRef)
				}
				val parents: Seq[Extension] = parentRef match {
					// Case: Extending UniqueXLike[...] => Only defines the row access & deprecation extensions
					case Some(genericParent) =>
						Vector[Extension](genericParent(modelRef, traitType), rowAccessParent) ++
							deprecationParentRef.map { _(traitType) }
					case None =>
						// Case: Extending another generic trait => Same logic with different parents
						if (classToWrite.isExtension)
							parentClassReferences
								.flatMap[Extension] { _.genericUniqueAccessTrait.map { _(modelRef, traitType) } } ++
								deprecationParentRef.map[Extension] { _(traitType) }
						// Case: No special extensions => Extends the standard access traits
						else
							Vector[Extension](
								rowAccessParent,
								distinctModelAccess(modelRef, ScalaType.option(modelRef), flow.value),
								deprecationParentRef.getOrElse(filterableView)(traitType),
								indexed
							)
				}
				val baseProperties = Vector(self, factoryProperty)
				val properties = if (parentRef.isDefined) baseProperties else baseProperties ++ highestTraitProperties
				val filterM = filteringApply(traitType)
				val methods = if (parentRef.isDefined) Set(filterM) else highestTraitMethods + filterM
				
				File(singleAccessPackage,
					accessCompanionObject(traitType),
					TraitDeclaration(traitNameString,
						extensions = parents, properties = properties, methods = methods,
						description = s"A common trait for access points that return individual and distinct ${
							classToWrite.name.pluralDoc
						}.",
						author = classToWrite.author,
						since = DeclarationDate.versionedToday
					)
				).write().map { childRef => parentRef -> Some(childRef) }
			}
	}
	
	// Writes the single model by id access point
	// This access point is used for accessing individual items based on their id
	// The inherited trait depends on whether descriptions are supported,
	// this also affects implemented properties
	private def writeSingleIdAccess(classToWrite: Class, modelRef: Reference, uniqueAccessRef: Reference,
	                                descriptionReferences: Option[(Reference, Reference, Reference)],
	                                singleAccessPackage: Package)
	                               (implicit naming: NamingRules, codec: Codec, setup: VaultProjectSetup) =
	{
		val (singleIdAccessParent, singleIdAccessParentProperties) = descriptionReferences match {
			// Case: Class uses description => extends described access version with its properties
			case Some((describedRef, singleDescRef, manyDescsRef)) =>
				val props = Vector(
					ComputedProperty("singleDescriptionAccess", Set(singleDescRef), Protected,
						isOverridden = true)(singleDescRef.target),
					ComputedProperty("manyDescriptionsAccess", Set(manyDescsRef), Protected, isOverridden = true)(
						manyDescsRef.target),
					ComputedProperty("describedFactory", Set(describedRef), Protected, isOverridden = true)(
						describedRef.target)
				)
				Extension(citadel.singleIdDescribedAccess(modelRef, describedRef)) -> props
			// Case: Descriptions are not supported => Extends SingleIdModel or its easier sub-trait
			case None =>
				if (classToWrite.useLongId) {
					val idValueCode = classToWrite.idType.toValueCode("id")
					Extension(singleIdModelAccess(modelRef)) -> Vector(
						ComputedProperty("idValue", idValueCode.references, isOverridden = true)(idValueCode.text))
				}
				else
					Extension(singleIntIdModelAccess(modelRef)) -> Empty
		}
		File(singleAccessPackage,
			ClassDeclaration(singleIdAccessNameFor(classToWrite.name),
				constructionParams = Vector(Parameter("id", classToWrite.idType.toScala)),
				extensions = Vector(uniqueAccessRef, singleIdAccessParent),
				// Implements the .condition property
				properties = singleIdAccessParentProperties,
				description = s"An access point to individual ${
					classToWrite.name.pluralDoc }, based on their id",
				author = classToWrite.author, since = DeclarationDate.versionedToday, isCaseClass = true
			)
		).write()
	}
	
	// Writes the single model access point
	// Root access points extend either the UnconditionalView or the NonDeprecatedView -trait,
	// depending on whether deprecation is supported
	private def writeSingleRootAccess(className: Name, idType: PropertyType, modelRef: Reference,
	                                  singleModelAccessRef: Reference, uniqueAccessRef: Reference,
	                                  singleIdAccessRef: Reference,
	                                  singleAccessPackage: Package, baseProperties: Seq[PropertyDeclaration],
	                                  rootViewExtension: Extension, author: String, isDeprecatable: Boolean)
	                                 (implicit naming: NamingRules, codec: Codec, setup: VaultProjectSetup) =
	{
		// Defines an .apply(id) method for accessing individual items
		val applyDec = MethodDeclaration("apply", Set(singleIdAccessRef),
			returnDescription = s"An access point to that ${ className.doc }")(
			Parameter("id", idType.toScala,
				description = s"Database id of the targeted ${ className.doc }"))(
			s"${ singleIdAccessRef.target }(id)")
		// Defines .filterDistinct(Condition) or .distinct(Condition) method for creating new unique access points
		// The implementation depends on whether this root access point defines any conditions
		val (distinctMethodName, distinctConditionParamName, distinctMethodImplementation) = {
			if (isDeprecatable)
				("filterDistinct", "additionalCondition", s"${uniqueAccessRef.target}(mergeCondition(additionalCondition))")
			else
				("distinct", "condition", s"${ uniqueAccessRef.target }(condition)")
		}
		val filterDec = MethodDeclaration(distinctMethodName, Set(uniqueAccessRef), Private,
			returnDescription = s"An access point to the ${className.doc} that satisfies the specified condition")(
			Parameter(distinctConditionParamName, condition,
				description = s"Filter condition to apply in addition to this root view's condition. Should yield unique ${
					className.pluralDoc}."))(
			distinctMethodImplementation)
		
		File(singleAccessPackage,
			ObjectDeclaration((accessPrefix +: className).className,
				Vector(singleModelAccessRef(modelRef), rootViewExtension, indexed),
				properties = baseProperties,
				// Defines an .apply(id) method for accessing individual items
				methods = Set(applyDec, filterDec),
				description = s"Used for accessing individual ${ className.pluralDoc }",
				author = author, since = DeclarationDate.versionedToday
			)
		).write()
	}
	
	// Writes all access points which access multiple items at a time
	// Returns Try[Option[ManyAccessLikeRef]]
	private def writeManyAccesses(classToWrite: Class, parentClassReferences: Seq[ClassReferences], modelRef: Reference,
	                              descriptionReferences: Option[(Reference, Reference, Reference)],
	                              modelProperty: PropertyDeclaration, factoryProperty: PropertyDeclaration,
	                              deprecationMethod: Option[MethodDeclaration])
	                             (implicit naming: NamingRules, codec: Codec, setup: VaultProjectSetup) =
	{
		val manyAccessPackage = manyAccessPackageFor(classToWrite)
		// Writes ManyXAccess and possibly ManyXAccessLike
		writeManyAccessTrait(classToWrite, parentClassReferences, modelRef, descriptionReferences, manyAccessPackage,
			modelProperty, factoryProperty, deprecationMethod)
			.flatMap { case (genericManyAccessTraitRef, manyAccessTraitRef) =>
				manyAccessTraitRef match {
					// Case: Concrete class => Writes DbXs (root access), also
					case Some(manyAccessTraitRef) =>
						writeManyRootAccess(classToWrite.name, modelRef, manyAccessTraitRef, descriptionReferences,
							manyAccessPackage, classToWrite.author, classToWrite.isDeprecatable, classToWrite.useLongId)
							// Returns only the most generic trait, since that's the only one being used later
							.map { _ => genericManyAccessTraitRef }
						
					// Case: Generic class => Won't generate the concrete root access point
					case None => Success(genericManyAccessTraitRef)
				}
			}
	}
	
	private def writeManyComboAccesses(combo: CombinationData, genericManyAccessRef: Reference, modelRef: Reference,
	                                   factoryRef: Reference, childDbModelRef: Reference)
	                                  (implicit codec: Codec, setup: VaultProjectSetup, naming: NamingRules) =
	{
		val packageName = manyAccessPackageFor(combo.parentClass)
		val traitName = manyAccessTraitNameFrom(combo.name).pluralClassName
		val traitType = ScalaType.basic(traitName)
		val extensions: Vector[Extension] = {
			val base = genericManyAccessRef(modelRef, traitType)
			if (combo.combinationType.isOneToMany)
				Vector(base)
			else
				Vector(base, manyRowModelAccess(modelRef))
		}
		val childModelProp = ComputedProperty((combo.childName + "Model").prop, Set(childDbModelRef),
			visibility = Protected,
			description = s"Model (factory) used for interacting the ${
				combo.childClass.name.pluralDoc } associated with this ${ combo.name.doc }")(childDbModelRef.target)
		
		val traitWriteResult = File(packageName,
			// Writes a private subAccess trait for filter(...) implementation
			accessCompanionObject(traitType),
			// Writes the common trait for all many combined access points
			TraitDeclaration(traitName,
				extensions = extensions,
				properties = Vector(
					self,
					ComputedProperty("factory", Set(factoryRef), isOverridden = true)(factoryRef.target),
					childModelProp
				) ++ propertyGettersFor(combo.childClass, childModelProp.name, pullMany = true,
					isCombo = true) { n => (combo.childName + n).props },
				methods = propertySettersFor(combo.childClass,
					childModelProp.name) { n => (combo.childName +: n).props } +
					filteringApply(traitType),
				description = s"A common trait for access points that return multiple ${ combo.name.pluralDoc } at a time",
				author = combo.author
			)
		).write()
		
		// For non-generic classes, writes the root access as well
		if (combo.parentClass.isGeneric)
			traitWriteResult
		else
			traitWriteResult.flatMap { traitRef =>
				// Next writes the root access point
				writeManyRootAccess(combo.name, modelRef, traitRef, None, packageName, combo.author,
					isDeprecatable = combo.isDeprecatable, useLongIds = combo.parentClass.useLongId)
			}
	}
	
	// Writes a trait common for the many model access points
	private def writeManyAccessTrait(classToWrite: Class, parentClassReferences: Seq[ClassReferences],
	                                 modelRef: Reference,
	                                 descriptionReferences: Option[(Reference, Reference, Reference)],
	                                 manyAccessPackage: Package, modelProperty: PropertyDeclaration,
	                                 factoryProperty: PropertyDeclaration,
	                                 deprecationMethod: Option[MethodDeclaration])
	                                (implicit naming: NamingRules, codec: Codec, setup: VaultProjectSetup) =
	{
		// Common part for all written trait names
		val traitNameBase = manyAccessTraitNameFrom(classToWrite.name)
		
		// Properties and methods that will be written to the highest trait (which may vary)
		val pullIds = {
			if (classToWrite.isExtension)
				None
			else {
				val idsPullCode = classToWrite.idType.fromValuesCode("pullColumn(index)")
				Some(ComputedProperty("ids", idsPullCode.references, implicitParams = Vector(connectionParam),
					description = s"Unique ids of the accessible ${ classToWrite.name.pluralDoc }")(
					idsPullCode.text))
			}
		}
		val highestTraitProperties = (modelProperty +:
			propertyGettersFor(classToWrite, pullMany = true) { _.props }) ++ pullIds
			
		val highestTraitMethods = propertySettersFor(classToWrite, modelProperty.name) { _.props } ++
			filterMethodsFor(classToWrite, modelProperty.name) ++ deprecationMethod
		
		// Deprecatable items inherit special parent traits
		val deprecatableViewParentRef = deprecationReferenceFor(classToWrite)
		
		// Writes the more generic trait version (-Like) first, if one is requested
		val parentRef = {
			if (classToWrite.writeGenericAccess || classToWrite.isGeneric) {
				// WET WET
				val item = GenericType.covariant("A",
					description = s"Type of read (${ classToWrite.name.pluralDoc } -like) instances")
				val itemType = item.toScalaType
				val repr = GenericType.covariant("Repr", description = "Type of this access point")
				val reprType = repr.toScalaType
				
				// The extended traits are different when inheritance is applied
				val extensions = {
					if (classToWrite.isExtension)
						parentClassReferences
							.flatMap { _.genericManyAccessTrait.map[Extension] { _(itemType, reprType) } } ++
							deprecatableViewParentRef.map[Extension] { _(reprType) }
					// Extends ManyModelAccess instead of ManyRowModel access because sub-traits may vary
					else
						Vector[Extension](manyModelAccess(itemType), indexed,
							deprecatableViewParentRef.getOrElse(filterableView)(reprType))
				}
				
				File(manyAccessPackage,
					TraitDeclaration(
						name = (traitNameBase + genericAccessSuffix).pluralClassName,
						genericTypes = Vector(item, repr),
						extensions = extensions,
						properties = highestTraitProperties,
						methods = highestTraitMethods,
						description = s"A common trait for access points which target multiple ${
							classToWrite.name.pluralDoc} or similar instances at a time",
						author = classToWrite.author, since = DeclarationDate.versionedToday))
					.write().map { Some(_) }
			}
			else
				Success(None)
		}
		
		// Writes the actual access trait, but only for concrete classes
		if (classToWrite.isGeneric)
			parentRef.map { _ -> None }
		else
			parentRef.flatMap { parentRef =>
				val traitName = traitNameBase.pluralClassName
				val traitType = ScalaType.basic(traitName)
				
				// Trait parent type depends on whether descriptions are used or not
				val (accessParent, inheritanceProperties, inheritanceMethods) = descriptionReferences match {
					// Case: Uses descriptions
					//       => Extends ManyDescribedAccess and implements the related properties and methods
					case Some((describedRef, _, manyDescsRef)) =>
						val parent = Extension(citadel.manyDescribedAccess(modelRef, describedRef))
						val props = Vector(
							ComputedProperty("manyDescriptionsAccess", Set(manyDescsRef), Protected,
								isOverridden = true)(manyDescsRef.target),
							ComputedProperty("describedFactory", Set(describedRef), Protected,
								isOverridden = true)(describedRef.target)
						)
						val methods = Set(MethodDeclaration("idOf", isOverridden = true)(
							Parameter("item", modelRef))("item.id"))
						(Some(parent), props, methods)
					
					// Case: No descriptions are used => Adds the indexed extension, if necessary
					case None =>
						val parent = if (parentRef.isDefined || classToWrite.isExtension) None else Some(Extension(indexed))
						(parent, Empty, Set[MethodDeclaration]())
				}
				// Determines the inheritance, which is affected by:
				//      1) Whether a generic version exists or not,
				//      2) Whether other traits are inherited (inheritance / extension use-case)
				//      3) Whether creation time is recorded, and
				//      4) Whether deprecation is used
				val parents: Seq[Extension] = {
					// Checks whether row creation time is tracked
					val creationTimeParent: Option[Extension] = {
						if (classToWrite.recordsIndexedCreationTime)
							Some(chronoRowFactoryView(modelRef, traitType))
						else
							None
					}
					val rowModelAccess: Extension = manyRowModelAccess(modelRef)
					val variableParents: Seq[Extension] = parentRef match {
						// Case: Extends ManyXAccessLike[...], which covers most of the access extensions
						case Some(parent) =>
							Vector[Extension](parent(modelRef, traitType), rowModelAccess) ++ creationTimeParent
						case None =>
							// Checks whether deprecation or filter view should be applied
							val extraParents: Iterable[Extension] =
								creationTimeParent ++ deprecatableViewParentRef.map { _(traitType) }
								
							// Case: Inherits other access traits instead
							if (classToWrite.isExtension)
								(parentClassReferences
									.flatMap { _.genericManyAccessTrait.map[Extension] { _(modelRef, traitType) } } ++
									extraParents) :+ rowModelAccess
							// Case: No special inheritances => Applies standard extensions
							else {
								val filterParents = {
									if (extraParents.isEmpty)
										Vector[Extension](filterableView(traitType))
									else
										extraParents.toVector
								}
								rowModelAccess +: filterParents
							}
					}
					variableParents ++ accessParent
				}
				
				File(manyAccessPackage,
					// The companion object contains a sub-view implementation
					accessCompanionObject(traitType),
					TraitDeclaration(traitName,
						extensions = parents,
						// Contains computed properties to access class properties
						properties = (factoryProperty +: inheritanceProperties) ++
							(if (parentRef.isDefined) Vector(self) else self +: highestTraitProperties),
						// Contains setters for property values (plural)
						methods = (if (parentRef.isDefined) Set[MethodDeclaration]() else highestTraitMethods) ++
							inheritanceMethods + filteringApply(traitType),
						description = s"A common trait for access points which target multiple ${
							classToWrite.name.pluralDoc } at a time",
						author = classToWrite.author, since = DeclarationDate.versionedToday
					)
				).write()
					// Returns both the more generic and the more concrete trait references
					.map { childRef => parentRef -> Some(childRef) }
			}
	}
	
	private def writeManyRootAccess(className: Name, modelRef: Reference, manyAccessTraitRef: Reference,
	                                descriptionReferences: Option[(Reference, Reference, Reference)],
	                                manyAccessPackage: Package, author: String, isDeprecatable: Boolean,
	                                useLongIds: Boolean)
	                               (implicit naming: NamingRules, codec: Codec, setup: VaultProjectSetup) =
	{
		val pluralClassName = className.pluralClassName
		// Writes the many model access point
		val manyAccessName = {
			if (className.className == pluralClassName)
				s"DbMany$pluralClassName"
			else
				s"Db$pluralClassName"
		}
		
		// There is also a nested object for id-based multi-access, which may have description support
		// These are only generated for classes which use descriptions.
		// Other implementations just use apply (which may be from an extension)
		val (customExtension, accessMethod, subSetClass) = descriptionReferences match {
			// Case: Described class => Generates a subset class
			case Some((describedRef, _, _)) =>
				val subSetClassName = s"Db${ pluralClassName }Subset"
				val subSetClass = ClassDeclaration(subSetClassName,
					constructionParams = Parameter("ids", ScalaType.set(ScalaType.int),
						prefix = Some(DeclarationStart.overrideVal)),
					extensions = Vector(manyAccessTraitRef,
						citadel.manyDescribedAccessByIds(modelRef, describedRef))
				)
				val subSetClassAccessMethod = MethodDeclaration("apply",
					returnDescription = s"Access to ${ className.pluralDoc } with the specified ids")(
					Parameter("ids", ScalaType.set(ScalaType.int),
						description = s"Ids of the targeted ${ className.pluralDoc }"))(
					s"new $subSetClassName(ids)")
				
				(None, Some(subSetClassAccessMethod), Some(subSetClass))
				
			case None =>
				// Case: Class with ids of type long => Generates an apply function
				if (useLongIds) {
					val subSetAccessMethod = MethodDeclaration("apply",
						explicitOutputType = Some(manyAccessTraitRef),
						returnDescription = s"Access to ${ className.pluralDoc } with the specified ids",
						isLowMergePriority = true)(
						Parameter("ids", ScalaType.iterable(ScalaType.long),
							description = s"Ids of the targeted ${ className.pluralDoc }"))(
						"apply(index in ids)")
					
					(None, Some(subSetAccessMethod), None)
				}
				// Case: Class with ids of type int => Extends ViewManyByIntIds[...]
				else {
					val extension: Extension = viewManyByIntIds(manyAccessTraitRef)
					(Some(extension), None, None)
				}
		}
		
		// For deprecating items, there is also a sub-object for accessing all items,
		// including those that were deprecated
		val history = {
			if (isDeprecatable)
				Some(historyAccessAndProperty(className, manyAccessTraitRef))
			else
				None
		}
		// Root access points extend either the UnconditionalView or the NonDeprecatedView -trait,
		// depending on whether deprecation is supported
		val rootViewExtension: Extension =
			if (isDeprecatable) nonDeprecatedView(modelRef) else unconditionalView
		File(manyAccessPackage,
			ObjectDeclaration(manyAccessName,
				extensions = Pair[Extension](manyAccessTraitRef, rootViewExtension) ++ customExtension,
				properties = history.map { _._2 }.toVector,
				methods = accessMethod.toSet,
				nested = Set(subSetClass, history.map { _._1 }).flatten,
				description = s"The root access point when targeting multiple ${ className.pluralDoc } at a time",
				author = author, since = DeclarationDate.versionedToday
			)
		).write()
	}
	
	private def subViewDeclaration(accessTraitType: ScalaType, subViewName: String) = {
		ClassDeclaration(subViewName,
			constructionParams =
				Parameter("accessCondition", ScalaType.option(condition), prefix = Some(DeclarationStart.overrideVal)),
			extensions = Vector(accessTraitType),
			visibility = Private,
			isCaseClass = true
		)
	}
	
	// Assumes that deprecation has been tested already
	private def historyAccessAndProperty(className: Name, accessTraitRef: Reference,
	                                     uniqueAccessRef: Option[Reference] = None)
	                                    (implicit naming: NamingRules) =
	{
		// For deprecating items, there is also a sub-object for accessing all items,
		// including those that were deprecated
		// TODO: Currently this is always None because this access point is not used in single access roots
		val distinctMethod = uniqueAccessRef.map { uniqueAccessRef =>
			MethodDeclaration("distinct", Set(uniqueAccessRef), visibility = Protected,
				description = s"Accesses a unique $className using a search condition")(
				Parameter("condition", condition,
					description = "A search condition to apply. Should yield unique results."))(
				s"${uniqueAccessRef.target}(condition)")
		}
		val historyAccess = ObjectDeclaration(
			name = s"Db${ className.pluralClassName }IncludingHistory",
			extensions = Vector(accessTraitRef, unconditionalView),
			methods = distinctMethod.toSet
		)
		val historyAccessProperty = ComputedProperty("includingHistory",
			description = s"A copy of this access point that includes historical (i.e. deprecated) ${ className.pluralDoc}")(
			historyAccess.name)
		historyAccess -> historyAccessProperty
	}
	
	private def propertyGettersFor(classToWrite: Class, modelPropName: String = "model", pullMany: Boolean = false,
	                               isCombo: Boolean = false)
	                              (parsePropName: Name => String)
	                              (implicit naming: NamingRules) =
	{
		// Will not redefine getters defined in parent traits
		val propertiesToWrite = {
			if (isCombo)
				classToWrite.properties
			else
				classToWrite.properties.filterNot { _.isDirectExtension }
		}
		propertiesToWrite.flatMap { prop =>
			// Rename-processing is not available in combo access points
			prop.rename.filterNot { _ => isCombo } match {
				// Case: Extending and renaming another property => Refers to the original implementation
				case Some((original, local)) =>
					Some(ComputedProperty(parsePropName(local),
						description = getterDescriptionFor(classToWrite.name, prop, prop.name, pullMany),
						implicitParams = Single(connectionParam))(
						parsePropName(original)))
				
				// Case: New property
				case None =>
					// Only single-column properties are pulled
					prop.onlyDbVariant.map { dbProp =>
						val pullColumn = s"pullColumn($modelPropName.${ dbProp.name.prop }.column)"
						val pullCode = {
							if (pullMany)
								prop.dataType.fromValuesCode(pullColumn)
							else
								prop.dataType.optional.fromValueCode(Single(pullColumn))
						}
						
						ComputedProperty(parsePropName(dbProp.name), pullCode.references,
							description = getterDescriptionFor(classToWrite.name, prop, dbProp.name, pullMany),
							implicitParams = Single(connectionParam))(pullCode.text)
					}
			}
		}
	}
	private def propertySettersFor(classToWrite: Class, modelPropName: String = "model")
	                              (methodNameFromPropName: Name => String)
	                              (implicit setup: VaultProjectSetup, naming: NamingRules) =
	{
		// TODO: Because of a technical limitation where accepted parameter type is not available, only single-column
		//  properties are written
		// Won't write extended properties
		classToWrite.properties.filter { p => p.isMutable && !p.isDirectExtension }.map { _.concrete }.flatMap { prop =>
			prop.onlyDbVariant.map { dbProp =>
				setter(prop, dbProp, classToWrite.name, modelPropName)(methodNameFromPropName)
			}
			// prop.dbProperties.map { dbProp => setter(prop, dbProp, classToWrite.name)(methodNameFromPropName) }
		}.toSet
	}
	
	private def setter(prop: Property, dbProp: DbProperty, className: Name, modelPropName: String)
	                  (methodNameFromPropName: Name => String)
	                  (implicit naming: NamingRules) =
	{
		val paramName = (newPrefix +: dbProp.name).prop
		val paramType = if (prop.isSingleColumn) prop.dataType.toScala else dbProp.conversion.origin
		
		val methodName = s"${ methodNameFromPropName(dbProp.name) }_="
		val description = s"Updates the ${ prop.name.pluralDoc } of the targeted ${ className.pluralDoc }"
		val returnDescription = s"Whether any ${className.doc} was affected"
		
		val param = Parameter(paramName, paramType, description = s"A new ${ dbProp.name.doc } to assign")
			.withImplicits(connectionParam)
		
		// When renaming a property in the parent trait, utilizes the parent's setter
		prop.rename match {
			// Case: Rename
			case Some((original, _)) =>
				MethodDeclaration(methodName, description = description, returnDescription = returnDescription)(
					param)(s"${ original.prop } = $paramName")
			
			// Case: Regular implementation
			case None =>
				val valueConversionCode = {
					if (prop.isSingleColumn)
						prop.dataType.toValueCode(paramName)
					else {
						val midConversion = dbProp.conversion.midConversion(paramName)
						dbProp.conversion.intermediate.toValueCode(midConversion.text)
							.referringTo(midConversion.references)
					}
				}
				MethodDeclaration(methodName, valueConversionCode.references,
					description = description, returnDescription = returnDescription)(
					param)(
					s"putColumn($modelPropName.${ dbProp.name.prop }.column, $valueConversionCode)")
		}
	}
	
	private def deprecationReferenceFor(classToWrite: Class) = {
		if (classToWrite.isNullDeprecatable)
			Some(nullDeprecatableView)
		else if (classToWrite.isExpiring)
			Some(timeDeprecatableView)
		else
			None
	}
	
	private def filterMethodsFor(classToWrite: Class, modelPropName: String)(implicit naming: NamingRules) =
	{
		classToWrite.properties.flatMap { prop =>
			// Only writes the with and in methods for properties with a single column
			prop.dbProperties.only.iterator.flatMap { dbProp =>
				// If these methods are not explicitly defined, only writes them for custom index properties
				lazy val isCustomIndex = dbProp.overrides.indexing.isCertainlyTrue ||
					classToWrite.comboIndexColumnNames.exists { _.contains(dbProp.columnName) }
				val withMethodName = prop.withAccessName.nonEmptyOrElse {
					if (isCustomIndex) ("with" +: prop.name).function else ""
				}
				val inMethodName = prop.inAccessName.nonEmptyOrElse {
					prop.withAccessName.ifNotEmpty match {
						// Case: "With" defined but "in" not defined => Generates "in" by pluralizing "with"
						case Some(withAccessName) =>
							val namingStyle = naming(FunctionName)
							Name.interpret(withAccessName, namingStyle).pluralIn(namingStyle)
						case None =>
							if (isCustomIndex)
								(Name("with", "with", CamelCase.lower) + prop.name).pluralIn(naming(FunctionName))
							else
								""
					}
				}
				
				// Writes the actual methods, if needed
				lazy val singleParamName = prop.name.prop
				lazy val concreteType = prop.dataType.concrete
				lazy val singleValueCode = concreteType.toValueCode(singleParamName)
				val withMethod = withMethodName.notEmpty
					// Will not redefine methods already introduced in the parent traits
					.filterNot { name => prop.parents.exists { _.withAccessName == name } }
					.map { name =>
						MethodDeclaration(name, singleValueCode.references,
							returnDescription = s"Copy of this access point that only includes ${
								classToWrite.name.pluralDoc } with the specified ${ prop.name }",
							isLowMergePriority = true)(
							Parameter(singleParamName, concreteType.toScala,
								description = s"${ prop.name } to target"))(
							s"filter($modelPropName.${ dbProp.name.prop }.column <=> ${singleValueCode.text})")
					}
				val inMethod = inMethodName.notEmpty
					// Here, also, won't redefine methods
					.filterNot { name => prop.parents.exists { _.inAccessName == name } }
					.map { name =>
						val paramsName = prop.name.pluralIn(naming(ClassPropName))
						val (code, inputCollectionName) = {
							def usingIntSet =
								CodePiece(s"IntSet.from($paramsName)", Set(flow.intSet)) -> "IterableOnce"
							val (valuesCode, inputCollectionName) = concreteType match {
								// Case: The parameter values are of type Int => Uses IntSet from IterableOnce[Int]
								case _: IntNumber => usingIntSet
								case c: ClassReference if c.referencedType.isInstanceOf[IntNumber] => usingIntSet
								case _ =>
									val implementation = singleValueCode.mapText { valueCode =>
										if (valueCode == singleParamName)
											paramsName
										else
											s"$paramsName.map { $singleParamName => $valueCode }"
									}
									implementation -> "Iterable"
							}
							val code = valuesCode.mapText { values =>
								s"filter($modelPropName.${ dbProp.name.prop }.column.in($values))"
							}
							code -> inputCollectionName
						}
						MethodDeclaration(name, code.references,
							returnDescription = s"Copy of this access point that only includes ${
								classToWrite.name.pluralDoc } where ${ prop.name } is within the specified value set",
							isLowMergePriority = true)(
							Parameter(paramsName, ScalaType.generic(inputCollectionName, concreteType.toScala),
								description = s"Targeted ${ prop.name.pluralDoc }"))(code.text)
					}
				
				Pair(withMethod, inMethod).flatten
			}
		}
	}
	
	/**
	 * @param className A class name
	 * @return Name of the single id access point for that class
	 */
	private def singleIdAccessNameFor(className: Name)(implicit naming: NamingRules) =
		(singleAccessPrefix +: className).className
	private def uniqueAccessTraitNameFrom(className: Name) = (uniqueAccessPrefix +: className) + accessTraitSuffix
	/**
	 * Converts a class name into a ManyXAccess trait name
	 * @param className Name of the X class
	 * @param naming Naming rules to apply
	 * @return A trait name for ManyXAccess traits
	 */
	private def manyAccessTraitNameFrom(className: Name)(implicit naming: NamingRules) = {
		// The "Many" -prefix is ignored if the class name already starts with "Many"
		if (className.pluralClassName.toLowerCase.startsWith("many"))
			className + accessTraitSuffix
		else
			(manyPrefix +: className) + accessTraitSuffix
	}
	
	private def packageFor(accessPackage: Package, c: Class) = {
		// Won't include the general package name part twice
		val pckName = c.packageName
		val base = accessPackage/pckName
		c.customAccessSubPackageName.notEmpty match {
			case Some(custom) =>
				// Case: "-" defined as a custom package name => Indicates that no sub-packaging should be used
				if (custom == "-")
					base
				else
					base/custom
			case None =>
				val end = {
					val full = c.name.singularIn(UnderScore).split('_').toVector
					(full.findIndexWhere { _ ~== pckName } match {
						case Some(idx) => full.drop(idx + 1)
						case None => full
					}).mkString("_")
				}
				if (end.isEmpty)
					base
				else
					base / end
		}
	}
	
	/**
	 * @param c     A class
	 * @param setup Project setup (implicit)
	 * @return Package that contains singular access points for that class
	 */
	private def singleAccessPackageFor(c: Class)(implicit setup: VaultProjectSetup) =
		packageFor(setup.singleAccessPackage, c)
	private def manyAccessPackageFor(c: Class)(implicit setup: VaultProjectSetup) = packageFor(setup.manyAccessPackage, c)
	
	private def getterDescriptionFor(className: Name, prop: Property, propName: Name, pullMany: Boolean)
	                                (implicit naming: NamingRules) =
	{
		if (pullMany)
			s"${ propName.pluralDoc } of the accessible ${ className.pluralDoc }"
		else {
			val basePart = prop.description.notEmpty match {
				case Some(desc) => desc.notEndingWith(".")
				case None => s"The ${ propName.doc } of this ${ className.doc }"
			}
			s"$basePart. \nNone if no ${ className.doc } (or value) was found."
		}
	}
}
