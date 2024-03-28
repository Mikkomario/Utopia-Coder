package utopia.coder.vault.controller.writer.database

import utopia.coder.model.data
import utopia.coder.model.data.{Name, NamingRules}
import utopia.flow.collection.CollectionExtensions._
import utopia.flow.collection.immutable.Pair
import utopia.flow.operator.equality.EqualsExtensions._
import utopia.flow.util.StringExtensions._
import utopia.coder.vault.model.data.{Class, CombinationData, DbProperty, Property, VaultProjectSetup}
import utopia.coder.vault.model.datatype.PropertyType
import utopia.coder.model.enumeration.NamingConvention.{CamelCase, UnderScore}
import utopia.coder.model.scala.Visibility.{Private, Protected}
import utopia.coder.model.scala.datatype.{Extension, GenericType, Reference, ScalaType}
import utopia.coder.model.scala.declaration.PropertyDeclarationType.{ComputedProperty, ImmutableValue, LazyValue}
import utopia.coder.model.scala.declaration._
import utopia.coder.model.scala.{DeclarationDate, Package, Parameter, Parameters}
import Reference._
import utopia.coder.model.enumeration.NameContext.{ClassPropName, FunctionName}
import utopia.coder.vault.util.VaultReferences._
import utopia.coder.vault.util.VaultReferences.Vault._

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
	private val subViewSuffix = data.Name("SubView", "SubView", CamelCase.capitalized)
	private val uniqueAccessPrefix = data.Name("Unique", "Unique", CamelCase.capitalized)
	
	private val newPrefix = data.Name("new", "new", CamelCase.lower)
	
	private lazy val connectionParam = Parameter("connection", connection)
	
	private lazy val self = ComputedProperty("self", visibility = Protected, isOverridden = true)("this")
	
	
	// OTHER    ------------------------------
	
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
	  * @param modelRef              Reference to the stored model class
	  * @param factoryRef            Reference to the from DB factory object
	  * @param dbModelRef            Reference to the database model class
	  * @param descriptionReferences References to the described model version + single description link access point +
	  *                              many description links access point, if applicable for this class
	  * @param codec                 Implicit codec to use when writing files
	  * @param setup                 Implicit project-specific setup to use
	  * @return References to the UniqueXAccessLike and ManyXAccessLike -traits
	  *         (i.e. the most generic access point for unique and multiple items), if these were generated
	  */
	def apply(classToWrite: Class, modelRef: Reference, factoryRef: Reference, dbModelRef: Reference,
	          descriptionReferences: Option[(Reference, Reference, Reference)])
	         (implicit codec: Codec, setup: VaultProjectSetup, naming: NamingRules) =
	{
		// Standard access point properties (factory, model)
		// are present in both single and many model access points
		val factoryProperty = ComputedProperty("factory", Set(factoryRef), isOverridden = true)(factoryRef.target)
		val modelProperty = ComputedProperty("model", Set(dbModelRef), Protected,
			description = "Factory used for constructing the database-interaction models")(dbModelRef.target)
		// For classes that support expiration, deprecate() -method is added for all traits
		// Null-deprecation -supporting classes already inherit this method
		// Option[Pair[method]], where first method is for individual access and second for many access
		val deprecationMethods = classToWrite.expirationProperty.map { prop =>
			Pair(prop.name.prop, prop.name.props).map { propName =>
				MethodDeclaration("deprecate", Set(flow.now, flow.valueConversions),
					description = s"Deprecates all accessible ${classToWrite.name.pluralDoc}",
					returnDescription = "Whether any row was targeted")(
					Parameters(Vector(Vector()), Vector(connectionParam)))(s"$propName = Now")
			}
		}
		// Root access points extend either the UnconditionalView or the NonDeprecatedView -trait,
		// depending on whether deprecation is supported
		val rootViewExtension: Extension = {
			if (classToWrite.isDeprecatable) nonDeprecatedView(modelRef) else unconditionalView
		}
		
		writeSingleAccesses(classToWrite, modelRef, descriptionReferences, modelProperty, factoryProperty,
			deprecationMethods.map { _.first }, rootViewExtension)
			.flatMap { case (_, uniqueAccessLikeRef) =>
				writeManyAccesses(classToWrite, modelRef, descriptionReferences, modelProperty, factoryProperty,
					deprecationMethods.map { _.second })
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
	  * @return Reference to the many combo items root access point
	  */
	def writeComboAccessPoints(combo: CombinationData, genericUniqueAccessRef: Reference,
	                           genericManyAccessRef: Reference, modelRef: Reference,
	                           factoryRef: Reference, parentDbModelRef: Reference, childDbModelRef: Reference)
	                          (implicit codec: Codec, setup: VaultProjectSetup, naming: NamingRules) =
	{
		// Writes many & single accesses
		writeManyComboAccesses(combo, genericManyAccessRef, modelRef, factoryRef, childDbModelRef)
			.flatMap { manyRootRef =>
				writeSingleComboAccesses(combo, genericUniqueAccessRef, modelRef, factoryRef,
					parentDbModelRef, childDbModelRef).map { _ -> manyRootRef }
			}
	}
	
	// Writes all single item access points
	// Returns Try[(SingleRootAccessRef, UniqueAccessLikeRef)]
	private def writeSingleAccesses(classToWrite: Class, modelRef: Reference,
	                                descriptionReferences: Option[(Reference, Reference, Reference)],
	                                modelProperty: PropertyDeclaration, factoryProperty: PropertyDeclaration,
	                                deprecationMethod: Option[MethodDeclaration], rootViewExtension: Extension)
	                               (implicit naming: NamingRules, codec: Codec, setup: VaultProjectSetup) =
	{
		val singleAccessPackage = singleAccessPackageFor(classToWrite)
		writeUniqueAccess(classToWrite, modelRef, singleAccessPackage, modelProperty, factoryProperty, deprecationMethod)
			.flatMap { case (genericUniqueTraitRef, uniqueAccessRef) =>
				writeSingleIdAccess(classToWrite, modelRef, uniqueAccessRef, descriptionReferences, singleAccessPackage)
					.flatMap { singleIdAccessRef =>
						writeSingleRootAccess(classToWrite.name, classToWrite.idType, modelRef,
							singleRowModelAccess, uniqueAccessRef, singleIdAccessRef, singleAccessPackage,
							Vector(modelProperty, factoryProperty), rootViewExtension, classToWrite.author)
							.map { _ -> genericUniqueTraitRef }
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
		val subViewName = s"_$uniqueAccessName"
		val uniqueTraitParents: Vector[Extension] = {
			val base = genericUniqueAccessTraitRef(combinedModelRef, uniqueAccessType)
			val deprecationParent = deprecationReferenceFor(combo.parentClass).map[Extension] { _(uniqueAccessType) }
			if (combo.combinationType.isOneToMany)
				Vector(base)
			else {
				if (combo.parentClass.recordsIndexedCreationTime)
					Vector[Extension](base, singleChronoRowModelAccess(combinedModelRef, uniqueAccessType)) ++
						deprecationParent
				else
					Vector[Extension](base, singleRowModelAccess(combinedModelRef)) ++ deprecationParent
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
				Vector()
			else
				propertyGettersFor(combo.childClass, childModelProp.name) { n => (combo.childName + n).prop }
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
		File(singleAccessPackage,
			accessCompanionObject(uniqueAccessType, subViewName),
			TraitDeclaration(uniqueAccessName,
				extensions = uniqueTraitParents,
				properties = Vector(self, factoryProp, childModelProp) ++ childGetters,
				methods = childSetters + filterMethod(uniqueAccessType),
				description = s"A common trait for access points that return distinct ${ combo.name.pluralDoc }",
				author = combo.author, since = DeclarationDate.versionedToday
			)
		).write().flatMap { uniqueRef =>
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
					Extension(singleIntIdModelAccess(combinedModelRef)) -> Vector()
			}
			File(singleAccessPackage,
				ClassDeclaration(singleIdAccessNameFor(combo.name),
					constructionParams = Vector(Parameter("id", idType.toScala)),
					extensions = Vector(uniqueRef, idAccessParentRef),
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
						ComputedProperty("model", Set(parentDbModelRef), visibility = Protected,
							description = s"A database model (factory) used for interacting with linked ${
								combo.parentName.pluralDoc }")(parentDbModelRef.target),
						childModelProp),
					rootParent, combo.author)
			}
		}
	}
	
	// Returns the more generic reference and then less generic reference
	private def writeUniqueAccess(classToWrite: Class, modelRef: Reference, singleAccessPackage: Package,
	                              modelProperty: PropertyDeclaration,
	                              factoryProperty: PropertyDeclaration, deprecationMethod: Option[MethodDeclaration])
	                             (implicit naming: NamingRules, codec: Codec, setup: VaultProjectSetup) =
	{
		// TODO: WET WET (much of similar code at writeManyAccessTrait)
		val uniqueAccessTraitName = uniqueAccessTraitNameFrom(classToWrite.name)
		
		// Properties and methods that will be written to the highest trait (which may vary)
		// These are: .model, .id and various property getters and setters
		val pullIdCode = classToWrite.idType.optional.fromValueCode(s"pullColumn(index)")
		val highestTraitProperties = modelProperty +:
			propertyGettersFor(classToWrite, modelProperty.name) { _.prop } :+
			ComputedProperty("id", pullIdCode.references, implicitParams = Vector(connectionParam),
				description = s"Unique id of the accessible ${ classToWrite.name }. None if no ${
					classToWrite.name } was accessible.")(pullIdCode.text)
		val highestTraitMethods = propertySettersFor(classToWrite, modelProperty.name) { _.prop } ++ deprecationMethod
		
		// Writes the more generic trait version (-Like) first, if one is requested
		val parentRef = {
			if (classToWrite.writeGenericAccess) {
				val item = GenericType.covariant("A")
				val itemType = item.toScalaType
				val repr = GenericType.covariant("Repr")
				File(singleAccessPackage,
					TraitDeclaration((uniqueAccessTraitName + genericAccessSuffix).className,
						Vector(item, repr),
						// Extends SingleModelAccess instead of SingleRowModelAccess because sub-traits may vary
						Vector(
							singleModelAccess(itemType),
							distinctModelAccess(itemType, ScalaType.option(itemType), flow.value),
							filterableView(repr.toScalaType),
							indexed
						),
						highestTraitProperties, highestTraitMethods,
						description = s"A common trait for access points which target individual ${
							classToWrite.name.pluralDoc } or similar items at a time",
						author = classToWrite.author, since = DeclarationDate.versionedToday
					)
				).write().map { Some(_) }
			}
			else
				Success(None)
		}
		
		// Writes the actual access trait
		parentRef.flatMap { parentRef =>
			val traitNameString = uniqueAccessTraitName.className
			val traitType = ScalaType.basic(traitNameString)
			val subViewName = s"_$traitNameString"
			// The parent types depend from 3 factors:
			// 1) Whether generic type is used,
			// 2) Whether row creation time is recorded, and
			// 3) Whether deprecation is used
			val deprecationParentRef = deprecationReferenceFor(classToWrite)
			val rowAccessParent: Extension = {
				if (classToWrite.recordsIndexedCreationTime)
					singleChronoRowModelAccess(modelRef, traitType)
				else
					singleRowModelAccess(modelRef)
			}
			val parents: Vector[Extension] = parentRef match {
				case Some(genericParent) =>
					Vector[Extension](genericParent(modelRef, traitType), rowAccessParent) ++
						deprecationParentRef.map { _(traitType) }
				case None =>
					Vector[Extension](
						rowAccessParent,
						distinctModelAccess(modelRef, ScalaType.option(modelRef), flow.value),
						deprecationParentRef.getOrElse(filterableView)(traitType),
						indexed
					)
			}
			val baseProperties = Vector(self, factoryProperty)
			val properties = if (parentRef.isDefined) baseProperties else baseProperties ++ highestTraitProperties
			val filterM = filterMethod(traitType)
			val methods = if (parentRef.isDefined) Set(filterM) else highestTraitMethods + filterM
			
			File(singleAccessPackage,
				accessCompanionObject(traitType, subViewName),
				TraitDeclaration(traitNameString,
					extensions = parents, properties = properties, methods = methods,
					description = s"A common trait for access points that return individual and distinct ${
						classToWrite.name.pluralDoc
					}.",
					author = classToWrite.author,
					since = DeclarationDate.versionedToday
				)
			).write().map { parentRef -> _ }
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
					Extension(singleIntIdModelAccess(modelRef)) -> Vector()
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
	                                  singleAccessPackage: Package, baseProperties: Vector[PropertyDeclaration],
	                                  rootViewExtension: Extension, author: String)
	                                 (implicit naming: NamingRules, codec: Codec, setup: VaultProjectSetup) =
	{
		// Defines an .apply(id) method for accessing individual items
		val applyDec = MethodDeclaration("apply", Set(singleIdAccessRef),
			returnDescription = s"An access point to that ${ className.doc }")(
			Parameter("id", idType.toScala,
				description = s"Database id of the targeted ${ className.doc }"))(
			s"${ singleIdAccessRef.target }(id)")
		// Defines .filterUnique(Condition) method for creating new unique access points
		val filterDec = MethodDeclaration("filterDistinct", Set(uniqueAccessRef), Protected,
			returnDescription = s"An access point to the ${className.doc} that satisfies the specified condition")(
			Parameter("condition", condition,
				description = s"Filter condition to apply in addition to this root view's condition. Should yield unique ${className.pluralDoc}."))(
			s"${uniqueAccessRef.target}(mergeCondition(condition))")
		
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
	// Returns Try[ManyAccessLikeRef]
	private def writeManyAccesses(classToWrite: Class, modelRef: Reference,
	                              descriptionReferences: Option[(Reference, Reference, Reference)],
	                              modelProperty: PropertyDeclaration, factoryProperty: PropertyDeclaration,
	                              deprecationMethod: Option[MethodDeclaration])
	                             (implicit naming: NamingRules, codec: Codec, setup: VaultProjectSetup) =
	{
		val manyAccessPackage = manyAccessPackageFor(classToWrite)
		writeManyAccessTrait(classToWrite, modelRef, descriptionReferences, manyAccessPackage, modelProperty,
			factoryProperty, deprecationMethod)
			.flatMap { case (genericManyAccessTraitRef, manyAccessTraitRef) =>
				writeManyRootAccess(classToWrite.name, modelRef, manyAccessTraitRef, descriptionReferences,
					manyAccessPackage, classToWrite.author, classToWrite.isDeprecatable)
					// Returns only the most generic trait, since that's the only one being used later
					.map { _ => genericManyAccessTraitRef }
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
		File(packageName,
			// Writes a private subAccess trait for filter(...) implementation
			accessCompanionObject(traitType, "SubAccess"),
			// Writes the common trait for all many combined access points
			TraitDeclaration(traitName,
				extensions = extensions,
				properties = Vector(
					self,
					ComputedProperty("factory", Set(factoryRef), isOverridden = true)(factoryRef.target),
					childModelProp
				) ++ propertyGettersFor(combo.childClass, childModelProp.name,
					pullMany = true) { n => (combo.childName + n).props },
				methods = propertySettersFor(combo.childClass,
					childModelProp.name) { n => (combo.childName +: n).props } +
					filterMethod(traitType),
				description = s"A common trait for access points that return multiple ${ combo.name.pluralDoc } at a time",
				author = combo.author
			)
		).write().flatMap { traitRef =>
			// Next writes the root access point
			writeManyRootAccess(combo.name, modelRef, traitRef, None, packageName, combo.author,
				isDeprecatable = combo.isDeprecatable)
		}
	}
	
	// Writes a trait common for the many model access points
	private def writeManyAccessTrait(classToWrite: Class, modelRef: Reference,
	                                 descriptionReferences: Option[(Reference, Reference, Reference)],
	                                 manyAccessPackage: Package, modelProperty: PropertyDeclaration,
	                                 factoryProperty: PropertyDeclaration,
	                                 deprecationMethod: Option[MethodDeclaration])
	                                (implicit naming: NamingRules, codec: Codec, setup: VaultProjectSetup) =
	{
		// Common part for all written trait names
		val traitNameBase = manyAccessTraitNameFrom(classToWrite.name)
		
		// Properties and methods that will be written to the highest trait (which may vary)
		val idsPullCode = classToWrite.idType.fromValuesCode("pullColumn(index)")
		val highestTraitProperties = modelProperty +:
			propertyGettersFor(classToWrite, pullMany = true) { _.props } :+
			ComputedProperty("ids", idsPullCode.references, implicitParams = Vector(connectionParam),
				description = s"Unique ids of the accessible ${ classToWrite.name.pluralDoc }")(
				idsPullCode.text)
		val highestTraitMethods = propertySettersFor(classToWrite, modelProperty.name) { _.props } ++
			filterMethodsFor(classToWrite, modelProperty.name) ++ deprecationMethod
		
		// Deprecatable items inherit special parent traits
		val deprecatableViewParentRef = deprecationReferenceFor(classToWrite)
		
		// Writes the more generic trait version (-Like) first, if one is requested
		val parentRef = {
			if (classToWrite.writeGenericAccess) {
				val item = GenericType.covariant("A")
				val repr = GenericType.covariant("Repr")
				
				File(manyAccessPackage,
					TraitDeclaration(
						(traitNameBase + genericAccessSuffix).pluralClassName, Vector(item, repr),
						// Extends ManyModelAccess instead of ManyRowModel access because sub-traits may vary
						Vector(manyModelAccess(item.toScalaType), indexed,
							deprecatableViewParentRef.getOrElse(filterableView)(repr.toScalaType)),
						highestTraitProperties, highestTraitMethods,
						description = s"A common trait for access points which target multiple ${
							classToWrite.name.pluralDoc} or similar instances at a time",
						author = classToWrite.author, since = DeclarationDate.versionedToday))
					.write().map { Some(_) }
			}
			else
				Success(None)
		}
		
		// Writes the actual access trait
		parentRef.flatMap { parentRef =>
			val traitName = traitNameBase.pluralClassName
			val traitType = ScalaType.basic(traitName)
			val subViewName = ((manyPrefix +: classToWrite.name) + subViewSuffix).pluralClassName
			
			// Trait parent type depends on whether descriptions are used or not
			val (accessParent, inheritanceProperties, inheritanceMethods) = descriptionReferences match {
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
				case None =>
					val parent = if (parentRef.isDefined) None else Some(Extension(indexed))
					(parent, Vector(), Set[MethodDeclaration]())
			}
			// Determines the inheritance, which is affected by:
			// 1) Whether a generic version exists or not,
			// 2) Whether creation time is recorded, and
			// 3) Whether deprecation is used
			val parents: Vector[Extension] = {
				// Checks whether row creation time is tracked
				val creationTimeParent: Option[Extension] = {
					if (classToWrite.recordsIndexedCreationTime)
						Some(chronoRowFactoryView(modelRef, traitType))
					else
						None
				}
				val rowModelAccess = manyRowModelAccess(modelRef)
				val variableParents: Vector[Extension] = parentRef match {
					case Some(parent) =>
						Vector[Extension](parent(modelRef, traitType), rowModelAccess) ++ creationTimeParent
					case None =>
						// Checks whether deprecation or filter view should be applied
						val extraParents: Iterable[Extension] =
							creationTimeParent ++ deprecatableViewParentRef.map { _(traitType) }
						val filterParents = {
							if (extraParents.isEmpty)
								Vector[Extension](filterableView(traitType))
							else
								extraParents.toVector
						}
						rowModelAccess +: filterParents
				}
				variableParents ++ accessParent
			}
			
			File(manyAccessPackage,
				// The companion object contains a sub-view implementation
				accessCompanionObject(traitType, subViewName),
				TraitDeclaration(traitName,
					extensions = parents,
					// Contains computed properties to access class properties
					properties = (factoryProperty +: inheritanceProperties) ++
						(if (parentRef.isDefined) Vector(self) else self +: highestTraitProperties),
					// Contains setters for property values (plural)
					methods = (if (parentRef.isDefined) Set[MethodDeclaration]() else highestTraitMethods) ++
						inheritanceMethods + filterMethod(traitType),
					description = s"A common trait for access points which target multiple ${
						classToWrite.name.pluralDoc } at a time",
					author = classToWrite.author, since = DeclarationDate.versionedToday
				)
			).write()
				// Returns both the more generic and the more concrete trait references
				.map { parentRef -> _ }
		}
	}
	
	private def writeManyRootAccess(className: Name, modelRef: Reference, manyAccessTraitRef: Reference,
	                                descriptionReferences: Option[(Reference, Reference, Reference)],
	                                manyAccessPackage: Package, author: String, isDeprecatable: Boolean)
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
		val subsetClassName = s"Db${ pluralClassName }Subset"
		val subSetClass = descriptionReferences match {
			case Some((describedRef, _, _)) =>
				ClassDeclaration(subsetClassName,
					constructionParams = Parameter("ids", ScalaType.set(ScalaType.int),
						prefix = Some(DeclarationStart.overrideVal)),
					extensions = Vector(manyAccessTraitRef,
						citadel.manyDescribedAccessByIds(modelRef, describedRef))
				)
			case None =>
				ClassDeclaration(subsetClassName,
					constructionParams = Parameter("targetIds", ScalaType.iterable(ScalaType.int)),
					extensions = Vector(manyAccessTraitRef),
					properties = Vector(ImmutableValue("accessCondition",
						Set(flow.valueConversions), isOverridden = true)("Some(index in targetIds)"))
				)
		}
		val subSetClassAccessMethod = MethodDeclaration("apply",
			returnDescription = s"An access point to ${ className.pluralDoc } with the specified ids")(
			Parameter("ids",
				if (descriptionReferences.isDefined) ScalaType.set(ScalaType.int) else ScalaType.iterable(ScalaType.int),
				description = s"Ids of the targeted ${ className.pluralDoc }"))(
			s"new $subsetClassName(ids)")
		
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
			ObjectDeclaration(manyAccessName, Vector(manyAccessTraitRef, rootViewExtension),
				properties = history.map { _._2 }.toVector,
				methods = Set(subSetClassAccessMethod),
				nested = Set[InstanceDeclaration](subSetClass) ++ history.map { _._1 },
				description = s"The root access point when targeting multiple ${
					className.pluralDoc } at a time",
				author = author, since = DeclarationDate.versionedToday
			)
		).write()
	}
	
	private def accessCompanionObject(accessTraitType: ScalaType, subViewName: String) = {
		ObjectDeclaration(
			name = accessTraitType.toString,
			methods = Set(
				MethodDeclaration("apply", explicitOutputType = Some(accessTraitType),
					returnDescription = "An access point that applies the specified filter condition (only)")(
					Parameter("condition", condition, description = "Condition to apply to all requests"))(
					s"new $subViewName(condition)")
			),
			nested = Set(subViewDeclaration(accessTraitType, subViewName))
		)
	}
	
	private def subViewDeclaration(accessTraitType: ScalaType, subViewName: String) = {
		ClassDeclaration(subViewName,
			constructionParams = Parameter("condition", condition),
			extensions = Vector(accessTraitType),
			visibility = Private,
			properties = Vector(ComputedProperty("accessCondition", isOverridden = true)("Some(condition)"))
		)
	}
	
	// Assumes that the companion object contains an apply method
	private def filterMethod(traitType: ScalaType) =
		MethodDeclaration("filter", explicitOutputType = Some(traitType), isOverridden = true)(
			Parameter("filterCondition", condition))(s"$traitType(mergeCondition(filterCondition))")
	
	// Assumes that deprecation has been tested already
	private def historyAccessAndProperty(className: Name, accessTraitRef: Reference)(implicit naming: NamingRules) = {
		// For deprecating items, there is also a sub-object for accessing all items,
		// including those that were deprecated
		val historyAccess = ObjectDeclaration(
					s"Db${ className.pluralClassName }IncludingHistory",
					Vector(accessTraitRef, unconditionalView)
				)
		val historyAccessProperty = ComputedProperty("includingHistory",
			description = s"A copy of this access point that includes historical (i.e. deprecated) ${ className.pluralDoc}")(
			historyAccess.name)
		historyAccess -> historyAccessProperty
	}
	
	private def propertyGettersFor(classToWrite: Class, modelPropName: String = "model",
	                               pullMany: Boolean = false)(parsePropName: Name => String)
	                              (implicit naming: NamingRules) =
	{
		classToWrite.properties.flatMap { prop =>
			// Only single-column properties are pulled
			prop.onlyDbVariant.map { dbProp =>
				val pullColumn = s"pullColumn($modelPropName.${ dbProp.name.prop }.column)"
				val pullCode = {
					if (pullMany)
						prop.dataType.fromValuesCode(pullColumn)
					else
						prop.dataType.optional.fromValueCode(Vector(pullColumn))
				}
				val desc = {
					if (pullMany)
						s"${ prop.name.pluralDoc } of the accessible ${ classToWrite.name.pluralDoc }"
					else {
						val basePart = prop.description.notEmpty match {
							case Some(desc) => desc.notEndingWith(".")
							case None => s"The ${ dbProp.name.doc } of this ${ classToWrite.name.doc }"
						}
						s"$basePart. \nNone if no ${ classToWrite.name.doc } (or value) was found."
					}
				}
				ComputedProperty(parsePropName(dbProp.name), pullCode.references, description = desc,
					implicitParams = Vector(connectionParam))(pullCode.text)
			}
		}
	}
	private def propertySettersFor(classToWrite: Class, modelPropName: String = "model")
	                              (methodNameFromPropName: Name => String)
	                              (implicit naming: NamingRules) =
	{
		// TODO: Because of a technical limitation where accepted parameter type is not available, only single-column
		//  properties are written
		classToWrite.properties.map { _.concrete }.flatMap { prop =>
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
		val valueConversionCode = {
			if (prop.isSingleColumn)
				prop.dataType.toValueCode(paramName)
			else {
				val midConversion = dbProp.conversion.midConversion(paramName)
				dbProp.conversion.intermediate.toValueCode(midConversion.text).referringTo(midConversion.references)
			}
		}
		MethodDeclaration(s"${ methodNameFromPropName(dbProp.name) }_=", valueConversionCode.references,
				description = s"Updates the ${ prop.name.pluralDoc } of the targeted ${ className.pluralDoc }",
				returnDescription = s"Whether any ${className.doc} was affected")(
				Parameter(paramName, paramType, description = s"A new ${ dbProp.name.doc } to assign")
					.withImplicits(connectionParam))(
				s"putColumn($modelPropName.${ dbProp.name.prop }.column, $valueConversionCode)")
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
					if (isCustomIndex)
						(Name("with", "with", CamelCase.lower) + prop.name).pluralIn(naming(FunctionName))
					else
						""
				}
				
				// Writes the actual methods, if needed
				lazy val singleParamName = prop.name.prop
				lazy val concreteType = prop.dataType.concrete
				lazy val singleValueCode = concreteType.toValueCode(singleParamName)
				val withMethod = withMethodName.notEmpty.map { name =>
					MethodDeclaration(name, singleValueCode.references,
						returnDescription = s"Copy of this access point that only includes ${
							classToWrite.name.pluralDoc } with the specified ${ prop.name }", isLowMergePriority = true)(
						Parameter(singleParamName, concreteType.toScala, description = s"${ prop.name } to target"))(
						s"filter($modelPropName.${ dbProp.name.prop }.column <=> ${singleValueCode.text})")
				}
				val inMethod = inMethodName.notEmpty.map { name =>
					val paramsName = prop.name.pluralIn(naming(ClassPropName))
					val code = singleValueCode.mapText { valueCode =>
						val valuesCode = {
							if (valueCode == singleParamName)
								paramsName
							else
								s"$paramsName.map { $singleParamName => $valueCode }"
						}
						s"filter($modelPropName.${ dbProp.name.prop }.column.in($valuesCode))"
					}
					MethodDeclaration(name, code.references,
						returnDescription = s"Copy of this access point that only includes ${
							classToWrite.name.pluralDoc } where ${ prop.name } is within the specified value set",
						isLowMergePriority = true)(
						Parameter(paramsName, ScalaType.generic("Iterable", concreteType.toScala),
							description = s"Targeted ${ prop.name.pluralDoc }"))(code.text)
				}
				
				Pair(withMethod, inMethod).flatten
			}
		}
	}
}
