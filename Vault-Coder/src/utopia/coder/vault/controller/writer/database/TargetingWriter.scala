package utopia.coder.vault.controller.writer.database

import utopia.coder.model.data.{Name, NamingRules}
import utopia.coder.model.enumeration.NameContext.ClassPropName
import utopia.coder.model.enumeration.NamingConvention.CamelCase
import utopia.coder.model.scala.Visibility.Protected
import utopia.coder.model.scala.code.CodePiece
import utopia.coder.model.scala.datatype.Reference._
import utopia.coder.model.scala.datatype._
import utopia.coder.model.scala.declaration.PropertyDeclarationType.{ComputedProperty, ImmutableValue, LazyValue}
import utopia.coder.model.scala.declaration._
import utopia.coder.model.scala.{DeclarationDate, Package, Parameter, Parameters}
import utopia.coder.vault.model.data.reference.{ClassReferences, TargetingReferences}
import utopia.coder.vault.model.data.{Class, CombinationData, VaultProjectSetup}
import utopia.coder.vault.model.datatype.StandardPropertyType.{CreationTime, Deprecation, Expiration}
import utopia.coder.vault.util.VaultReferences
import utopia.coder.vault.util.VaultReferences.Vault
import utopia.coder.vault.util.VaultReferences.Vault._
import utopia.flow.collection.CollectionExtensions._
import utopia.flow.collection.immutable.{Empty, Pair, Single}
import utopia.flow.util.TryExtensions._

import scala.io.Codec
import scala.util.Success

/**
 * Writes the targeting access classes
 *
 * @author Mikko Hilpinen
 * @since 22.05.2025, v1.13
 */
object TargetingWriter
{
	// ATTRIBUTES   -----------------
	
	private val accessPrefix = Name("Access", "Access", CamelCase.capitalized)
	private lazy val individualPrefix = Name("Individual", "Individuals", CamelCase.capitalized)
	private val combinedPrefix = Name("Combined", "Combined", CamelCase.capitalized)
	private val valueSuffix = Name("Value", "Values", CamelCase.capitalized)
	private val filterPrefix = Name("Filter", "Filter", CamelCase.capitalized)
	private lazy val filterByPrefix = Name("FilterBy", "FilterBy", CamelCase.capitalized)
	private val rowsSuffix = Name("Row", "Rows", CamelCase.capitalized)
	
	private lazy val joinedToPrefix = Name("joinedTo", "joinedTo", CamelCase.lower)
	private lazy val wherePrefix = Name("where", "where", CamelCase.lower)
	private lazy val withPrefix = Name("with", "with", CamelCase.lower)
	
	
	// OTHER    ---------------------
	
	/**
	 * Generates class references, as if files had been written
	 * @param accessPackage The applied "database/access" package
	 * @param c Class for which references are acquired
	 * @param naming Implicit naming rules
	 * @return References to that class' targeting interfaces
	 */
	def generateReferencesFor(accessPackage: Package, c: Class)(implicit naming: NamingRules) = {
		val targetPackage = AccessWriter.packageFor(accessPackage, c)
		TargetingReferences(
			Reference(targetPackage, valueAccessNameFor(c, accessMany = false)),
			Reference(targetPackage, valueAccessNameFor(c, accessMany = true)),
			Some(Reference(targetPackage, filterTraitNameFor(c))))
	}
	
	/**
	 * Writes targeting access point classes for the specified class
	 * @param classToWrite Class for which these files are generated
	 * @param parentClassRefs References concerning the written class' parents
	 * @param combos Combos where the written class is the parent class
	 * @param tablesRef Reference to the tables object
	 * @param modelRef A reference to the stored model class
	 * @param dbModelRef A reference to the database model or database properties class/trait
	 * @param dbFactoryRef A reference to the database factory object
	 * @param codec Implicit encoding applied
	 * @param setup Implicit project setup
	 * @param naming Implicit naming rules applied
	 * @return Success or a failure. A success yields generated (reusable) references.
	 */
	def apply(classToWrite: Class, parentClassRefs: Seq[ClassReferences], combos: Seq[CombinationData],
	          tablesRef: Reference, modelRef: Reference, dbModelRef: Reference, dbFactoryRef: Reference)
	         (implicit codec: Codec, setup: VaultProjectSetup, naming: NamingRules) =
	{
		val targetPackage = packageFor(classToWrite)
		
		// Writes the filter trait, if appropriate
		val filterRef = writeFilter(targetPackage, classToWrite, parentClassRefs, dbModelRef)
			.flatMap { _.logWithMessage(s"Failed to generate the filter trait for ${ classToWrite.name }")(setup.log) }
		// Writes the filter by -class
		filterRef.foreach { filterRef =>
			writeFilterBy(targetPackage, classToWrite, filterRef, dbModelRef)
				.logWithMessage(s"Failed to generate the filter by -class for ${ classToWrite.name }")(setup.log)
		}
		
		// Writes the single & plural value access classes, and the primary access classes
		// NB: Won't generate primary access classes for generic classes
		Pair(false, true)
			.tryMap { accessMany =>
				writeAccessValue(targetPackage, classToWrite, parentClassRefs, dbModelRef, accessMany)
					.flatMap { accessValueRef =>
						val accessRef = {
							if (classToWrite.isGeneric)
								Success(None)
							else {
								val appliedFilter = filterRef.orElse {
									parentClassRefs.findMap { _.targeting.flatMap { _.filtering } }
								}
								writeAccess(targetPackage, classToWrite, combos, tablesRef, modelRef, dbModelRef,
									dbFactoryRef, accessValueRef,
									appliedFilter.map { classToWrite.name -> _ },
									author = classToWrite.author,
									accessMany = accessMany)
									.map { Some(_) }
							}
						}
						accessRef.map { _ -> accessValueRef }
					}
			}
			.map { references =>
				val (_, singularValue) = references.head
				val (_, manyValue) = references(1)
				TargetingReferences(singularValue, manyValue, filterRef)
			}
	}
	
	/**
	 * @param c Targeted class
	 * @param idCode Code that yields an id value
	 * @param setup Implicit project setup
	 * @param naming Implicit naming rules
	 * @return Code that accesses that class by that id in the database
	 */
	def singleAccessCodeFor(c: Class, idCode: CodePiece)(implicit setup: VaultProjectSetup, naming: NamingRules) = {
		val singleAccessRef = Reference(packageFor(c), (accessPrefix +: c.name).className)
		idCode.flatMapText { id => CodePiece(s"${ singleAccessRef.target }($id)", Set(singleAccessRef)) }
	}
	
	private def writeAccessValue(targetPackage: Package, classToWrite: Class, parentClassRefs: Seq[ClassReferences],
	                             dbModelRef: Reference, accessMany: Boolean)
	                             (implicit codec: Codec, setup: VaultProjectSetup, naming: NamingRules) =
	{
		val name = valueAccessNameFor(classToWrite, accessMany)
		val extensions: Seq[Extension] = {
			// If extending other value access traits, won't need to specify the root parent again
			val parentExtensions = parentClassRefs.flatMap { _.targeting }.map[Extension] { _.valueFor(accessMany) }
			if (parentExtensions.isEmpty)
				Single(if (accessMany) accessValues else accessValue)
			else
				parentExtensions
		}
		
		// Prepares the class properties
		val modelProp = {
			lazy val desc = s"Interface for accessing ${ classToWrite.name } database properties"
			if (classToWrite.isGeneric)
				ComputedProperty.newAbstract("model", dbModelRef, description = desc)
			else
				ImmutableValue("model", Set(dbModelRef), description = desc, isOverridden = classToWrite.isExtension)(
					dbModelRef.target)
		}
		val idProp = {
			if (classToWrite.isExtension)
				None
			else {
				val (name, code) = {
					if (accessMany)
						"ids" -> "apply(model.index) { _.getInt }"
					else
						"id" -> "apply(model.index).optional { _.int }"
				}
				Some(LazyValue(name, Set(flow.valueConversions),
					description = s"Access to ${ classToWrite.name } $name")(code))
			}
		}
		val columnProps = classToWrite.properties.filterNot { p => p.isExtension || p.isMultiColumn }.map { prop =>
			val name = if (accessMany) prop.name.props else prop.name.prop
			val readType = if (accessMany) prop.dataType else prop.dataType.optional
			val inputType = prop.dataType.concrete
			
			val fromValue = readType.fromValueCode(Single("v"))
			val defaultMethodName = if (accessMany || readType.isBothOptionalAndConcrete) "" else ".optional"
			// When from value yields a try, requires a custom to-value conversion
			val (methodName, toValue) = {
				if (readType.yieldsTryFromValue) {
					if (accessMany)
						".logging" -> CodePiece.empty
					else
						".customInput" -> inputType.toValueCode("v")
							.mapText { toValue => s" { v: ${ inputType.toScala } => $toValue }" }
				}
				else if (accessMany && !readType.isBothOptionalAndConcrete && readType.isOptional)
					".flatten" -> CodePiece.empty
				else
					defaultMethodName -> CodePiece.empty
			}
			
			LazyValue(name, fromValue.references ++ toValue.references + flow.valueConversions,
				description = prop.description, isLowMergePriority = true)(
				s"apply(model.${ prop.name.prop })$methodName { v => $fromValue }${ toValue.text }")
		}
		val props = Single(modelProp) ++ idProp ++ columnProps
		
		val description = s"Used for accessing${ if (accessMany) "" else " individual" } ${
			classToWrite.name } values from the DB"
		
		// Prepares the class
		// Value access interfaces of generic classes are written as traits instead
		val accessValueClass = {
			if (classToWrite.isGeneric)
				TraitDeclaration(
					name = name,
					extensions = extensions,
					properties = props,
					description = description,
					author = classToWrite.author,
					since = DeclarationDate.versionedToday
				)
			else
				ClassDeclaration(
					name = name,
					constructionParams = Single(Parameter("access", if (accessMany) accessManyColumns else accessColumn)),
					extensions = extensions,
					properties = props,
					description = description,
					author = classToWrite.author,
					since = DeclarationDate.versionedToday,
					isCaseClass = true
				)
		}
		
		// Writes the class
		File(targetPackage, accessValueClass).write()
	}
	
	private def writeFilter(targetPackage: Package, classToWrite: Class, parentClassRefs: Seq[ClassReferences],
	                        dbModelRef: Reference)
	                       (implicit codec: Codec, setup: VaultProjectSetup, naming: NamingRules) =
	{
		// Won't generate a file if there are no "with" or "in" -methods
		// (except for classes that extend more than 1 other class, and those which support deprecation)
		val filterMethods = AccessWriter.filterMethodsFor(classToWrite, "model")
		val parents = parentClassRefs.flatMap { _.targeting.flatMap { _.filtering } }
		if (filterMethods.nonEmpty || parents.hasSize >= 2 || classToWrite.isDeprecatable) {
			// Extends filter traits of parents, or FilterableView[+Repr]
			// Also, if deprecation is supported, may extend DeprecatableView, TimeDeprecatableView or NullDeprecatableView
			val reprType = ScalaType.basic("Repr")
			val deprecationProp = classToWrite.deprecationProperty.filterNot { _.isExtension }
			val deprecationParent = deprecationProp.map[Extension] { prop =>
				val parent = prop.dataType match {
					case Deprecation => nullDeprecatableView
					case Expiration => timeDeprecatableView
					case _ => deprecatableView
				}
				parent(reprType)
			}
			val extensions = parents.notEmpty match {
				case Some(parents) => parents.map[Extension] { _(reprType) } ++ deprecationParent
				case None => Single(deprecationParent.getOrElse[Extension] { filterableView(reprType) })
			}
			
			// The model declaration is abstract for generic classes
			val modelProp = {
				val desc = s"Model that defines ${ classToWrite.name } database properties"
				if (classToWrite.isGeneric)
					ComputedProperty.newAbstract("model", dbModelRef, description = desc)
				else
					ComputedProperty("model", Set(dbModelRef), description = desc)(dbModelRef.target)
			}
			
			Some(File(targetPackage,
				TraitDeclaration(
					name = filterTraitNameFor(classToWrite),
					genericTypes = Single(GenericType.covariant("Repr")),
					extensions = extensions,
					properties = Single(modelProp),
					methods = filterMethods.toSet,
					description = s"Common trait for access points which may be filtered based on ${
						classToWrite.name } properties",
					author = classToWrite.author,
					since = DeclarationDate.versionedToday
				)
			).write())
		}
		// Case: Shouldn't write a separate filter trait => Refers to a parent filter trait, if applicable
		else
			parents.headOption.map { Success(_) }
	}
	
	private def writeFilterBy(targetPackage: Package, classToWrite: Class, filterRef: Reference, dbModelRef: Reference)
	                         (implicit codec: Codec, setup: VaultProjectSetup, naming: NamingRules) =
	{
		val accessType = ScalaType.basic("A")
		val modelParam = {
			if (classToWrite.isGeneric)
				Some(Parameter("model", dbModelRef,
					description = s"A model used for accessing ${ classToWrite.name } database properties"))
			else
				None
		}
		
		File(targetPackage,
			ClassDeclaration(
				name = filterByTraitNameFor(classToWrite),
				genericTypes = Single(
					GenericType.covariant("A",
						requirement = Some(TypeRequirement.childOf(filterableView(accessType))),
						description = "Type of the wrapped access class"
					)
				),
				constructionParams =
					Single(
						Parameter("wrapped", accessType,
							description = s"Wrapped access point. Expected to include ${ classToWrite.tableName }.")
					) ++ modelParam,
				extensions = Pair(filterRef(accessType), filterableViewWrapper(accessType)),
				description = s"An interface which provides ${
					classToWrite.name } -based filtering for other types of access points.",
				author = classToWrite.author,
				since = DeclarationDate.versionedToday,
				isCaseClass = true
			)
		).write()
	}
	
	private def writeAccess(targetPackage: Package, classToWrite: Class, combos: Seq[CombinationData],
	                        tablesRef: Reference, modelRef: Reference, dbModelRef: Reference, dbFactoryRef: Reference,
	                        valuesRef: Reference, filterRef: Option[(Name, Reference)], author: String,
	                        accessMany: Boolean)
	                       (implicit codec: Codec, setup: VaultProjectSetup, naming: NamingRules) =
	{
		val className = classToWrite.name
		val rawAccessName = accessPrefix +: className
		val manyAccessName = rawAccessName.pluralClassName
		val singleAccessName = {
			// Makes sure there are no naming conflicts between the singular and plural versions
			val default = rawAccessName.className
			if (default == manyAccessName)
				((accessPrefix + individualPrefix) +: className).className
			else
				default
		}
		val accessName = if (accessMany) manyAccessName else singleAccessName
		val accessType = ScalaType.basic(accessName)
		lazy val singleAccessType = ScalaType.basic(s"$singleAccessName[A]")
		
		val genericOutput = GenericType("A")
		val genericOutputType = genericOutput.toScalaType
		val targetingOneType = targetingOne(ScalaType.option(genericOutputType))
		val wrappedAccessType: ScalaType = if (accessMany) accessManyColumns else targetingOneType
		val accessTypeWithOutput = accessType(genericOutputType)
		
		// The abstract access many version has a Repr type
		lazy val reprType = ScalaType.basic("Repr")
		val genericRepr = {
			if (accessMany) {
				val anyType = ScalaType.basic("_")
				Some(GenericType.covariant("Repr", Some(
					TypeRequirement.childOf(targetingManyLike(anyType, reprType, anyType)))))
			}
			else
				None
		}
		val (parentType, timestampProp): (Extension, Option[PropertyDeclaration]) = {
			if (accessMany)
				classToWrite.properties.find { p => p.dataType == CreationTime && p.isIndexed } match {
					case Some(creationProp) =>
						Extension.fromType(targetingTimeline(genericOutputType, reprType, singleAccessType)) ->
							Some(ComputedProperty("timestamp", Set(dbModelRef),
								isOverridden = true, isLowMergePriority = true)(s"${
								dbModelRef.target }.${ creationProp.name.prop }"))
					
					case None =>
						Extension.fromType(targetingManyLike(genericOutputType, reprType, singleAccessType)) -> None
				}
			else
				Extension.fromType(accessOneWrapper(ScalaType.option(genericOutputType), accessTypeWithOutput)) -> None
		}
		
		// May implement an abstract model property from a filter trait
		val modelProp = {
			if (filterRef.isEmpty)
				Some(ImmutableValue("model", Set(dbModelRef),
					description = s"A database model used for interacting with $className DB properties")(
					dbModelRef.target))
			else
				None
		}
		// For combinations, writes additional value access points and filter by -properties
		val comboProps = combos.flatMap { combo =>
			val childAccessPackage = packageFor(combo.childClass)
			
			// 1. Caches the joined access version, which is used in these properties
			val joinedPropName = (joinedToPrefix +: combo.childName).apply(ClassPropName, plural = accessMany)
			val joinedProp = LazyValue(joinedPropName, Set(tablesRef),
				description = s"A copy of this access which also targets ${ combo.childClass.tableName }")(
				s"join(${ tablesRef.target }.${ combo.childClass.name.prop })")
			
			// 2. Provides values access
			val accessValuesRef = Reference(childAccessPackage, valueAccessNameFor(combo.childClass, accessMany))
			val valuesProp = LazyValue(combo.childName(ClassPropName, plural = accessMany), Set(accessValuesRef),
				description = s"Access to the values of linked ${
					if (accessMany || combo.combinationType.isOneToMany)
						combo.childClass.name.pluralDoc
					else
						combo.childClass.name.doc }")(s"${ accessValuesRef.target }($joinedPropName)")
			
			// 3. Provides filtered access
			val filterByRef = Reference(childAccessPackage, filterByTraitNameFor(combo.childClass))
			val whereProp = LazyValue((wherePrefix +: combo.childName)(ClassPropName, accessMany), Set(filterByRef),
				description = s"Access to ${ combo.childClass.name } -based filtering functions",
				isLowMergePriority = true)(
				s"${ filterByRef.target }($joinedPropName)")
			
			Vector(joinedProp, valuesProp, whereProp)
		}
		val selfProp = ComputedProperty("self", visibility = Protected, isOverridden = true)("this")
		
		val deprecationMethod = {
			if (accessMany)
				classToWrite.deprecationProperty.filter { _.dataType == Deprecation }.map { prop =>
					MethodDeclaration("deprecate", Set(flow.now),
						description = s"Deprecates all accessible ${ className.pluralDoc }",
						returnDescription = s"Whether any $className was targeted")(
						Parameters(Empty,
							Single(Parameter("connection", connection, description = "Implicit DB connection"))))(
						s"values.${ prop.name.props }.set(Now)")
				}
			else
				None
		}
		def wrapMethodFor(wrappedAccessType: ScalaType, constructedTypeName: String) =
			MethodDeclaration("wrap", visibility = Protected, isOverridden = true)(
				Parameter("newTarget", wrappedAccessType))(s"$constructedTypeName(newTarget)")
		
		val accessClass = ClassDeclaration(
			name = accessName,
			genericTypes = Single(genericOutput) ++ genericRepr,
			constructionParams = Single(Parameter("wrapped", wrappedAccessType)),
			extensions = Single(parentType) ++
				filterRef.map[Extension] { _._2(if (accessMany) reprType else accessTypeWithOutput) },
			properties = Single(
				LazyValue("values", Set(valuesRef),
					description = s"Access to the values of accessible ${
						if (accessMany) className.pluralDoc else className.doc }")(
					s"${ valuesRef.target }(wrapped)")
			) ++ modelProp ++ timestampProp ++ comboProps ++ (if (accessMany) None else Some(selfProp)),
			// The abstract version doesn't contain wrap functions
			methods = (if (accessMany) Set() else Set(wrapMethodFor(wrappedAccessType, accessName))) ++
				deprecationMethod,
			description = s"Used for accessing ${ if (accessMany) s"multiple" else "individual" } ${
				className.pluralDoc } from the DB at a time",
			author = author,
			since = DeclarationDate.versionedToday,
			isCaseClass = !accessMany,
			// The access many -version is abstract (divides into 2 separate implementations)
			isAbstract = accessMany
		)
		
		// For access many -variants, also writes two implementations:
		//      1. A row-based one
		//      2. A separate version for one-to-many links
		lazy val accessRowsName = (rawAccessName + rowsSuffix).pluralClassName
		lazy val accessRowsType = ScalaType.basic(accessRowsName)
		lazy val accessRowsTypeWithOutput = accessRowsType(genericOutputType)
		lazy val accessCombinedName = ((accessPrefix + combinedPrefix) +: className).pluralClassName
		lazy val accessCombinedType = ScalaType.basic(accessCombinedName)
		val additionalClasses = {
			if (accessMany) {
				val accessCombinedTypeWithOutput = accessCombinedType(genericOutputType)
				
				// Multi-access points need to define a function for wrapping individual access points, too
				val wrapIndividual = MethodDeclaration("wrapUniqueTarget", visibility = Protected, isOverridden = true)(
					Parameter("target", targetingOneType))(s"$singleAccessName(target)")
				
				val accessRowsClass = ClassDeclaration(
					name = accessRowsName,
					genericTypes = Single(genericOutput),
					constructionParams = Parameter("wrapped", targetingManyRows(genericOutputType),
						description = "The wrapped access point"),
					extensions = Pair(
						(accessType(genericOutputType, accessRowsTypeWithOutput): Extension).withConstructor("wrapped"),
						accessRowsWrapper(genericOutputType, accessRowsTypeWithOutput, singleAccessType)
					),
					properties = Single(selfProp),
					methods = Set(wrapMethodFor(targetingManyRows(genericOutputType), accessRowsName), wrapIndividual),
					description = s"Provides access to row-specific $className -like items",
					author = author,
					since = DeclarationDate.versionedToday,
					isCaseClass = true
				)
				val accessCombinedClass = ClassDeclaration(
					name = accessCombinedName,
					genericTypes = Single(genericOutput),
					constructionParams = Parameter("wrapped", targetingMany(genericOutputType),
						description = "The wrapped access point"),
					extensions = Pair(
						(accessType(genericOutputType, accessCombinedTypeWithOutput): Extension).withConstructor("wrapped"),
						accessWrapper(genericOutputType, accessCombinedTypeWithOutput, singleAccessType)
					),
					properties = Single(selfProp),
					methods = Set(wrapMethodFor(targetingMany(genericOutputType), accessCombinedName), wrapIndividual),
					description = s"Used for accessing $className items that have been combined with one-to-many combinations",
					author = author,
					since = DeclarationDate.versionedToday,
					isCaseClass = true
				)
				
				Pair(accessRowsClass, accessCombinedClass)
			}
			else
				Empty
		}
		
		// Prepares the companion object, also
		// Provides access to standard access root(s)
		val companionParentType = {
			if (accessMany)
				accessManyRoot(accessRowsType(modelRef))
			else
				accessOneRoot(accessType(modelRef))
		}
		val unfilteredRootCode = {
			if (accessMany) {
				val accessRef = VaultReferences.vault.accessManyRows
				CodePiece(s"$accessRowsName(${ accessRef.target }(${ dbFactoryRef.target }))",
					Set(accessRef, dbFactoryRef))
			}
			else
				CodePiece(s"$manyAccessName.root.head")
		}
		// Deprecating classes have a separate includingHistory -access point
		val defaultRootProps = {
			if (accessMany && classToWrite.isDeprecatable)
				Pair(
					LazyValue("includingHistory", unfilteredRootCode.references,
						description = s"Access to ${ className.pluralDoc }, including historical entries")(
						unfilteredRootCode.text),
					LazyValue("root", isOverridden = true)("includingHistory.active")
				)
			else
				Single(LazyValue("root", unfilteredRootCode.references, isOverridden = true)(unfilteredRootCode.text))
		}
		val comboRootProps = combos.map { combo =>
			val rawPropName = withPrefix +: combo.childName
			val code = {
				if (accessMany) {
					val dbFactoryRef = CombinedFactoryWriter.generateReference(combo, targeting = true)
					val (function, accessRef) = {
						if (combo.combinationType.isOneToMany)
							accessCombinedName -> Vault.accessMany
						else
							accessRowsName -> accessManyRows
					}
					CodePiece(s"$function(${ accessRef.target }(${ dbFactoryRef.target }))",
						Set(accessRef, dbFactoryRef))
				}
				else
					CodePiece(s"$manyAccessName.${ rawPropName.props }.head")
			}
			LazyValue(rawPropName(ClassPropName, accessMany), code.references,
				description = s"Access to ${ if (accessMany) "" else "individual " }${
					className.pluralDoc } in the DB, also including ${ combo.childClass.name } information")(code.text)
		}
		val anyTypeParam = ScalaType.basic("_")
		val companion = ObjectDeclaration(
			name = accessName,
			extensions = Single(companionParentType),
			properties = defaultRootProps ++ comboRootProps,
			methods = Set(
				MethodDeclaration("accessValues", Set(implicitConversions), explicitOutputType = Some(valuesRef),
					isImplicit = true, description = "Provides implicit access to an access point's .values property")(
					Parameter("access",
						ScalaType.basic(accessName)
							.apply(if (accessMany) Pair.twice(anyTypeParam) else Single(anyTypeParam)),
						description = "Access point whose values are accessed"))(
					"access.values"))
		)
		
		File(targetPackage, Pair(companion, accessClass) ++ additionalClasses).write()
	}
	
	private def packageFor(c: Class)(implicit setup: VaultProjectSetup) =
		AccessWriter.packageFor(setup.accessPackage, c)
	
	private def filterTraitNameFor(classToWrite: Class)(implicit naming: NamingRules) =
		(filterPrefix + classToWrite.name).pluralClassName
	private def filterByTraitNameFor(classToWrite: Class)(implicit naming: NamingRules) =
		(filterByPrefix +: classToWrite.name).className
	private def valueAccessNameFor(classToWrite: Class, accessMany: Boolean)(implicit naming: NamingRules) =
		((accessPrefix +: classToWrite.name) + (if (accessMany) valueSuffix.plural else valueSuffix.singular)).className
}
