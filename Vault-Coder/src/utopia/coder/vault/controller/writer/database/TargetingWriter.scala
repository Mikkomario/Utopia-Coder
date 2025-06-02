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
import utopia.coder.model.scala.{DeclarationDate, Package, Parameter}
import utopia.coder.vault.model.data.reference.{ClassReferences, TargetingReferences}
import utopia.coder.vault.model.data.{Class, CombinationData, VaultProjectSetup}
import utopia.coder.vault.util.VaultReferences
import utopia.coder.vault.util.VaultReferences.Vault
import utopia.coder.vault.util.VaultReferences.Vault._
import utopia.flow.collection.CollectionExtensions._
import utopia.flow.collection.immutable.{Pair, Single}
import utopia.flow.util.StringExtensions._
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
	private val valueSuffix = Name("Value", "Values", CamelCase.capitalized)
	private val filterPrefix = Name("Filter", "Filter", CamelCase.capitalized)
	private lazy val filterByPrefix = Name("FilterBy", "FilterBy", CamelCase.capitalized)
	
	private lazy val joinedToPrefix = Name("joinedTo", "joinedTo", CamelCase.lower)
	private lazy val wherePrefix = Name("where", "where", CamelCase.lower)
	private lazy val withPrefix = Name("with", "with", CamelCase.lower)
	private val modelSuffix = Name("model", "models", CamelCase.lower)
	
	
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
		filterRef.foreach { case (filterRef, abstractModelPropName) =>
			writeFilterBy(targetPackage, classToWrite, filterRef, abstractModelPropName, dbModelRef)
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
									parentClassRefs.findMap { _.targeting.flatMap { refs =>
										refs.filtering.map { _ -> refs.filteringModelPropName }
									} }
								}
								writeAccess(targetPackage, classToWrite.name, combos, tablesRef, modelRef, dbModelRef,
									dbFactoryRef, accessValueRef,
									appliedFilter.map { case (ref, modelPropName) =>
										(classToWrite.name, ref, modelPropName)
									},
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
				val (filter, abstractModelPropName) = filterRef match {
					case Some((filter, prop)) => Some(filter) -> prop
					case None => None -> ""
				}
				
				TargetingReferences(singularValue, manyValue, filter, abstractModelPropName)
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
			else
				Some(LazyValue(if (accessMany) "ids" else "id", Set(flow.valueConversions))(
					"apply(model.index) { _.getInt }"))
		}
		val columnProps = classToWrite.properties.filterNot { _.isExtension }.map { prop =>
			val name = if (accessMany) prop.name.props else prop.name.prop
			val fromValue = prop.dataType.fromValueCode(Single("v"))
			// When from value yields a try, requires a custom to-value conversion
			val (factoryMethodCall, toValue) = {
				if (prop.dataType.yieldsTryFromValue)
					".customInput" -> prop.dataType.toValueCode("v")
						.mapText { toValue => s" { v: ${ prop.dataType.toScala } => $toValue }" }
				else
					"" -> CodePiece.empty
			}
			
			LazyValue(name, fromValue.references ++ toValue.references + flow.valueConversions,
				description = prop.description, isLowMergePriority = true)(
				s"apply(model.${ prop.name.prop })$factoryMethodCall { v => $fromValue }${ toValue.text }")
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
		val modelName = (classToWrite.name + modelSuffix).prop
		// Won't generate a file if there are no "with" or "in" -methods
		// (except for classes that extend more than 1 other class)
		val filterMethods = AccessWriter.filterMethodsFor(classToWrite, modelName)
		val parents = parentClassRefs
			.flatMap { _.targeting.flatMap { refs => refs.filtering.map { _ -> refs.filteringModelPropName } } }
		if (filterMethods.nonEmpty || parents.hasSize >= 2) {
			// Extends filter traits of parents, or FilterableView[+Repr]
			val reprType = ScalaType.basic("Repr")
			val extensions = parents.notEmpty match {
				case Some(parents) => parents.map[Extension] { _._1(reprType) }
				case None => Single[Extension](filterableView(reprType))
			}
			
			// The model declaration is abstract for generic classes
			val modelProp = {
				val desc = s"Model that defines ${ classToWrite.name } database properties"
				if (classToWrite.isGeneric)
					ComputedProperty.newAbstract(modelName, dbModelRef, description = desc)
				else
					ComputedProperty(modelName, Set(dbModelRef), description = desc)(dbModelRef.target)
			}
			// When inheriting, implements those abstract properties
			val extendedModelImplementations = parents.view.map { _._2 }.filter { _.nonEmpty }
				.map { propName => ComputedProperty(propName, isOverridden = true)(modelProp.name) }
				.toOptimizedSeq
			
			val writeResult = File(targetPackage,
				TraitDeclaration(
					name = filterTraitNameFor(classToWrite),
					genericTypes = Single(GenericType.covariant("Repr")),
					extensions = extensions,
					properties = modelProp +: extendedModelImplementations,
					methods = filterMethods.toSet,
					description = s"Common trait for access points which may be filtered based on ${
						classToWrite.name } properties",
					author = classToWrite.author,
					since = DeclarationDate.versionedToday
				)
			).write()
			
			Some(writeResult.map { _ -> (if (classToWrite.isGeneric) modelName else "") })
		}
		// Case: Shouldn't write a separate filter trait => Refers to a parent filter trait, if applicable
		else
			parents.headOption.map { Success(_) }
	}
	
	private def writeFilterBy(targetPackage: Package, classToWrite: Class, filterRef: Reference,
	                          abstractModelPropName: String, dbModelRef: Reference)
	                         (implicit codec: Codec, setup: VaultProjectSetup, naming: NamingRules) =
	{
		val accessType = ScalaType.basic("A")
		val modelParam = abstractModelPropName.ifNotEmpty.map { propName =>
			Parameter(propName, dbModelRef,
				description = s"A model used for accessing ${ classToWrite.name } database properties")
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
	
	private def writeAccess(targetPackage: Package, className: Name, combos: Seq[CombinationData], tablesRef: Reference,
	                        modelRef: Reference, dbModelRef: Reference, dbFactoryRef: Reference, valuesRef: Reference,
	                        filterRef: Option[(Name, Reference, String)], author: String, accessMany: Boolean,
	                        allowRowAccess: Boolean = true)
	                       (implicit codec: Codec, setup: VaultProjectSetup, naming: NamingRules) =
	{
		val rawAccessName = accessPrefix + className
		val singleAccessName = rawAccessName.className
		val manyAccessName = rawAccessName.pluralClassName
		val accessName = if (accessMany) manyAccessName else singleAccessName
		val accessType = ScalaType.basic(s"$accessName[A]")
		
		val genericOutput = GenericType("A")
		val genericOutputType = genericOutput.toScalaType
		val targetingOneType = targetingOne(ScalaType.option(genericOutputType))
		val wrappedAccessType = {
			if (accessMany) {
				if (allowRowAccess)
					targetingManyRows(genericOutputType)
				else
					targetingMany(genericOutputType)
			}
			else
				targetingOneType
		}
		
		val parentType: Extension = {
			if (accessMany) {
				val singleAccessType = ScalaType.basic(s"$singleAccessName[A]")
				if (allowRowAccess)
					accessRowsWrapper(genericOutputType, accessType, singleAccessType)
				else
					accessWrapper(genericOutputType, accessType, singleAccessType)
			}
			else
				accessOneWrapper(ScalaType.option(genericOutputType), accessType)
		}
		
		// May implement an abstract model property from a filter trait
		val modelProp = filterRef.flatMap { _._3.ifNotEmpty }.map { propName =>
			ComputedProperty(propName, Set(dbModelRef), isOverridden = true)(dbModelRef.target)
		}
		
		val wrapMethod = MethodDeclaration("wrap", visibility = Protected, isOverridden = true)(
			Parameter("newTarget", wrappedAccessType))(s"$accessName(newTarget)")
		// Multi-access points need to define a function for wrapping individual access points, too
		val wrapIndividual = {
			if (accessMany)
				Some(MethodDeclaration("wrapUniqueTarget", visibility = Protected, isOverridden = true)(
					Parameter("target", targetingOneType))(s"$singleAccessName(target)"))
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
				description = s"Access to ${ combo.childClass } -based filtering functions")(
				s"${ filterByRef.target }($joinedPropName)")
			
			Vector(joinedProp, valuesProp, whereProp)
		}
		
		val accessClass = ClassDeclaration(
			name = accessName,
			genericTypes = Single(genericOutput),
			constructionParams = Single(Parameter("wrapped", wrappedAccessType)),
			extensions = Single(parentType) ++ filterRef.map[Extension] { _._2(accessType) },
			properties = Pair(
				LazyValue("values", Set(valuesRef),
					description = s"Access to the values of accessible ${
						if (accessMany) className.pluralDoc else className.doc }")(
					s"${ valuesRef.target }(wrapped)"),
				ComputedProperty("self", visibility = Protected, isOverridden = true)("this")
			) ++ modelProp ++ comboProps,
			methods = Set(wrapMethod) ++ wrapIndividual,
			description = s"Used for accessing ${ if (accessMany) s"multiple" else "individual" } ${
				className.pluralDoc } from the DB at a time",
			author = author,
			since = DeclarationDate.versionedToday,
			isCaseClass = true
		)
		
		// Prepares the companion object, also
		val companionParentRef = if (accessMany) accessManyRoot else accessOneRoot
		val rootCode = {
			if (accessMany) {
				val accessRef = {
					if (allowRowAccess)
						accessManyRows
					else
						VaultReferences.vault.accessMany
				}
				CodePiece(s"apply(${ accessRef.target }(${ dbFactoryRef.target }))", Set(accessRef, dbFactoryRef))
			}
			else
				CodePiece(s"$manyAccessName.root.head")
		}
		val comboRootProps = combos.map { combo =>
			val rawPropName = withPrefix +: combo.childName
			val code = {
				if (accessMany) {
					val (dbFactoryRef, _) = DbFactoryWriter.generateReferences(setup.factoryPackage, combo.childClass)
					val accessRef = {
						if (combo.combinationType.isOneToMany)
							Vault.accessMany
						else
							accessManyRows
					}
					CodePiece(s"apply(${ accessRef.target }(${ dbFactoryRef.target }))", Set(accessRef, dbFactoryRef))
				}
				else
					CodePiece(s"$manyAccessName.${ rawPropName.props }.head")
			}
			LazyValue(rawPropName(ClassPropName, accessMany), code.references,
				description = s"Access to ${ if (accessMany) "" else "individual " }${
					className.pluralDoc } in the DB, also including ${ combo.childClass } information")(code.text)
		}
		val companion = ObjectDeclaration(
			name = accessName,
			extensions = Single(companionParentRef(ScalaType.basic(accessName)(modelRef))),
			properties = LazyValue("root", rootCode.references, isOverridden = true)(rootCode.text) +: comboRootProps,
			methods = Set(
				MethodDeclaration("accessValues", Set(implicitConversions), explicitOutputType = Some(valuesRef),
					isImplicit = true, description = "Provides implicit access to an access point's .values property")(
					Parameter("access", ScalaType.basic(accessName)(ScalaType.basic("_")),
						description = "Access point whose values are accessed"))(
					"access.values"))
		)
		
		File(targetPackage, companion, accessClass).write()
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
