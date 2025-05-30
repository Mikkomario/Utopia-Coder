package utopia.coder.vault.controller.writer.database

import utopia.coder.model.data.{Name, NamingRules}
import utopia.coder.model.enumeration.NamingConvention.CamelCase
import utopia.coder.model.scala.Visibility.Protected
import utopia.coder.model.scala.datatype.Reference._
import utopia.coder.model.scala.datatype.{Extension, GenericType, Reference, ScalaType}
import utopia.coder.model.scala.declaration.PropertyDeclarationType.{ComputedProperty, ImmutableValue, LazyValue}
import utopia.coder.model.scala.declaration._
import utopia.coder.model.scala.{DeclarationDate, Package, Parameter}
import utopia.coder.vault.model.data.reference.{ClassReferences, TargetingReferences}
import utopia.coder.vault.model.data.{Class, CombinationData, VaultProjectSetup}
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
	private val valueSuffix = Name("Value", "Values", CamelCase.capitalized)
	private val filterPrefix = Name("Filter", "Filter", CamelCase.capitalized)
	private lazy val filterByPrefix = Name("FilterBy", "FilterBy", CamelCase.capitalized)
	
	private lazy val wherePrefix = Name("where", "where", CamelCase.lower)
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
	 * @param modelRef A reference to the stored model class
	 * @param dbModelRef A reference to the database model or database properties class/trait
	 * @param dbFactoryRef A reference to the database factory object
	 * @param codec Implicit encoding applied
	 * @param setup Implicit project setup
	 * @param naming Implicit naming rules applied
	 * @return Success or a failure. A success yields generated (reusable) references.
	 */
	def apply(classToWrite: Class, parentClassRefs: Seq[ClassReferences], modelRef: Reference,
	          dbModelRef: Reference, dbFactoryRef: Reference)
	         (implicit codec: Codec, setup: VaultProjectSetup, naming: NamingRules) =
	{
		val targetPackage = AccessWriter.packageFor(setup.accessPackage, classToWrite)
		
		// Writes the filter trait, if appropriate
		val filterRef = writeFilter(targetPackage, classToWrite, dbModelRef)
			.flatMap { _.logWithMessage(s"Failed to generate the filter trait for ${ classToWrite.name }")(setup.log) }
		
		// Writes the single & plural value access classes, and the primary access classes
		// NB: Won't generate primary access classes for generic classes
		Pair(false, true)
			.tryMap { accessMany =>
				writeAccessValue(targetPackage, classToWrite, parentClassRefs, dbModelRef, accessMany)
					.flatMap { accessValueRef =>
						val accessRef = {
							if (classToWrite.isGeneric)
								Success(None)
							else
								writeAccess(targetPackage, classToWrite, modelRef, dbFactoryRef, accessValueRef,
									filterRef.emptyOrSingle.map { classToWrite.name -> _ } ++
										parentClassRefs.iterator.flatMap { c =>
											c.targeting.flatMap { _.filtering.map { c.targetClass.name -> _ } }
										},
									accessMany = accessMany)
									.map { Some(_) }
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
	 * Writes the access classes for a combo class
	 * @param combo Combo to write
	 * @param comboRef Reference to the combo trait / class
	 * @param comboDbFactoryRef Reference to the combo-specific database factory
	 * @param parentRefs Targeting references concerning the parent class
	 * @param childRefs Targeting references concerning the child class
	 * @param codec Implicit encoding settings
	 * @param setup Implicit project setup
	 * @param naming Implicit naming rules applied
	 * @return Success or a failure
	 */
	def writeForCombo(combo: CombinationData, comboRef: Reference, comboDbFactoryRef: Reference,
	                  parentRefs: TargetingReferences, childRefs: TargetingReferences)
	                 (implicit codec: Codec, setup: VaultProjectSetup, naming: NamingRules) =
	{
		val childClassName = combo.childClass.name
		Pair(false, true).tryMap { accessMany =>
			val childValuesPropName = if (accessMany) childClassName.props else childClassName.prop
			val childValuesRef = childRefs.valueFor(accessMany)
			writeAccess(
				targetPackage = AccessWriter.packageFor(setup.accessPackage, combo.parentClass),
				classToWrite = combo.parentClass,
				modelRef = comboRef, dbFactoryRef = comboDbFactoryRef, valuesRef = parentRefs.valueFor(accessMany),
				filterRefs = Pair(combo.parentClass -> parentRefs, combo.childClass -> childRefs)
					.flatMap { case (c, refs) => refs.filtering.map { c.name -> _ } },
				extraProps = Single(
					LazyValue(childValuesPropName, Set(childValuesRef),
						description = s"Access to $childClassName -specific values")(
						s"${ childValuesRef.target }(wrapped)")),
				accessMany = accessMany)
		}
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
				Some(LazyValue(if (accessMany) "id" else "ids", Set(flow.valueConversions))(
					"apply(model.index) { _.getInt }"))
		}
		val columnProps = classToWrite.properties.filterNot { _.isExtension }.map { prop =>
			val name = if (accessMany) prop.name.props else prop.name.prop
			val fromValue = prop.dataType.fromValueCode(Single("v"))
			LazyValue(name, fromValue.references + flow.valueConversions, description = prop.description,
				isLowMergePriority = true)(s"apply(model.$name) { v => $fromValue }")
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
	
	private def writeFilter(targetPackage: Package, classToWrite: Class, dbModelRef: Reference)
	                       (implicit codec: Codec, setup: VaultProjectSetup, naming: NamingRules) =
	{
		val modelName = (classToWrite.name + modelSuffix).prop
		// Won't generate a file if there are no "with" or "in" -methods
		AccessWriter.filterMethodsFor(classToWrite, modelName).notEmpty.map { filterMethods =>
			val reprType = ScalaType.basic("Repr")
			val modelProp = ComputedProperty(modelName, Set(dbModelRef),
				description = s"Model that defines ${ classToWrite.name } database properties")(dbModelRef.target)
			
			File(targetPackage,
				TraitDeclaration(
					name = filterTraitNameFor(classToWrite),
					genericTypes = Single(GenericType.covariant("Repr")),
					extensions = Single(filterableView(reprType)),
					properties = Single(modelProp),
					methods = filterMethods.toSet,
					description = s"Common trait for access points which may be filtered based on ${
						classToWrite.name } properties",
					author = classToWrite.author,
					since = DeclarationDate.versionedToday
				)
			).write()
		}
	}
	
	private def writeAccess(targetPackage: Package, classToWrite: Class,
	                        modelRef: Reference, dbFactoryRef: Reference, valuesRef: Reference,
	                        filterRefs: Seq[(Name, Reference)], extraProps: Iterable[PropertyDeclaration] = Empty,
	                        accessMany: Boolean)
	                       (implicit codec: Codec, setup: VaultProjectSetup, naming: NamingRules) =
	{
		val rawAccessName = accessPrefix + classToWrite.name
		val singleAccessName = rawAccessName.className
		val manyAccessName = rawAccessName.pluralClassName
		val accessName = if (accessMany) manyAccessName else singleAccessName
		val accessType = ScalaType.basic(s"$accessName[A]")
		
		val genericOutput = GenericType("A")
		val genericOutputType = genericOutput.toScalaType
		val targetingOneType = targetingOne(ScalaType.option(genericOutputType))
		val wrappedAccessType = if (accessMany) targetingMany(genericOutputType) else targetingOneType
		
		val parentType: Extension = {
			if (accessMany)
				accessRowsWrapper(genericOutputType, accessType, ScalaType.basic(s"$singleAccessName[A]"))
			else
				accessOneWrapper(ScalaType.option(genericOutputType), accessType)
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
		
		// Additional filtering functions are available from nested objects (to avoid naming conflicts)
		val (filterObjects, filterProps) = {
			if (filterRefs.hasSize > 1)
				filterRefs.tail.splitMap { case (className, filterRef) =>
					val objectName = (filterByPrefix +: className).className
					val filterObject = ObjectDeclaration(
						name = objectName,
						extensions = Single(filterRef(accessType)),
						properties = Vector(
							ComputedProperty("table", isOverridden = true)(s"$accessName.this.table"),
							ComputedProperty("target", isOverridden = true)(s"$accessName.this.target"),
							ComputedProperty("accessCondition", isOverridden = true)(s"$accessName.this.accessCondition")),
						methods = Set(
							MethodDeclaration("apply", isOverridden = true)(Parameter("condition", condition))(
								s"$accessName.this(condition)")),
						description = s"An interface for $className -based filtering"
					)
					val filterProp = ComputedProperty((wherePrefix +: className).prop,
						description = s"Access to $className -based filtering functions")(objectName)
					
					filterObject -> filterProp
				}
			else
				Empty -> Empty
		}
		
		val accessClass = ClassDeclaration(
			name = accessName,
			genericTypes = Single(genericOutput),
			constructionParams = Single(Parameter("wrapped", wrappedAccessType)),
			extensions = Single(parentType) ++ filterRefs.headOption.map[Extension] { _._2(accessType) },
			properties = Pair(
				LazyValue("values", Set(valuesRef),
					description = s"Access to the values of accessible ${
						if (accessMany) classToWrite.name.pluralDoc else classToWrite.name.doc }")(
					s"$valuesRef(wrapped)"),
				ComputedProperty("self", visibility = Protected, isOverridden = true)("this")
			) ++ filterProps ++ extraProps,
			methods = Set(wrapMethod) ++ wrapIndividual,
			nested = filterObjects.toSet,
			description = s"Used for accessing ${ if (accessMany) s"multiple" else "individual" } ${
				classToWrite.name.pluralDoc } from the DB at a time",
			author = classToWrite.author,
			since = DeclarationDate.versionedToday,
			isCaseClass = true
		)
		
		// Prepares the companion object, also
		val companionParentRef = if (accessMany) accessManyRoot else accessOneRoot
		val companion = ObjectDeclaration(
			name = accessName,
			extensions = Single(companionParentRef(ScalaType.basic(accessName)(modelRef))),
			properties = Single(LazyValue("root", if (accessMany) Set(dbFactoryRef) else Set(), isOverridden = true)(
				if (accessMany) s"apply(AccessManyRows($dbFactoryRef))" else s"$manyAccessName.root.head")),
			methods = Set(
				MethodDeclaration("accessValues", Set(implicitConversions), isImplicit = true,
					description = "Provides implicit access to an access point's .values property")(
					Parameter("access", ScalaType.basic(accessName)(ScalaType.basic("_")),
						description = "Access point whose values are accessed"))(
					"access.values"))
		)
		
		File(targetPackage, companion, accessClass).write()
	}
	
	private def filterTraitNameFor(classToWrite: Class)(implicit naming: NamingRules) =
		(filterPrefix + classToWrite.name).pluralClassName
	private def valueAccessNameFor(classToWrite: Class, accessMany: Boolean)(implicit naming: NamingRules) =
		((accessPrefix +: classToWrite.name) + (if (accessMany) valueSuffix.plural else valueSuffix.singular)).className
}
