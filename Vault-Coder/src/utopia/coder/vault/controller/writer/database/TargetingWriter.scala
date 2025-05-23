package utopia.coder.vault.controller.writer.database

import utopia.coder.model.data.{Name, NamingRules}
import utopia.coder.model.enumeration.NamingConvention.CamelCase
import utopia.coder.model.scala.Visibility.Protected
import utopia.coder.model.scala.datatype.Reference._
import utopia.coder.model.scala.datatype.{Extension, GenericType, Reference, ScalaType}
import utopia.coder.model.scala.declaration.PropertyDeclarationType.{ComputedProperty, ImmutableValue, LazyValue}
import utopia.coder.model.scala.declaration.{ClassDeclaration, File, MethodDeclaration, TraitDeclaration}
import utopia.coder.model.scala.{DeclarationDate, Package, Parameter}
import utopia.coder.vault.model.data.{Class, VaultProjectSetup}
import utopia.coder.vault.util.VaultReferences.Vault._
import utopia.flow.collection.CollectionExtensions._
import utopia.flow.collection.immutable.{Pair, Single}

import scala.io.Codec

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
	
	private val modelSuffix = Name("model", "models", CamelCase.lower)
	
	
	// OTHER    ---------------------
	
	def apply(classToWrite: Class, modelRef: Reference, dbModelRef: Reference, dbFactoryRef: Reference)
	         (implicit codec: Codec, setup: VaultProjectSetup, naming: NamingRules) =
	{
		// TODO: Implement
	}
	
	private def writeAccessValue(classToWrite: Class, targetPackage: Package, dbModelRef: Reference,
	                              accessMany: Boolean = false)
	                             (implicit codec: Codec, setup: VaultProjectSetup, naming: NamingRules) =
	{
		// Prepares the class properties
		val modelProp = ImmutableValue("model", Set(dbModelRef))(dbModelRef.target)
		val idProp = LazyValue(if (accessMany) "id" else "ids", Set(flow.valueConversions))(
			"apply(model.index) { _.getInt }")
		val columnProps = classToWrite.properties.map { prop =>
			val name = if (accessMany) prop.name.props else prop.name.prop
			val fromValue = prop.dataType.fromValueCode(Single("v"))
			LazyValue(name, fromValue.references + flow.valueConversions, description = prop.description,
				isLowMergePriority = true)(s"apply(model.$name) { v => $fromValue }")
		}
		
		// Prepares the class
		val accessValueClass = ClassDeclaration(
			name = ((accessPrefix +: classToWrite.name) +
				(if (accessMany) valueSuffix.plural else valueSuffix.singular)).className,
			constructionParams = Single(Parameter("access", if (accessMany) accessManyColumns else accessColumn)),
			extensions = Single(if (accessMany) accessValues else accessValue),
			properties = Pair(modelProp, idProp) ++ columnProps,
			description = s"Used for accessing${ if (accessMany) "" else " individual" } ${
				classToWrite.name } values from the DB",
			author = classToWrite.author,
			since = DeclarationDate.versionedToday,
			isCaseClass = true
		)
		
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
					name = (filterPrefix + classToWrite.name).pluralClassName,
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
	
	private def writeAccess(targetPackage: Package, classToWrite: Class, modelRef: Reference, valuesRef: Reference,
	                        filterRef: Option[Reference], accessMany: Boolean)
	                       (implicit codec: Codec, setup: VaultProjectSetup, naming: NamingRules) =
	{
		val rawAccessName = accessPrefix + classToWrite.name
		val singleAccessName = rawAccessName.className
		val accessName = if (accessMany) rawAccessName.pluralClassName else singleAccessName
		val accessType = ScalaType.basic(s"$accessName[A]")
		
		val genericOutput = GenericType("A")
		val genericOutputType = genericOutput.toScalaType
		val targetingOneType = targetingOne(ScalaType.option(genericOutputType))
		val wrappedAccessType = if (accessMany) targetingMany(genericOutputType) else targetingOneType
		
		val parentType: Extension = {
			if (accessMany)
				accessWrapper(genericOutputType, accessType, ScalaType.basic(s"$singleAccessName[A]"))
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
		
		val accessClass = ClassDeclaration(
			name = accessName,
			genericTypes = Single(genericOutput),
			constructionParams = Single(Parameter("wrapped", wrappedAccessType)),
			extensions = filterRef match {
				case Some(filter) => Pair[Extension](parentType, filter)
				case None => Single(parentType)
			},
			properties = Pair(
				LazyValue("values", Set(valuesRef),
					description = s"Access to the values of accessible ${
						if (accessMany) classToWrite.name.pluralDoc else classToWrite.name.doc }")(
					s"$valuesRef(wrapped)"),
				ComputedProperty("self", visibility = Protected, isOverridden = true)("this")
			),
			methods = Set(wrapMethod) ++ wrapIndividual,
			description = s"Used for accessing ${ if (accessMany) s"multiple" else "individual" } ${
				classToWrite.name.pluralDoc } from the DB at a time",
			author = classToWrite.author,
			since = DeclarationDate.versionedToday,
			isCaseClass = true
		)
		
		// TODO: Continue by writing the companion object
	}
	 
	/*
	object AccessIssue extends AccessOneRoot[AccessIssue[Issue]]
	{
		override lazy val root: AccessIssue[Issue] = AccessIssues.root.head
	}
	 */
	/*
	object AccessIssues extends AccessManyRoot[AccessIssues[Issue]]
	{
		// ATTRIBUTES   -----------------------
		
		/**
		  * Root issue access point
		  */
		override lazy val root = apply(AccessManyRows(IssueFactory))
		
		
		// IMPLICIT ------------------------------
		
		implicit def accessValues(access: AccessIssues[_]): AccessIssueValues = access.values
	}
	 */
}
