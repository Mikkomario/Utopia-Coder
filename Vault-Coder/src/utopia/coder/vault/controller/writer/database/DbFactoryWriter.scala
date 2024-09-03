package utopia.coder.vault.controller.writer.database

import utopia.coder.model.data.{Name, NamingRules}
import utopia.coder.model.enumeration.NamingConvention.CamelCase
import utopia.coder.model.scala.Visibility.{Private, Protected}
import utopia.coder.model.scala.code.CodePiece
import utopia.coder.model.scala.datatype.Reference._
import utopia.coder.model.scala.datatype.{Extension, GenericType, Reference, ScalaType}
import utopia.coder.model.scala.declaration.PropertyDeclarationType.ComputedProperty
import utopia.coder.model.scala.declaration._
import utopia.coder.model.scala.{DeclarationDate, Package, Parameter}
import utopia.coder.vault.model.data.{Class, ClassModelReferences, ClassReferences, VaultProjectSetup}
import utopia.coder.vault.util.ClassMethodFactory
import utopia.coder.vault.util.VaultReferences.Vault._
import utopia.flow.collection.immutable.{Pair, Single}
import utopia.flow.util.Mutate

import scala.collection.immutable.VectorBuilder
import scala.io.Codec

/**
  * Used for writing standard model (from DB) factory objects
  * @author Mikko Hilpinen
  * @since 1.9.2021, v0.1
  */
object DbFactoryWriter
{
	// ATTRIBUTES   --------------------
	
	/**
	  * A suffix added to class names in order to make them factory class names
	  */
	val factorySuffix = Name("DbFactory", "DbFactories", CamelCase.capitalized)
	
	private val likeSuffix = Name("Like", "Likes", CamelCase.capitalized)
	
	private lazy val defaultOrderingProp = ComputedProperty("defaultOrdering",
		explicitOutputType = Some(ScalaType.option(orderBy)), isOverridden = true, isLowMergePriority = true)("None")
	
	
	// OTHER    -------------------------
	
	/**
	  * Writes a factory used for processing database object data
	  * @param classToWrite Class data based on which the factory is created
	  * @param parentClassReferences References for this class' parents
	 * @param modelRefs    References to the model classes & traits
	  * @param dbPropsOrDbModelRef   Reference to the XDbProps trait (if generic),
	 *                              or the database model class (if concrete)
	  * @param codec        Implicit codec to use when writing the document
	  * @param setup        Implicit project-specific setup
	  * @return Reference to the new written factory object.
	 *         Followed by a reference to the generated XDbFactoryLike trait, if applicable.
	 *         Failure if writing failed.
	  */
	def apply(classToWrite: Class, parentClassReferences: Seq[ClassReferences],
	          modelRefs: ClassModelReferences, dbPropsOrDbModelRef: Reference)
	         (implicit codec: Codec, setup: VaultProjectSetup, naming: NamingRules) =
	{
		val factoryPackage = setup.factoryPackage / classToWrite.packageName
		val factoryName = (classToWrite.name + factorySuffix).className
		
		// Case: Abstract trait => Implements XDbFactoryLike, XDbFactory trait
		//                         and XDbFactory companion object containing a concrete implementation
		if (classToWrite.isGeneric)
			writeGenericFactory(classToWrite, factoryPackage, factoryName, parentClassReferences, modelRefs,
				dbPropsOrDbModelRef)
		// Case: Concrete factory => Just implements the XDbFactory object
		else {
			// Implements the primary from model method
			val fromModel = fromModelMethodFor(classToWrite) { assignments =>
				modelRefs.stored.targetCode +
					accessIdInApply(classToWrite)
						.append(modelRefs.data.targetCode + assignments.withinParenthesis, ", ")
						.withinParenthesis
			}
			
			File(factoryPackage,
				ObjectDeclaration(
					name = factoryName,
					extensions = extensionsFor(classToWrite, parentClassReferences, modelRefs.stored),
					properties = concretePropertiesFor(classToWrite, dbPropsOrDbModelRef),
					methods = Set(fromModel),
					description = s"Used for reading ${ classToWrite.name.doc } data from the DB",
					author = classToWrite.author, since = DeclarationDate.versionedToday
				)
			).write().map { _ -> None }
		}
	}
	
	/**
	 * Generates references to class files, as if they had been written
	 * @param factoryPackage Package where these factory classes would have been placed
	 * @param classToWrite Class for which these references are written
	 * @param naming Implicit naming rules
	 * @return Reference to the factory class/object + reference to the factory like -trait, if applicable
	 */
	def generateReferences(factoryPackage: Package, classToWrite: Class)(implicit naming: NamingRules) = {
		val pck = factoryPackage / classToWrite.packageName
		val factoryName = classToWrite.name + factorySuffix
		val factory = Reference(pck, factoryName.className)
		
		val factoryLike = {
			if (classToWrite.isGeneric)
				Some(Reference(pck, (factoryName + likeSuffix).className))
			else
				None
		}
		factory -> factoryLike
	}
	
	private def writeGenericFactory(classToWrite: Class, factoryPackage: Package, factoryName: String,
	                                parentClassReferences: Seq[ClassReferences],
	                                modelRefs: ClassModelReferences, dbPropsOrDbModelRef: Reference)
	                               (implicit codec: Codec, setup: VaultProjectSetup, naming: NamingRules) =
	{
		// Starts with the XDbFactoryLike[+A] trait
		val aGenericType = GenericType.covariant("A", description = "Type of read instances")
		val aType = aGenericType.toScalaType
		
		// Defines an abstract dbProps -property
		val dbProps = ComputedProperty.newAbstract("dbProps", dbPropsOrDbModelRef,
			description = "Database properties used when parsing column data",
			isOverridden = classToWrite.isExtension)
		
		// Defines an abstract apply(AnyModel, id, ...) for constructing new instances
		val applyParams = genericTraitApplyParamsFor(classToWrite)
		val applyMethod = MethodDeclaration.newAbstract("apply", aType,
			returnDescription = s"A ${ classToWrite.name } with the specified data", isProtected = true)(
			applyParams)
		
		// Implements the from model parsing functionality using this apply function
		val fromModelImplementation = fromModelMethodFor(classToWrite) { propAssignments =>
			val modelName = if (classToWrite.isExtension) "model" else "valid"
			s"apply($modelName, " +: accessIdInApply(classToWrite).append(propAssignments, ", ").append(")")
		}
		// When inheriting another trait, makes sure the new apply is not identical to the old
		val methods = {
			// Case: New apply would just override the old one with no effect => Skips listing either function
			if (classToWrite.isExtension && applyMethod.matches(fromModelImplementation))
				Set[MethodDeclaration]()
			else
				Set(applyMethod, fromModelImplementation)
		}
		
		val factoryDescription = s"Common trait for factories which parse ${
			classToWrite.name } data from database-originated models"
		
		File(factoryPackage, TraitDeclaration(
			name = (classToWrite.name + factorySuffix + likeSuffix).className,
			genericTypes = Single(aGenericType),
			extensions = extensionsFor(classToWrite, parentClassReferences, aType),
			properties = Single(dbProps),
			methods = methods,
			description = factoryDescription,
			author = classToWrite.author,
			since = DeclarationDate.versionedToday
		)).write().flatMap { factoryLikeRef =>
			// Next writes the XDbFactory trait + companion object
			val factoryType = ScalaType.basic(factoryName)
			
			// The trait doesn't add additional functionality, it simply removes the generic A type
			val traitDeclaration = TraitDeclaration(
				name = factoryName,
				extensions = factoryLikeRef(modelRefs.stored),
				description = factoryDescription,
				author = classToWrite.author,
				since = DeclarationDate.versionedToday
			)
			
			// The concrete implementation accepts 2 parameters: 1) table and 2) dbProps
			// The linked apply(...) method uses these same parameters
			val concreteImplementationName = s"_$factoryName"
			val constructorParams = Pair(
				Parameter("table", table, description = "Table from which data is read"),
				Parameter("dbProps", dbPropsOrDbModelRef,
					description = "Database properties used when reading column data"))
			val concreteFactoryImplementation = ClassDeclaration(
				visibility = Private,
				name = concreteImplementationName,
				extensions = Single(factoryType),
				constructionParams = constructorParams,
				properties = Single(defaultOrderingProp),
				// NB: The drop(2) here is not very elegant. May require refactoring later.
				methods = Set(MethodDeclaration("apply", Set(modelRefs.stored, modelRefs.data),
					visibility = Protected, isOverridden = true)(
					applyParams)(
					s"${ modelRefs.stored.target }(id, ${ modelRefs.data.target }(${
						applyParams.drop(2).map { _.name }.mkString(", ") }))")),
				isCaseClass = true
			)
			
			val constructConcrete = MethodDeclaration("apply", explicitOutputType = Some(factoryType),
				returnDescription = s"A factory used for parsing ${
					classToWrite.name.pluralDoc } from database model data")(constructorParams)(
				s"$concreteImplementationName(${ constructorParams.map { _.name }.mkString(", ") })")
			
			// The companion object provides access to this concrete implementation via apply
			val companionObject = ObjectDeclaration(
				name = factoryName,
				methods = Set(constructConcrete),
				nested = Set(concreteFactoryImplementation)
			)
			
			File(factoryPackage, companionObject, traitDeclaration).write().map { _ -> Some(factoryLikeRef) }
		}
	}
	
	private def extensionsFor(classToWrite: Class, parentClassReferences: Seq[ClassReferences],
	                          modelType: ScalaType): Vector[Extension] =
	{
		val builder = new VectorBuilder[Extension]()
		
		// When inheriting, extends the YDbFactoryLike[X]
		if (classToWrite.isExtension)
			builder ++= parentClassReferences.flatMap { _.generic.map[Extension] { _.dbFactoryLike(modelType) } }
		// If no enumerations are included, the inheritance is more specific (=> uses automatic validation)
		else if (classToWrite.fromDbModelConversionMayFail)
			builder += fromRowModelFactory(modelType)
		else
			builder += fromValidatedRowModelFactory(modelType)
		
		// For tables which contain a creation time index, additional inheritance is added
		if (classToWrite.recordsIndexedCreationTime)
			builder += fromTimelineRowFactory(modelType)
		
		// If the class supports deprecation, it is reflected in this factory also
		if (classToWrite.isDeprecatable)
			builder += deprecatable
		
		builder.result()
	}
	
	private def concretePropertiesFor(classToWrite: Class, dbModelRef: Reference)(implicit naming: NamingRules) =
	{
		val builder = new VectorBuilder[PropertyDeclaration]()
		
		// All objects define a model property, which is used in other functions
		// (when inheriting, this is called dbProps instead)
		val modelName = if (classToWrite.isExtension) "dbProps" else "model"
		builder += ComputedProperty(modelName, Set(dbModelRef),
			description = "Model that specifies how the data is read", isOverridden = classToWrite.isExtension)(
			dbModelRef.target)
		
		// All objects define the table property (implemented)
		builder += ComputedProperty("table", isOverridden = true)(s"$modelName.table")
		// Timestamp-based factories also specify a creation time property name
		if (classToWrite.recordsIndexedCreationTime)
			classToWrite.timestampProperty.foreach { createdProp =>
				builder += ComputedProperty("timestamp", isOverridden = true)(s"$modelName.${createdProp.name.prop}")
			}
		// Non-timestamp-based factories need to specify default ordering
		else
			builder += defaultOrderingProp
			
		// Deprecatable factories specify the deprecation condition (read from the database model)
		if (classToWrite.isDeprecatable) {
			builder += ComputedProperty("nonDeprecatedCondition", Set(dbModelRef), isOverridden = true)(
				s"$modelName.nonDeprecatedCondition")
		}
		
		builder.result()
	}
	
	private def fromModelMethodFor(classToWrite: Class)(fromAssignments: Mutate[CodePiece])
	                              (implicit naming: NamingRules) =
	{
		// When extending a parent YDbFactoryLike trait, implements their prepared apply function
		classToWrite.parents.headOption match {
			// Case: Inheriting an abstract trait => Implements their apply function
			case Some(parent) =>
				val code = ClassMethodFactory.classFromModelCode(classToWrite, "model")(fromAssignments)
				MethodDeclaration.usingCode("apply", code, visibility = Protected, isOverridden = true)(
					genericTraitApplyParamsFor(parent))
			
			// Case: Highest level class or trait => Implements apply(AnyModel) or fromValidatedModel(Model)
			case None =>
				// Case: Some properties must be parsed separately => Can't use fromValidatedModel(...)
				if (classToWrite.fromDbModelConversionMayFail)
					ClassMethodFactory
						.classFromModel(classToWrite, "table.validate(model)")(fromAssignments)
				// Case: Default => Uses fromValidatedModel(...)
				else
					ClassMethodFactory
						.classFromValidatedModel(classToWrite)(fromAssignments)
		}
	}
	
	private def genericTraitApplyParamsFor(classToWrite: Class)(implicit naming: NamingRules) = {
		// The apply parameters consist of 2 static parameters: model + id,
		// as well as n custom parameters which are defined by trait properties
		val customApplyParams = classToWrite.properties.map { prop =>
			Parameter(prop.name.prop, prop.dataType.toScala,
				description = s"${ prop.name } to assign to the new ${ classToWrite.name }")
		}
		Pair(
			Parameter("model", flow.anyModel, description = "Model from which additional data may be read"),
			Parameter("id", classToWrite.idType.toScala,
				description = s"Id to assign to the read/parsed ${ classToWrite.name }")
		) ++ customApplyParams
	}
	
	// Accesses the id property in an apply function implementation
	// Either from a validated model or from a property prepared by a parent trait
	private def accessIdInApply(classToWrite: Class) = {
		if (classToWrite.isExtension)
			CodePiece("id")
		else {
			val dbProps = if (classToWrite.isGeneric) "dbProps" else "this.model"
			classToWrite.idType.fromValueCode(s"valid($dbProps.id.name)")
		}
	}
}
