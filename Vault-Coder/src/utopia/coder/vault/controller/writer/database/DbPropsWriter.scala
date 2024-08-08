package utopia.coder.vault.controller.writer.database

import utopia.coder.model.data.{Name, NamingRules}
import utopia.coder.model.enumeration.NamingConvention.CamelCase
import utopia.coder.model.scala.Visibility.Private
import utopia.coder.model.scala.datatype.{Reference, ScalaType}
import utopia.coder.model.scala.declaration.PropertyDeclarationType.{ComputedProperty, LazyValue}
import utopia.coder.model.scala.declaration._
import utopia.coder.model.scala.{DeclarationDate, Package, Parameter}
import utopia.coder.vault.model.data.{Class, ClassReferences, VaultProjectSetup}
import utopia.coder.vault.util.VaultReferences._
import utopia.flow.collection.CollectionExtensions.RichIterable
import utopia.flow.collection.immutable.{Pair, Single}
import utopia.flow.util.StringExtensions._

import scala.io.Codec

/**
 * Used for writing database configuration classes, which are used with generic traits
 *
 * @author Mikko Hilpinen
 * @since 14.06.2024, v1.11
 */
object DbPropsWriter
{
	// ATTRIBUTES   -----------------------
	
	private val dbPropsSuffix = Name("DbProps", "DbProps", CamelCase.capitalized)
	private val wrapperSuffix = Name("Wrapper", "Wrappers", CamelCase.capitalized)
	
	private val propNameSuffix = Name("propName", "propNames", CamelCase.lower)
	
	
	// OTHER    ---------------------------
	
	/**
	 * Writes a XDbProps and XDbPropsWrapper traits,
	 * which are used when defining/customizing database interactions with generic traits
	 * @param classToWrite Written class / trait this configuration is used with
	 * @param codec Implicit codec used to determine file formatting
	 * @param setup Implicit project setup
	 * @param naming Implicit naming rules
	 * @return References to 1) XDbProps trait and 2) to XDbPropsWrapper trait +
	 *         name of the property used for defining the wrapped XDbProps.
	 *         Failure if file-writing failed.
	 */
	def apply(classToWrite: Class, parentClassReferences: Seq[ClassReferences])
	         (implicit codec: Codec, setup: VaultProjectSetup, naming: NamingRules) =
	{
		val configPackage = setup.databasePackage/"props"/classToWrite.packageName
		writeDbPropsTrait(classToWrite, configPackage, parentClassReferences).flatMap { dbProps =>
			writeDbPropsWrapperTrait(classToWrite, configPackage, dbProps)
				.map { case (wrapper, wrappedName) => Pair(dbProps, wrapper) -> wrappedName }
		}
	}
	
	// Writes XDbProps trait and companion object
	private def writeDbPropsTrait(classToWrite: Class, configPackage: Package,
	                              parentClassReferences: Seq[ClassReferences])
	                             (implicit codec: Codec, setup: VaultProjectSetup, naming: NamingRules) =
	{
		// The trait defines declarations for all the database properties (except those defined in parent traits)
		val traitName = (classToWrite.name + dbPropsSuffix).className
		val traitType = ScalaType.basic(traitName)
		
		val abstractProps = classToWrite.properties.filterNot { _.isDirectExtension }
			.flatMap { prop =>
				prop.dbProperties.map { prop =>
					ComputedProperty.newAbstract(prop.name.prop, vault.dbProp,
						description = s"Declaration which defines how ${
							prop.name } shall be interacted with in the database")
				}
			}
			.toVector
		// When extending parent traits, implements the renamed property declarations
		// NB: Doesn't work for multi-part properties
		val renameImplementations = classToWrite.properties.filter { _.dbProperties.hasSize(1) }.flatMap { prop =>
			prop.rename.map { case (original, implementation) =>
				ComputedProperty(original.prop, isOverridden = true)(implementation.prop)
			}
		}
		
		val dbPropsTrait = TraitDeclaration(
			name = traitName,
			extensions = parentClassReferences.flatMap { _.generic.map { _.dbProps } },
			properties = abstractProps ++ renameImplementations,
			author = classToWrite.author,
			description = s"Common trait for classes which provide access to ${ classToWrite.name } database properties",
			since = DeclarationDate.versionedToday
		)
		
		// The companion object contains a concrete implementation
		val propNames = classToWrite.dbProperties
			.map { prop =>
				val defaultName = prop.modelName.quoted
				prop -> Parameter((prop.name + propNameSuffix).prop, ScalaType.string, defaultName,
					description = s"Name of the database property matching ${ prop.name } (default = $defaultName)")
			}
			.toVector
		val concreteProps = propNames.map { case (prop, propParam) =>
			LazyValue(prop.name.prop, Set(vault.dbProp), isOverridden = true)(
				s"${ vault.dbProp.target }(table, ${ propParam.name })")
		}
		// val idProp = LazyValue("id", Set(vault.dbProp), isOverridden = true)("DbPropertyDeclaration(idPropName, index)")
		
		// val defaultIdPropName = classToWrite.idDatabasePropName.quoted
		/*
		Parameter("idPropName", ScalaType.string, classToWrite.idDatabasePropName.quoted,
				description = s"Name of the database property which represents this class' primary row id (default = \"$defaultIdPropName\")")
		 */
		val constructionParams = Parameter("table", vault.table,
			description = "Table operated using this configuration") +: propNames.map { _._2 }
		
		val concreteClassName = s"_$traitName"
		val concreteImplementation = ClassDeclaration(
			name = concreteClassName,
			extensions = Single(traitType),
			constructionParams = constructionParams,
			properties = /*idProp +:*/ concreteProps,
			visibility = Private,
			isCaseClass = true
		)
		
		val companionObject = ObjectDeclaration(
			name = traitName,
			methods = Set(MethodDeclaration("apply", explicitOutputType = Some(traitType),
				returnDescription = s"A model which defines all ${ classToWrite.name } database properties")(
				constructionParams)(s"$concreteClassName(${ constructionParams.map { _.name }.mkString(", ") })")),
			nested = Set(concreteImplementation)
		)
		
		File(configPackage, companionObject, dbPropsTrait).write()
	}
	
	// Writes XDbPropsWrapper trait
	// TODO: It is probably possible to extend YDbPropsWrapper from class parents. But is that necessary?
	private def writeDbPropsWrapperTrait(classToWrite: Class, configPackage: Package, dbPropsRef: Reference)
	                                    (implicit codec: Codec, setup: VaultProjectSetup, naming: NamingRules) =
	{
		val wrappedName = dbPropsRef.target.uncapitalize
		val wrapped = ComputedProperty.newAbstract(wrappedName, dbPropsRef,
			description = s"The wrapped ${ classToWrite.name } database properties", isProtected = true)
		// val idProp = ComputedProperty("id", isOverridden = true)(s"$wrappedName.id")
		val props = classToWrite.dbProperties
			.map { prop =>
				val propName = prop.name.prop
				ComputedProperty(prop.name.prop, isOverridden = true)(s"$wrappedName.$propName")
			}
			.toVector
		
		File(configPackage, TraitDeclaration(
			name = (classToWrite.name + dbPropsSuffix + wrapperSuffix).className,
			extensions = Single(dbPropsRef),
			properties = wrapped +: props,
			description = s"Common trait for interfaces that provide access to ${
				classToWrite.name } database properties by wrapping a ${ dbPropsRef.target }",
			author = classToWrite.author,
			since = DeclarationDate.versionedToday
		)).write()
			// Includes the wrapped property name in the result
			.map { _ -> wrappedName }
	}
}
