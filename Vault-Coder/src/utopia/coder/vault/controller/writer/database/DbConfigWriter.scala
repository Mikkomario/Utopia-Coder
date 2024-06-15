package utopia.coder.vault.controller.writer.database

import utopia.coder.model.data.{Name, NamingRules}
import utopia.coder.model.enumeration.NamingConvention.CamelCase
import utopia.coder.model.scala.{DeclarationDate, Parameter}
import utopia.coder.model.scala.datatype.{Reference, ScalaType}
import utopia.coder.model.scala.declaration.{ClassDeclaration, File}
import utopia.coder.model.scala.declaration.PropertyDeclarationType.{ComputedProperty, LazyValue}
import utopia.coder.vault.model.data
import utopia.coder.vault.model.data.VaultProjectSetup
import utopia.coder.vault.util.VaultReferences._
import utopia.flow.util.StringExtensions._

import scala.io.Codec

/**
 * Used for writing database configuration classes, which are used with generic traits
 *
 * @author Mikko Hilpinen
 * @since 14.06.2024, v1.11
 */
object DbConfigWriter
{
	// ATTRIBUTES   -----------------------
	
	private val dbConfigSuffix = Name("DbConfig", "DbConfigurations", CamelCase.capitalized)
	private val propNameSuffix = Name("propName", "propNames", CamelCase.lower)
	
	
	// OTHER    ---------------------------
	
	/**
	 * Writes a XDbConfig class, which is used with defining database interactions with generic traits
	 * @param classToWrite Written class / trait this configuration is used with
	 * @param codec Implicit codec used to determine file formatting
	 * @param setup Implicit project setup
	 * @param naming Implicit naming rules
	 * @return Reference to the generated class. Failure if file-writing failed.
	 */
	def apply(classToWrite: data.Class)
	         (implicit codec: Codec, setup: VaultProjectSetup, naming: NamingRules) =
	{
		val propNames = classToWrite.dbProperties
			.map { prop =>
				prop -> Parameter((prop.name + propNameSuffix).prop, ScalaType.string, prop.modelName.quoted,
					description = s"Name of the database property matching ${ prop.name }")
			}
			.toVector
		val props = propNames.map { case (prop, propParam) =>
			LazyValue(prop.name.prop, Set(vault.dbProp),
				description = s"Access to ${ prop.name } database property name and column")(
				s"${ vault.dbProp.target }(table, ${ propParam.name })")
		}
		
		val dbConfig = ClassDeclaration(
			name = (classToWrite.name + dbConfigSuffix).className,
			constructionParams = Parameter("table", vault.table,
				description = "Table operated using this configuration") +: propNames.map { _._2 },
			properties = props,
			description = s"Configuration class used for specifying how $classToWrite DB data is to be interacted with",
			author = classToWrite.author,
			since = DeclarationDate.versionedToday,
			isCaseClass = true
		)
		
		File(setup.databasePackage/"config"/classToWrite.packageName, dbConfig).write()
	}
}
