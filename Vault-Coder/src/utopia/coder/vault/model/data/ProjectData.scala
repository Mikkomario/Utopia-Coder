package utopia.coder.vault.model.data

import utopia.coder.model.data.{Name, NamingRules, ProjectPaths}
import utopia.coder.model.enumeration.NamingConvention.CamelCase
import utopia.coder.model.scala.Package
import utopia.coder.vault.model.datatype.PropertyType
import utopia.coder.vault.model.enumeration.Mutability
import utopia.coder.vault.model.enumeration.Mutability.Immutable
import utopia.flow.collection.immutable.Empty
import utopia.flow.util.Version

object ProjectData
{
	// ATTRIBUTES   ------------------------
	
	/**
	 * Default project settings (mostly empty)
	 */
	lazy val default = apply()
}

/**
 * Contains information which affects multiple modules / sub-projects
 * @param name Name of this project
 * @param modulePaths For each module in this project, the paths to module-specific input, output & source directories
 * @param version Current project version - Used as the minimum version in each module.
 *                Default = None = (default) version not specified.
 * @param rootPackage Package common to all project modules - All module packages will be prefixed with this path.
 *                    Default = empty = each module specifies its own full package paths.
 * @param databaseName Name of the database used in this project. Modules may override this.
 *                     Default = empty = no default database specified.
 * @param namingRules Naming rules to use by default. Modules may override this.
 *                    Default = [[NamingRules.default]].
 * @param defaultMutability Mutability to apply by default. May be overridden. Default = immutable.
 * @param customDataTypes Custom data types used throughout this project.
 *                        May be referenced from all sub-modules.
 *                        Specified as a map, where keys are type names which may be referenced and values are the
 *                        actual referenced data types.
 *                        Default = empty.
 * @param author Author to assign to each generated class by default (may be overridden).
 *               Default = empty.
 * @param prefixColumns Whether column names should be prefixed with a unique table-specific prefix.
 *                      May be overridden in each module.
 *                      Default = false.
 * @author Mikko Hilpinen
 * @since 27.07.2024, v1.11
 */
case class ProjectData(name: Name = Name("Project", CamelCase.capitalized), modulePaths: Seq[ProjectPaths] = Empty,
                       version: Option[Version] = None, rootPackage: Package = Package.empty,
                       databaseName: Option[Name] = None, namingRules: NamingRules = NamingRules.default,
                       defaultMutability: Mutability = Immutable,
                       customDataTypes: Map[String, PropertyType] = Map(), author: String = "",
                       prefixColumns: Boolean = false)
