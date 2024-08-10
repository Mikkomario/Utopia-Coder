package utopia.coder.vault.controller.reader

import utopia.bunnymunch.jawn.JsonBunny
import utopia.coder.controller.parsing.file.InputFiles
import utopia.coder.model.data
import utopia.coder.model.data.{Name, NamingRules, ProjectPaths}
import utopia.coder.model.enumeration.NameContext.{ClassName, ClassPropName, ColumnName, DatabaseName, EnumName, EnumValueName, FunctionName, Header, TableName}
import utopia.coder.model.enumeration.NamingConvention
import utopia.coder.model.enumeration.NamingConvention.{CamelCase, UnderScore}
import utopia.coder.model.scala.Package
import utopia.coder.model.scala.code.CodePiece
import utopia.coder.vault.model.data.{Class, CombinationData, DbPropertyOverrides, Enum, EnumerationValue, Instance, ModuleData, ProjectData, Property}
import utopia.coder.vault.model.datatype.StandardPropertyType.BasicPropertyType.IntNumber
import utopia.coder.vault.model.datatype.StandardPropertyType.{ClassReference, EnumValue, Text}
import utopia.coder.vault.model.datatype.{CustomPropertyType, PropertyType, StandardPropertyType}
import utopia.coder.vault.model.enumeration.CombinationType.{Combined, MultiCombined, PossiblyCombined}
import utopia.coder.vault.model.enumeration.IntSize.Default
import utopia.coder.vault.model.enumeration.{CombinationType, IntSize, Mutability}
import utopia.flow.collection.CollectionExtensions._
import utopia.flow.collection.immutable.Empty
import utopia.flow.error.DataTypeException
import utopia.flow.generic.casting.ValueConversions._
import utopia.flow.generic.model.immutable.Model
import utopia.flow.generic.model.mutable.DataType.{ModelType, StringType, VectorType}
import utopia.flow.operator.equality.EqualsExtensions._
import utopia.flow.parse.file.FileExtensions._
import utopia.flow.util.StringExtensions._
import utopia.flow.util.logging.Logger
import utopia.flow.util.{UncertainBoolean, Version}

import java.io.FileNotFoundException
import java.nio.file.Path
import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

/**
  * Used for reading class data from a .json file
  * @author Mikko Hilpinen
  * @since 3.9.2021, v0.1
  */
object ClassReader
{
	// OTHER    -----------------------------------
	
	/**
	 * Reads project or module data from the specified json file
	 * @param projectOrModuleFilePath Path to the json file to read
	 * @param projectRootPath Path to the project root directory (call-by-name)
	 * @param log Implicit logging implementation used for handling non-critical failures
	 * @return Either
	 *              - Left: Read project's data
	 *              - Right: Read module data
	 *
	 *         Failure if json-parsing failed.
	 */
	def projectOrModuleFrom(projectOrModuleFilePath: Path, projectRootPath: => Path)(implicit log: Logger) =
		JsonBunny(projectOrModuleFilePath).map { v =>
			val root = v.getModel
			if (root.contains("classes"))
				Right(moduleFrom(projectOrModuleFilePath, root, ProjectData.default))
			else
				Left(projectFrom(root, projectRootPath))
		}
	
	/**
	  * Reads module data from a .json file
	  * @param path Path to the file to read
	  * @param project Project settings to apply as defaults (default = empty / default project settings)
	 * @param log Implicit logging implementation used for handling non-critical failures
	 * @return Read module data. Failure if file reading or class parsing failed.
	  */
	def moduleFrom(path: Path, project: ProjectData = ProjectData.default)(implicit log: Logger): Try[ModuleData] =
		JsonBunny(path).flatMap { _.tryModel.map { root => moduleFrom(path, root, project) } }
	
	/**
	 * Reads the referenced modules from a project file path
	 * @param path Path to a project json file
	 * @param projectRootPath Path to the project root directory
	 * @param log Implicit logging implementation
	 * @return Paths of each of the targeted project's modules
	 */
	def moduleReferencesFrom(path: Path, projectRootPath: Path)(implicit log: Logger): Try[Vector[ProjectPaths]] =
		JsonBunny(path).flatMap { _.tryModel.map { moduleReferencesFrom(_, projectRootPath) } }
	
	private def projectFrom(root: Model, rootPath: Path)(implicit log: Logger) = {
		// Reads basic project information
		implicit val namingRules: NamingRules = NamingRules(root("naming").getModel).value
		val author = root("author").getString
		val rootPackage = Package(root("base_package", "package").getString)
		val projectName = Header.from(root, disableGeneric = true)
			.orElse { root("name", "project").string.map { Name.interpret(_, namingRules(Header)) } }
			.getOrElse {
				rootPackage.parts.lastOption match {
					case Some(lastPart) => Name.interpret(lastPart, CamelCase.lower)
					case None => Name("Project", CamelCase.capitalized)
				}
			}
		val databaseName = DatabaseName.from(root, disableGeneric = true)
		val customTypes = customTypesFrom(root)
		
		// Reads module paths
		val modules = moduleReferencesFrom(root, rootPath)
		
		ProjectData(projectName, modules, root("version").string.map { Version(_) }, rootPackage, databaseName,
			namingRules, Mutability.forIsMutable(root("mutable_props", "mutable").getBoolean), customTypes,
			author, root("prefix_columns").getBoolean)
	}
	
	private def moduleReferencesFrom(root: Model, rootPath: Path)(implicit log: Logger) =
		root("modules").getVector.flatMap { v =>
			val parsed = v.castTo(ModelType, StringType) match {
				case Left(modelVal) => modelVal.tryModel.flatMap(ProjectPaths.apply).logToOption
				case Right(stringVal) => Some(ProjectPaths(stringVal.getString))
			}
			parsed.map { _.under(rootPath) }
		}
	
	private def moduleFrom(path: Path, root: Model, project: ProjectData)(implicit log: Logger): ModuleData = {
		implicit val namingRules: NamingRules = NamingRules.from(root("naming").getModel, project.namingRules)
		
		val version = root("version").string.map { Version(_) } match {
			case Some(version) =>
				// If project version is also defined, selects the latest version
				Some(project.version match {
					case Some(projectVersion) => version max projectVersion
					case None => version
				})
			case None => project.version
		}
		val author = root("author").stringOr(project.author)
		val basePackage = project.rootPackage/Package(root("base_package", "package").getString)
		val modelPackage = basePackage/root("model_package", "package_model").stringOr("model")
		val dbPackage = basePackage/
			root("db_package", "database_package", "package_db", "package_database").stringOr("database")
		val moduleName = Header.from(root, disableGeneric = true)
			.orElse { root("name", "project").string.map { Name.interpret(_, namingRules(Header)) } }
			.getOrElse {
				basePackage.parts.lastOption match {
					case Some(lastPart) => Name.interpret(lastPart, CamelCase.lower)
					case None =>
						dbPackage.parent.parts.lastOption
							.orElse { dbPackage.parts.lastOption }
							.map { Name.interpret(_, CamelCase.lower) }
							.getOrElse { project.name }
				}
			}
		
		val databaseName = DatabaseName.from(root, disableGeneric = true).orElse(project.databaseName)
		val enumPackage = modelPackage / "enumeration"
		val enumerations = enumsFrom(root, enumPackage, author)
		val referencedEnumerations = root("referenced_enums", "referenced_enumerations").getVector
			.flatMap { _.string }
			.map { enumPath =>
				val (packagePart, enumName) = enumPath.splitAtLast(".").toTuple
				Enum(enumName, packagePart, Empty)
			}
		val allEnumerations = enumerations ++ referencedEnumerations
		
		val customTypes = project.customDataTypes ++ customTypesFrom(root)
		val defaultMutability = root("mutable_props", "mutable").boolean match {
			case Some(customMutability) => Mutability.forIsMutable(customMutability)
			case None => project.defaultMutability
		}
		val usePrefixes = root("prefix_columns").booleanOr(project.prefixColumns)
		
		// Expects the classes property to contain an object where keys are package names and values are
		// either references to other files or arrays containing class objects
		// Each entry in 'classData' contains 4 values:
		//      1) Read class
		//      2) Unfinished combination data (0-n)
		//      3) Class instances (0-n)
		//      4) Class parent names (0-n)
		val classData = root("classes").getModel.properties.flatMap { packageAtt =>
			packageAtt.value.castTo(VectorType, StringType) match {
				// Case: Class object array input => Parses the objects
				case Left(vectorVal) =>
					vectorVal.getVector.flatMap { _.model }
						.map { classFrom(_, packageAtt.name, allEnumerations, customTypes, author) }
				// Case: Reference input => Identifies and parses the referenced file
				case Right(stringVal) =>
					// If the specified path is relative, assumes that it is referred from this file's parent
					val subPath: Path = stringVal.getString
					val targetPath = if (subPath.isAbsolute) subPath else path.parent/subPath
					InputFiles.versionedFileFrom(targetPath, "json") match {
						// Case: File found => Parses it
						case Some(referencedPath) =>
							JsonBunny(referencedPath) match {
								// Case: Parsing succeeded => Processes parsed content
								case Success(parsed) =>
									parsed.getVector.flatMap { _.model }
										.map { classFrom(_, packageAtt.name, allEnumerations, customTypes, author) }
								// Case: Parsing failed => Logs a warning
								case Failure(error) =>
									log(error, s"Failed to parse the json file referred as package ${
										packageAtt.name}, pointing to file ${referencedPath.absolute}")
									Empty
							}
						// Case: File not found => Prints a warning
						case None =>
							log(new FileNotFoundException(s"Warning: $subPath for package ${
								packageAtt.name} is not a valid reference from ${path.parent.absolute}"))
							Empty
					}
			}
		}
		// Compiles the full class hierarchy
		val classes = compileClassHierarchy(
			classData.map { case (c, _, _, parentNames, propRefs) => (c, parentNames, propRefs) })
		
		// Processes the proposed combinations
		val combinations = classData.flatMap { case (parentClass, combos, _, _, _) =>
			combos.flatMap { combo =>
				// Finds the child class (child name match)
				classes.find { c => c.name.variants.exists { _ ~== combo.childName } }.map { childClass =>
					// Determines the combination type
					val combinationType = combo.comboTypeName.ifNotEmpty.flatMap(CombinationType.interpret)
						.getOrElse {
							if (combo.childrenDefinedAsPlural)
								MultiCombined
							else if ((combo.childName !~== childClass.name.singular) &&
								(combo.childName ~== childClass.name.plural))
								MultiCombined
							else if (combo.alwaysLinked.isCertainlyTrue)
								Combined
							else
								PossiblyCombined
						}
					def parseAlias(alias: String, pluralForm: String, pluralIsDefault: Boolean = false) = {
						// Finds the alias name property, if defined
						alias.ifNotEmpty.map { alias =>
							val namingConvention = NamingConvention.of(alias, namingRules(ClassPropName))
							// Finds the plural form, if defined
							pluralForm.ifNotEmpty match {
								// Case: Plural form is defined
								case Some(pluralAlias) => Name(alias, pluralAlias, namingConvention)
								// Case: Only one form is defined
								case None =>
									// Case: Expects the main alias to be pluralized => Uses plural as singular, also
									if (pluralIsDefault)
										Name(alias, alias, namingConvention)
									// Case: Expects the main alias to be singular => Auto-pluralizes
									else
										Name.interpret(alias, namingConvention)
							}
						}
					}
					// Determines combination name
					val childAlias = parseAlias(combo.childAlias, combo.pluralChildAlias,
						combinationType.isOneToMany)
					val parentAlias = parseAlias(combo.parentAlias, combo.pluralParentAlias)
					val comboName = combo.name.ifNotEmpty match {
						// Case: Custom name has been given
						case Some(name) =>
							val namingConvention = NamingConvention.of(name, namingRules(ClassName))
							combo.pluralName.ifNotEmpty match {
								// Case: Plural form has also been specified
								case Some(pluralName) => Name(name, pluralName, namingConvention)
								// Case: No plural form specified => Auto-pluralizes
								case None => Name.interpret(name, namingConvention)
							}
						// Case: No name specified => Generates a name
						case None =>
							val childPart = childAlias.getOrElse(childClass.name)
							val base = parentClass.name + data.Name("with", "with", CamelCase.lower)
							// Case: There are multiple children => Plural child name is used in all name forms
							// E.g. CarWithTires and CarsWithTires
							if (combinationType.isOneToMany)
								base + Name(childPart.plural, childPart.plural, childPart.style)
							// Case: There are 0-1 children => Both parent and child name follow same pluralization
							// E.g. CarWithMotor => CarsWithMotors
							else
								Name(s"${base.singular}${ childPart.singularIn(base.style) }",
									s"${ base.plural }${ childPart.pluralIn(base.style) }", base.style)
					}
					val isAlwaysLinked = combo.alwaysLinked.getOrElse {
						combinationType match {
							case Combined => true
							case _ => false
						}
					}
					CombinationData(combinationType, comboName, parentClass, childClass, parentAlias,
						childAlias, combo.doc, isAlwaysLinked)
				}
			}
		}
		// Returns class instances, also
		val instances = classData.flatMap { _._3 }
		ModuleData(moduleName, modelPackage, dbPackage, databaseName,
			enumerations, classes, combinations, instances, namingRules, version, defaultMutability,
			!root("models_without_vault").getBoolean, usePrefixes)
	}
	
	private def customTypesFrom(root: Model)(implicit log: Logger) = {
		root("data_types", "types").getModel.properties
			.flatMap { typeProperty =>
				typeProperty.value.model
					.toTry { new DataTypeException(
						s"Custom data types must be presented as json objects. ${ typeProperty.name }/'${
							typeProperty.value }' is not a model.") }
					.flatMap(CustomPropertyType.apply)
					.logToOption
					.map { typeProperty.name.toLowerCase -> _ }
			}
			.toMap
	}
	
	private def compileClassHierarchy(classesToResolve: Seq[(Class, Seq[String], Map[Property, Seq[String]])])
	                                 (implicit naming: NamingRules): Seq[Class] =
	{
		// Checks which classes have references that need to be resolved
		val (needResolving, availableParents) = classesToResolve
			.divideWith { case (classToResolve, parentNames, propertyReferences) =>
				if (parentNames.isEmpty)
					Right(classToResolve.name.className -> classToResolve)
				else
					Left((classToResolve, parentNames, propertyReferences))
			}
		// Case: No class references exist => No resolving required
		if (needResolving.isEmpty)
			classesToResolve.map { _._1 }
		// Case: Class references need resolving => Resolves missing references
		else
			compileClassHierarchy(needResolving, availableParents.toMap)
	}
	
	@tailrec
	private def compileClassHierarchy(unresolvedClasses: Iterable[(Class, Seq[String], Map[Property, Seq[String]])],
	                                  availableParents: Map[String, Class])
	                                 (implicit naming: NamingRules): Seq[Class] =
	{
		// Attempts to resolve all the unresolved cases by matching with available parent classes
		val (remainsUnresolved, resolved) = unresolvedClasses
			.divideWith { case (classToResolve, parentNames, propertyReferences) =>
				parentNames.findForAll(availableParents.get) match {
					case Some(parents) => Right(classToResolve.extending(parents, propertyReferences))
					case None => Left((classToResolve, parentNames, propertyReferences))
				}
			}
		
		// Case: All resolved => Returns
		if (remainsUnresolved.isEmpty)
			resolved ++ availableParents.valuesIterator
		// Case: Resolve failed => Logs a warning and returns without resolving the classes
		else if (resolved.isEmpty) {
			println(s"Failed to resolve ${ remainsUnresolved.size } class extension references:")
			remainsUnresolved.foreach { case (classToResolve, parentNames, _) =>
				println(s"\t- ${ classToResolve.name.className } extends ${ parentNames.mkString(" with ") }")
			}
			availableParents.valuesIterator.toVector ++ unresolvedClasses.view.map { _._1 }
		}
		// Case: Some classes remain unresolved => Performs another iteration
		else
			compileClassHierarchy(remainsUnresolved, availableParents ++ resolved.map { c => c.name.className -> c })
	}
	
	private def enumsFrom(root: Model, enumPackage: Package, author: String)(implicit namingRules: NamingRules) = {
		root("enumerations", "enums").castTo(VectorType, ModelType) match {
			// Case: Array model -based enumeration syntax => parses each object into an enumeration
			case Left(enumsValue) =>
				enumsValue.getVector.flatMap { _.model }.map { model =>
					val name = EnumName.from(model).getOrElse { Name("UnnamedEnum") }
					val idName = Name.interpret(model("index_name", "index_property", "index_prop", "id_name", "id_property",
						"id_prop", "index", "id").stringOr("id"), ClassPropName.style)
					val underScoreIdName = idName.singularIn(UnderScore)
					val values = model("values", "vals").getVector.zipWithIndex.map { case(v, index) =>
						v.model match {
							case Some(model) => enumValueFrom(model, index, underScoreIdName)
							case None => EnumerationValue(v.getString, (index + 1).toString)
						}
					}
					val idType = model("index_type", "id_type", "type").string
						.flatMap { custom =>
							StandardPropertyType.interpret(custom, model("id_length", "id_max", "length").int,
								Some(name.singular))
						}
						// The default ID type is integer, with size enough to fit all values (i.e. tiny)
						.getOrElse { IntNumber(IntSize.fitting(values.size).getOrElse(Default)) }
					val default = model("default").string
						.flatMap { defaultName => values.find { _.name ~== defaultName } }
					val actualPackage = model("package").string match {
						case Some(pck) => Package(pck)
						case None => enumPackage
					}
					Enum(name, actualPackage, values, default, idName, idType, model("doc").getString, author)
				}
			// Case: Old-school enumeration syntax using an object
			case Right(enumsModelValue) =>
				enumsModelValue.getModel.properties.map { enumAtt =>
					Enum(enumAtt.name.capitalize, enumPackage,
						enumAtt.value.getVector.zipWithIndex.map { case (v, index) =>
							v.model match {
								case Some(model) => enumValueFrom(model, index)
								case None => EnumerationValue(v.getString, (index + 1).toString)
							}
						},
						author = author)
				}
		}
	}
	
	private def enumValueFrom(model: Model, index: Int, underScoreIdPropName: String = "id")
	                         (implicit naming: NamingRules) =
	{
		val customId: Option[CodePiece] = model(underScoreIdPropName, "key", "id")
		val id = customId.getOrElse(CodePiece((index + 1).toString))
		val name = EnumValueName.from(model)
			// Uses the id as a backup value name
			.getOrElse {
				Name.interpret(if (id.text.headOption.exists { _.isLetter }) id.text else s"_${id.text}",
					naming(EnumValueName))
			}
		EnumerationValue(name, id, model("doc", "description").getString)
	}
	
	private def classFrom(classModel: Model, packageName: String, enumerations: Iterable[Enum],
	                      customTypes: Map[String, PropertyType], defaultAuthor: String)
	                     (implicit naming: NamingRules) =
	{
		// Determines class name
		// Uses table name as a backup for class name
		val tableName = TableName.from(classModel, disableGeneric = true)
		val name = ClassName.from(classModel).orElse(tableName)
			.getOrElse { data.Name("Unnamed", naming(ClassName)) + packageName }
		
		// Reads properties
		val (properties, propertyReferences) = classModel("properties", "props").getVector.flatMap { _.model }
			.splitMap { model =>
				val (prop, references) = propertyFrom(model, enumerations, name, customTypes)
				prop -> (prop -> references)
			}
		val idName = classModel("id_name", "id").string.map { Name.interpret(_, ClassPropName.style) }
		
		// Finds the combo indices
		// The indices in the document are given as property names, but here they are converted to column names
		val comboIndexColumnNames: Seq[Seq[String]] = classModel("index", "combo_index").vector
			.map[Vector[Vector[Name]]] { v =>
				Vector(v.flatMap { _.string }.map { s => Name.interpret(s, ClassPropName.style) })
			}
			.orElse {
				classModel("indices", "combo_indices").vector
					.map { vectors =>
						vectors.map[Vector[Name]] { vector =>
							vector.getVector.flatMap { _.string }.map { s => Name.interpret(s, ClassPropName.style) }
						}
					}
			}
			.getOrElse(Empty)
			.map { combo =>
				combo.flatMap { propName =>
					properties.view.flatMap { _.dbProperties }.find { _.name ~== propName }.map { _.columnName }
				}
			}
			.filter { _.nonEmpty }
		
		// Checks whether descriptions are supported for this class
		val descriptionLinkColumnName: Option[Name] =
			classModel("description_link", "desc_link", "description_link_column")
				.string match {
				case Some(n) => Some(Name.interpret(n, ClassPropName.style))
				case None =>
					if (classModel("described", "is_described").getBoolean)
						Some(name + "id")
					else
						None
			}
		
		// Reads combination-related information
		val comboInfo = (classModel("combination", "combo").model match {
			case Some(comboModel) => Vector(comboModel)
			case None => classModel("combinations", "combos").getVector.flatMap { _.model }
		}).flatMap { comboModel =>
			comboModel("child", "children").string.map { childName =>
				RawCombinationData(childName, comboModel("parent_alias", "alias_parent").getString,
					comboModel("parent_alias_plural", "plural_parent_alias", "alias_parent_plural",
						"plural_alias_parent").getString,
					comboModel("child_alias", "alias_child").getString,
					comboModel("child_alias_plural", "plural_child_alias", "alias_child_plural", "plural_alias_child",
						"children_alias", "alias_children").getString,
					comboModel("type").getString,
					comboModel("name").getString.capitalize, comboModel("name_plural").getString.capitalize,
					comboModel("doc").getString, comboModel("always_linked", "is_always_linked").boolean,
					comboModel.containsNonEmpty("children"))
			}
		}
		
		val hasManyCombos = classModel("has_many_combos", "write_combo_trait", "many_combos")
			.booleanOr(comboInfo.hasSize > 1)
		val readClass = new Class(name, tableName.map { _.table }, idName.getOrElse(Class.defaultIdName),
			properties, packageName, classModel("access_package", "sub_package", "access").getString,
			comboIndexColumnNames, descriptionLinkColumnName,
			// Doesn't add extensions at this point. These are inserted later.
			parents = Empty,
			description = classModel("doc").getString,
			author = classModel("author").stringOr(defaultAuthor),
			useLongId = classModel("use_long_id").getBoolean,
			// Writes generic access point if this class has combinations, or if explicitly specified
			writeGenericAccess = classModel("has_combos", "generic_access", "tree_inheritance")
				.booleanOr(comboInfo.nonEmpty) || hasManyCombos,
			writeCommonComboTrait = hasManyCombos,
			isGeneric = classModel("is_generic", "generic", "is_trait", "trait").getBoolean)
		
		// Parses class instances if they are present
		val instances = classModel("instance").model match {
			case Some(instanceModel) => Vector(instanceFrom(instanceModel, readClass))
			case None => classModel("instances").getVector.flatMap { _.model }.map { instanceFrom(_, readClass) }
		}
		
		// Includes referred parent class and property names in the return value
		val parentReferences = classModel("extends", "parent", "parents").getVector.map { _.getString }
		
		(readClass, comboInfo, instances, parentReferences, propertyReferences.toMap)
	}
	
	private def propertyFrom(propModel: Model, enumerations: Iterable[Enum], className: Name,
	                         customTypes: Map[String, PropertyType])
	                        (implicit naming: NamingRules) =
	{
		// May use column name as a backup name
		val specifiedName = ClassPropName.from(propModel).orElse { ColumnName.from(propModel, disableGeneric = true) }
		
		// val name = propModel("name").getString
		// val columnName = propModel("column_name").stringOr(NamingUtils.camelToUnderscore(name))
		val tableReference = propModel("references", "ref").string.map { ref =>
			val (tablePart, columnPart) = ref.splitAtFirst("(").toTuple
			tablePart -> columnPart.untilLast(")").ifNotEmpty.map { Name.interpret(_, ColumnName.style) }
		}
		val length = propModel("length", "len").int
		val baseDataType = propModel("type").string.flatMap { typeName =>
			val lowerTypeName = typeName.toLowerCase
			val innermostTypeName = lowerTypeName.afterLast("[").ifNotEmpty match {
				case Some(afterBracket) => afterBracket.untilFirst("]")
				case None => lowerTypeName
			}
			// Checks for a custom data type
			customTypes.get(innermostTypeName) match {
				case Some(customType) =>
					Some(if (lowerTypeName.untilFirst("[").contains("option")) customType.optional else customType)
				case None =>
					// Checks for an enumeration reference
					val enumType = {
						if (lowerTypeName.contains("enum")) {
							val enumName = lowerTypeName.afterFirst("enum")
								.afterFirst("[").untilFirst("]")
							enumerations.find { _.name ~== enumName }
						}
						else
							None
					}
					enumType match {
						// Case: Enumeration reference
						case Some(enumType) =>
							val baseType = EnumValue(enumType)
							Some(if (lowerTypeName.contains("option")) baseType.optional else baseType)
						// Case: Standard data type
						case None => StandardPropertyType.interpret(typeName, length, specifiedName)
					}
			}
		}
		// Applies the possible table reference
		val actualDataType = tableReference match {
			// Case: Reference type
			case Some((tableName, columnName)) =>
				ClassReference(tableName, columnName.getOrElse(Class.defaultIdName), baseDataType.getOrElse(IntNumber()))
			// Case: Other type
			case None =>
				baseDataType.getOrElse {
					// The default type is string for properties with a specified maximum length and int for others
					length match {
						case Some(length) => Text(length)
						case None => IntNumber(Default)
					}
				}
		}
		
		val extendedPropNames = propModel("extends", "extend", "parents", "parent", "from").getVector.map { _.getString }
		val name: Name = specifiedName.getOrElse {
			extendedPropNames.headOption match {
				case Some(parentName) => Name.interpret(parentName, naming(ClassPropName))
				case None => actualDataType.defaultPropertyName
			}
		}
		
		val rawDoc = propModel("doc").string.filter { _.nonEmpty }
		val doc = rawDoc.getOrElse { actualDataType.writeDefaultDescription(className, name) }
		
		val default: CodePiece = propModel("default", "def")
		// A property either lists database-interaction information within itself (single-column use-case) or
		// within a special property named "parts" (multi-column use-case)
		val partModels = propModel("parts").getVector.flatMap { _.model }
		val dbPropertyOverrides = {
			if (partModels.isEmpty && actualDataType.isSingleColumn)
				Vector(dbPropertyOverridesFrom(propModel, default))
			else
				partModels.map { dbPropertyOverridesFrom(_, readName = true) }
		}
		
		// If "with" is defined, may automatically name "in" by pluralizing it
		val withAccessName = propModel("with").getString
		val inAccessName = propModel("in").string.getOrElse {
			if (withAccessName.nonEmpty)
				Name.interpret(withAccessName, naming(FunctionName)).plural
			else
				""
		}
		
		val property = Property(name, actualDataType, default, dbPropertyOverrides,
			propModel("mutable").boolean.map(Mutability.forIsMutable), Empty, withAccessName, inAccessName, doc)
		
		// Includes extension references in the return value
		property -> extendedPropNames
	}
	
	private def dbPropertyOverridesFrom(model: Model, default: CodePiece = CodePiece.empty, readName: Boolean = false)
	                                   (implicit naming: NamingRules) =
	{
		// Name-processing may be skipped
		val columnName = ColumnName.from(model, disableGeneric = true)
		val name = if (readName) ClassPropName.from(model).orElse(columnName) else None
		val finalColumnName = columnName match {
			case Some(name) => name.column
			case None => ""
		}
		
		// A custom default value may be carried over to sql under some circumstances
		val sqlDefault = model("sql_default", "sql_def").stringOr { default.toSql.getOrElse("") }
		
		val limit = {
			// Attempts to parse the limit from the "limit" property
			val raw = model("length_rule", "length_limit", "limit", "max_length", "length_max", "max")
			val base = raw.int
				// Uses type property upper limit as a backup
				.orElse { model("type").getString.afterFirst("(").untilFirst(")").afterFirst("-").int } match
			{
				case Some(max) => s"to $max"
				case None => raw.getString
			}
			// Adds cropping, if appropriate
			if (model("allow_crop", "crop").getBoolean) {
				if (base.isEmpty) "crop" else s"$base or crop"
			}
			else
				base
		}
		
		DbPropertyOverrides(name, finalColumnName, sqlDefault, limit, model("indexed", "index", "is_index").boolean)
	}
	
	private def instanceFrom(model: Model, parentClass: Class)(implicit naming: NamingRules) =
	{
		// Matches model properties against class properties
		val properties = ClassPropName.use { implicit c =>
			model.nonEmptyProperties.flatMap { att =>
				val attName = Name.contextual(att.name)
				parentClass.properties.find { _.name ~== attName }.map { _ -> att.value }
			}.toMap
		}
		Instance(parentClass, properties, model("id"))
	}
	
	
	// NESTED   --------------------------------------
	
	private case class RawCombinationData(childName: String, parentAlias: String, pluralParentAlias: String,
	                                      childAlias: String, pluralChildAlias: String, comboTypeName: String,
	                                      name: String, pluralName: String, doc: String,
	                                      alwaysLinked: UncertainBoolean, childrenDefinedAsPlural: Boolean)
}
