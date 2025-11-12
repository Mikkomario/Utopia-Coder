package utopia.coder.vault.main

import utopia.coder.controller.app.CoderAppLogic
import utopia.coder.controller.parsing.file.InputFiles
import utopia.coder.model.data._
import utopia.coder.model.enumeration.NameContext.FileName
import utopia.coder.model.enumeration.NamingConvention.CamelCase
import utopia.coder.model.scala.Package
import utopia.coder.model.scala.datatype.Reference
import utopia.coder.vault.controller.reader.ClassReader
import utopia.coder.vault.controller.writer.database._
import utopia.coder.vault.controller.writer.database.sql.{ColumnLengthRulesWriter, InsertsWriter, SqlWriter, TablesWriter}
import utopia.coder.vault.controller.writer.documentation.{DocumentationWriter, TableDescriptionsWriter}
import utopia.coder.vault.controller.writer.model.{CombinedModelWriter, DescribedModelWriter, EnumerationWriter, ModelWriter}
import utopia.coder.vault.model.data.reference.{ClassReferences, GenericClassReferences}
import utopia.coder.vault.model.data.{Class, CombinationData, ModuleData, VaultProjectSetup}
import utopia.coder.vault.util.Common
import utopia.flow.collection.CollectionExtensions._
import utopia.flow.collection.immutable.{Empty, Pair, Single}
import utopia.flow.collection.template.MapAccess
import utopia.flow.parse.file.FileExtensions._
import utopia.flow.time.Today
import utopia.flow.util.EitherExtensions._
import utopia.flow.util.TryExtensions._
import utopia.flow.util.console.{ArgumentSchema, CommandArguments}
import utopia.flow.util.logging.Logger
import utopia.flow.util.{Mutate, Version}
import utopia.flow.view.immutable.caching.Lazy

import java.nio.file.Path
import java.time.LocalTime
import scala.concurrent.ExecutionContext
import scala.io.Codec
import scala.util.{Failure, Success, Try}

/**
  * The main Vault-Coder application logic which reads one or more input files and generates class files,
  * tables, documentation, etc.
  * @author Mikko Hilpinen
  * @since 4.9.2021, v0.1
  */
object MainAppLogic extends CoderAppLogic
{
	// ATTRIBUTES   ----------------------
	
	private implicit val codec: Codec = Codec.UTF8
	
	// Options for target type selection
	private val _class = 1
	private val _package = 2
	private val _enums = 3
	private val _all = 4
	
	override val name = "main"
	
	
	// IMPLEMENTED  ----------------------
	
	override protected implicit def exc: ExecutionContext = Common.exc
	override protected implicit def log: Logger = Common.log
	
	override protected def projectsStoreLocation: Path = Common.projectsPath
	override protected def supportsAlternativeMergeRoots: Boolean = true
	
	// Adds the following arguments:
	//      1. "no combos" -argument
	//      2. "module" argument
	//      3. Targeting -flag
	override def argumentSchema = {
		val original = super.argumentSchema
		val firstFlagIndex = original.findIndexWhere { _.isFlag }.getOrElse { original.size }
		
		val module = ArgumentSchema("module", help = "Name of the targeted project module (default = all)")
		val noCombos = ArgumentSchema.flag("no-combos", "NC", help = "Whether combo-class writing should be disabled")
		val targeting = ArgumentSchema.flag("no-targeting", "NT",
			help = "Whether older access implementations should be used instead of targeting access classes")
		
		((original.take(firstFlagIndex) :+ module) ++ original.drop(firstFlagIndex)) ++ Pair(noCombos, targeting)
	}
	
	override protected def run(args: CommandArguments, paths: LazyProjectPaths, filter: Lazy[Option[Filter]],
	                           targetGroup: Option[String]): Boolean =
	{
		lazy val targetType = targetGroup match {
			case Some(specifiedGroup) =>
				specifiedGroup.toLowerCase match {
					case "class" | "classes" => _class
					case "package" | "packages" => _package
					case "enums" | "enumerations" | "enum" | "enumeration" => _enums
					case "all" => _all
					case other =>
						log(s"Warning: Unrecognized target type '$other' => Targets all types")
						_all
				}
			case None => _all
		}
		
		if (paths.input.notExists) {
			println(s"Looks like no data can be found from ${ paths.input }. Please try again with different input.")
			false
		}
		else {
			if (paths.input.fileType.toLowerCase != "json")
				log(s"Warning: Expect file type is .json. Specified file is of type .${ paths.input.fileType }")
			
			ClassReader.projectOrModuleFrom(paths.input, paths.root) match {
				case Success(Left(project)) =>
					args("module").string match {
						// Case: Targeting a specific project module
						case Some(moduleName) =>
							project.modulePaths.findMap { paths =>
								InputFiles.versionedFileOrDirectoryFrom(paths.input, "json").flatMap { inputPath =>
									ClassReader.moduleFrom(inputPath, project)
										.logWithMessage(s"Failed to parse module data from $inputPath")
										.filter { _.moduleName ~== moduleName }
								}
							} match {
								case Some(module) => filterAndWriteModule(module, paths, targetType, filter.value, args)
								case None =>
									println(s"None of the listed modules matched '$moduleName'")
									false
							}
						
						// Case: Targeting the whole project
						case None =>
							val modules = project.modulePaths.flatMap { modulePaths =>
								InputFiles.versionedFileOrDirectoryFrom(modulePaths.input, "json").flatMap { inputPath =>
									ClassReader.moduleFrom(inputPath, project)
										.logWithMessage(s"Failed to parse module data from $inputPath")
										.map { _ -> modulePaths }
								}
							}
							filterAndWriteModules(project.name, project.version, paths.output, project.namingRules, modules,
								targetType, filter.value, args, project.prefixColumns)
					}
					
				case Success(Right(module)) => filterAndWriteModule(module, paths, targetType, filter.value, args)
				case Failure(error) =>
					log(error, "Class reading failed. Please make sure the file is in correct format.")
					false
			}
		}
	}
	
	
	// OTHER    -------------------
	
	/**
	 * Backs up an old directory by moving its contents to a sub-directory instead
	 * @param directory Directory that will be backed up
	 * @param subDirectoryName Name of the directory in which the old files from this directory are placed
	 */
	def backup(directory: Path, subDirectoryName: String = "last-build") = {
		// Moves the previously written files to a backup directory (which is cleared first)
		val backupDirectory = directory/subDirectoryName
		if (backupDirectory.exists)
			backupDirectory.deleteContents().failure.foreach { e =>
				println(s"WARNING: failed to clear $backupDirectory before backup. Error message: ${e.getMessage}")
			}
		else
			backupDirectory.createDirectories().failure.foreach { _.printStackTrace() }
		directory.tryIterateChildren {
				_.filterNot { _ == backupDirectory }.map { _.moveTo(backupDirectory) }.toVector.toTry }
			.failure
			.foreach { e =>
				println(s"WARNING: Failed to back up some previously built files. Error message: ${e.getMessage}")
			}
	}
	
	private def filterAndWriteModule(module: ModuleData, paths: ProjectPaths, targetType: => Int,
	                                 filter: => Option[Filter], arguments: CommandArguments) =
		filterAndWriteModules(module.moduleName, module.version, paths.output, module.namingRules,
			Single(module -> paths), targetType, filter, arguments, module.prefixColumnNames)
	
	private def filterAndWriteModules(projectName: Name, commonVersion: Option[Version], commonOutputPath: Path,
	                                  commonNaming: NamingRules, modules: Iterable[(ModuleData, ProjectPaths)],
	                                  targetType: => Int, filter: => Option[Filter], arguments: CommandArguments,
	                                  prefixColumnNames: Boolean): Boolean =
	{
		val startTime = LocalTime.now()
		
		println()
		// Applies filters
		val filteredData = {
			def modifyModule(data: Iterable[(ModuleData, ProjectPaths)])(f: Mutate[ModuleData]) =
				data.map { case (module, paths) => f(module) -> paths }
			
			// May remove combo classes
			val noCombos = arguments("no-combos").getBoolean
			val base = if (noCombos) modifyModule(modules) { _.withoutCombos } else modules
			base.foreach { case (module, _) =>
				println(s"${module.moduleName}: ${ module.classes.size } classes and ${
					module.combinations.size } combinations")
			}
			targetType match {
				case t if t == _class =>
					filter match {
						case Some(filter) =>
							modifyModule(base) { _.filterByClassName(filter, includeCombos = !noCombos) }
						case None => modifyModule(base) { _.onlyClasses }
					}
				case t if t == _package =>
					filter match {
						case Some(filter) =>
							modifyModule(base) { _.filterByPackage(filter, includeCombos = !noCombos) }
						case None => base
					}
				case t if t == _enums =>
					filter match {
						case Some(filter) => modifyModule(base) { _.filterByEnumName(filter) }
						case None => modifyModule(base) { _.onlyEnumerations }
					}
				case _ =>
					filter match {
						case Some(filter) => modifyModule(base) { _.filter(filter) }
						case None => base
					}
			}
		}
		
		println()
		println(s"Read ${filteredData.map { _._1.classes.size }.sum } classes, ${
			filteredData.map { _._1.enumerations.size }.sum } enumerations and ${
			filteredData.map { _._1.combinations.size }.sum} combinations within ${filteredData.size} projects and ${
			filteredData.map { _._1.classes.map { _.packageName }.toSet.size }.sum } packages")
		println()
		
		// Writes a single common SQL, table documentation, etc. for each database
		commonOutputPath.createDirectories() match {
			case Success(commonOutputPath) =>
				backup(commonOutputPath)
				modules.map { _._1 }.groupBy { _.databaseName }
					.map { case (databaseName, modules) =>
						println(s"Writing common files for ${ modules.size } modules to $commonOutputPath")
						writeCommonFiles(projectName, commonVersion, modules, databaseName, commonOutputPath,
							prefixColumnNames)(commonNaming)
					}
					.findMap { _.failure }
					.foreach { log(_, "Failed to write some of the commonly shared files") }
			case Failure(error) => log(error, s"Failed to create the output directory $commonOutputPath")
		}
		
		// Handles one module at a time
		val targetingByDefault = !arguments("no-targeting").getBoolean
		val writeResults = filteredData.filter { _._1.nonEmpty }.map { case (module, paths) =>
			println(s"Writing class and enumeration data to ${paths.output.toAbsolutePath}...")
			paths.output.asExistingDirectory.flatMap { directory =>
				// Moves the previously written files to a backup directory (which is cleared first)
				if (directory != commonOutputPath)
					backup(directory)
				
				println(s"Writing ${module.classes.size} classes, ${
					module.enumerations.size} enumerations and ${
					module.combinations.size } combinations for project ${module.moduleName}${module.version match {
					case Some(version) => s" $version"
					case None => ""
				}}")
				implicit val naming: NamingRules = module.namingRules
				val mergeFileName = s"${
					(module.moduleName.inContext(FileName) ++ Vector("merge", "conflicts",
						Today.toString, startTime.getHour.toString, startTime.getMinute.toString)).fileName
				}.txt"
				implicit val setup: VaultProjectSetup = new VaultProjectSetup(module.moduleName, module.modelPackage,
					module.databasePackage, subDirectory(directory, "src"), subDirectory(directory, "backup"),
					paths.sources, directory/mergeFileName, module.version, module.defaultMutability,
					module.modelCanReferToDB, module.prefixColumnNames)
				
				writeModuleFiles(module, paths.output, targetingByDefault)
			}
		}
		val writeFailures = writeResults.flatMap { _.failure }
		if (writeFailures.nonEmpty) {
			writeFailures.foreach(log.apply)
			println(s"Failed to write ${ writeFailures.size } of the ${ writeResults.size } modules")
		}
		writeResults.hasSize > writeFailures
	}
	
	private def writeCommonFiles(projectName: Name, version: Option[Version], modules: Iterable[ModuleData],
	                             databaseName: Option[Name], outputDir: Path, prefixColumnNames: Boolean)
	                            (implicit naming: NamingRules) =
	{
		// WET WET (refactor)
		def path(fileType: String, parts: String*) = {
			val fileNameBase = (projectName.inContext(FileName) ++ parts).fileName
			val fullFileName = version match {
				case Some(version) => s"$fileNameBase${naming(FileName).separator}$version.$fileType"
				case None => s"$fileNameBase.$fileType"
			}
			outputDir/fullFileName
		}
		
		val classes = modules.flatMap { _.classes }.toSeq.sorted
		// Writes the table structure
		SqlWriter(projectName, databaseName, version, classes.filterNot { _.isGeneric }, path("sql", "db", "structure"),
			prefixColumnNames)
			.flatMap { _ =>
				// Writes the inserts
				InsertsWriter(projectName, databaseName, version, modules.flatMap { _.instances },
					path("sql", "initial", "inserts"))
					.flatMap { _ =>
						// Writes the length rules
						ColumnLengthRulesWriter(databaseName, classes, path("json", "length", "rules")).flatMap { _ =>
							// Writes table descriptions json
							TableDescriptionsWriter(classes, path("ndjson", "tables"))
						}
					}
		}
	}
	
	private def writeModuleFiles(data: ModuleData, outputRoot: Path, targetingByDefault: Boolean)
	                            (implicit setup: VaultProjectSetup, naming: NamingRules): Try[Unit] =
	{
		def path(fileType: String, parts: String*) = {
			val fileNameBase = (setup.dbModuleName.inContext(FileName) ++ parts).fileName
			val fullFileName = data.version match {
				case Some(version) => s"$fileNameBase${naming(FileName).separator}$version.$fileType"
				case None => s"$fileNameBase.$fileType"
			}
			outputRoot/fullFileName
		}
		
		// Writes the enumerations
		data.enumerations.tryMap { EnumerationWriter(_) }
			// Writes project documentation
			.flatMap { _ => DocumentationWriter(data, path("md")) }
			// Writes the tables document, which is referred to later, also
			.flatMap { _ => TablesWriter(data.classes.filterNot { _.isGeneric }) }
			.flatMap { tablesRef =>
				DescriptionLinkInterfaceWriter(data.classes, tablesRef, targetingByDefault).flatMap { descriptionLinkObjects =>
					// Next writes all required documents for each class in order
					val combosByClass = data.combinations.groupBy { _.parentClass }
					writeClassesInOrder(data.classes, combosByClass, Map(), tablesRef, descriptionLinkObjects,
						targetingByDefault)
						.flatMap { classRefs =>
							// Finally writes the combined models
							combosByClass.tryForeach { case (parent, combos) =>
								// Provides alternative mapping in case of certain inheriting combo-classes,
								// where classes are transformed
								lazy val simplifiedRefs = classRefs.mapKeys { _.name.singularIn(CamelCase.capitalized) }
								val safeClassRefMap = MapAccess { c: Class =>
									classRefs.getOrElse(c, simplifiedRefs(c.name.singularIn(CamelCase.capitalized)))
								}
								
								// May write a generic combination trait first
								val commonComboTrait = {
									if (parent.writeCommonComboTrait)
										CombinedModelWriter.writeGeneralCombinationTrait(parent, safeClassRefMap(parent))
											.map { Some(_) }
									else
										Success(None)
								}
								commonComboTrait.flatMap { commonComboTrait =>
									combos.tryForeach {
										writeCombo(_, safeClassRefMap, commonComboTrait, targetingByDefault)
									}
								}
							}
						}
				}
			}
	}
	
	private def writeClassesInOrder(classesToWrite: Seq[Class], combosByClass: Map[Class, Seq[CombinationData]],
	                                classReferenceMap: Map[Class, ClassReferences], tablesRef: Reference,
	                                descriptionLinkObjects: Option[(Reference, Reference, Reference)],
	                                targetingByDefault: Boolean)
	                               (implicit setup: VaultProjectSetup, naming: NamingRules): Try[Map[Class, ClassReferences]] =
	{
		// Checks which classes may be written immediately
		val (pendingClasses, readyClasses) = classesToWrite
			.divideBy { _.parents.forall(classReferenceMap.contains) }.toTuple
		
		// If the class has been marked as an external reference, won't write any files for it
		def write(classToWrite: Class) = classToWrite.referenceFrom match {
			// Case: Externally referenced class => Only generates references
			case Some(externalPackage) =>
				val refs = generateReferencesForClass(classToWrite, externalPackage, targetingByDefault)
				Success(classToWrite -> refs)
				
			// Case: Normal class => Proceeds to write the class files
			case None =>
				writeClass(classToWrite, combosByClass, tablesRef, descriptionLinkObjects,
					classToWrite.parents.flatMap(classReferenceMap.get), targetingByDefault)
		}
		
		// Case: All remaining classes may be written => Writes and returns
		if (pendingClasses.isEmpty)
			readyClasses.tryMap(write).map { classReferenceMap ++ _ }
		// Case: No class may be written => Logs a warning and writes without proper references
		else if (readyClasses.isEmpty) {
			println(s"Warning: ${ pendingClasses.size } classes can't resolve their inheritances:")
			pendingClasses.foreach { c => println(s"\t- ${ c.name.className } extends ${
				c.parents.map { _.name.className }.mkString(" with ") }") }
			
			pendingClasses.tryMap(write).map { classReferenceMap ++ _ }
		}
		// Case: Some may be written while some can't
		//       => Writes the immediately available classes and uses recursion to write the rest
		else
			readyClasses.tryMap(write).flatMap { newRefs =>
				writeClassesInOrder(pendingClasses, combosByClass, classReferenceMap ++ newRefs, tablesRef,
					descriptionLinkObjects, targetingByDefault)
			}
	}
	
	private def writeClass(classToWrite: Class, combosByClass: Map[Class, Seq[CombinationData]], tablesRef: Reference,
	                       descriptionLinkObjects: Option[(Reference, Reference, Reference)],
	                       parentClassReferences: Seq[ClassReferences], targetingByDefault: Boolean)
	         (implicit setup: VaultProjectSetup, naming: NamingRules): Try[(Class, ClassReferences)] =
	{
		if (!parentClassReferences.hasSize.of(classToWrite.parents))
			println(s"Warning: ${ classToWrite.parents.size - parentClassReferences.size } of the ${
				classToWrite.parents.size } parent references are missing for ${ classToWrite.name.className }")
		
		ModelWriter(classToWrite, parentClassReferences, targetingByDefault).flatMap { modelRefs =>
			// dbPropsRefs is stored as: Try[Option[(Pair(DbPropsRef, DbPropsWrapperRef), wrapperPropName)]]
			val dbPropsRefs = {
				if (classToWrite.isGeneric)
					DbPropsWriter(classToWrite, parentClassReferences).map { Some(_) }
				else
					Success(None)
			}
			dbPropsRefs.flatMap { dbPropsRefs =>
				DbModelWriter(classToWrite, parentClassReferences, modelRefs, tablesRef, dbPropsRefs)
					// dbModelRefs is either Left: Multiple references (generic use-case), or Right: Reference to DbModel
					.flatMap { dbModelRefs =>
						val dbPropsOrModelRef = dbPropsRefs match {
							case None => dbModelRefs.rightOrMap { _.model }
							case Some((refs, _)) => refs.first
						}
						DbFactoryWriter(classToWrite, parentClassReferences, modelRefs, dbPropsOrModelRef,
							targetingByDefault)
							.flatMap { case (dbFactoryRef, dbFactoryLikeRef) =>
								// Adds description-specific references if applicable
								(descriptionLinkObjects match {
									// Case: At least one class uses descriptions
									case Some((linkModels, _, linkedDescriptionFactories)) =>
										classToWrite.descriptionLinkClass match {
											case Some(descriptionLinkClass) =>
												DescribedModelWriter(classToWrite, modelRefs.stored)
													.flatMap { describedRef =>
														DbDescriptionAccessWriter(descriptionLinkClass,
															classToWrite.name, linkModels, linkedDescriptionFactories)
															.map { case (singleAccessRef, manyAccessRef) =>
																Some(describedRef, singleAccessRef, manyAccessRef)
															}
													}
											case None => Success(None)
										}
									// Case: No classes use descriptions => automatically succeeds
									case None => Success(None)
									
								}).flatMap { descriptionReferences =>
									// Finally writes the access points
									// May generate new Targeting -based files instead of the older Access-based files
									val useTargeting = targetingByDefault && descriptionReferences.isEmpty
									val accessRefs = {
										if (useTargeting)
											TargetingWriter(classToWrite, parentClassReferences,
												combosByClass.getOrElse(classToWrite, Empty), tablesRef,
												modelRefs.stored, dbPropsOrModelRef, dbFactoryRef)
												.map { refs => (None, None, Some(refs)) }
										else
											AccessWriter(classToWrite, parentClassReferences, modelRefs.stored, dbFactoryRef,
												dbPropsOrModelRef, descriptionReferences)
												.map { case (single, many) => (single, many, None) }
									}
									accessRefs.map { case (genericUniqueAccessRef, genericManyAccessRef, targetingRefs) =>
										val genericReferences = modelRefs.generic.flatMap { modelRefs =>
											dbPropsRefs.flatMap { case (dbPropsRefs, _) =>
												dbModelRefs.leftOption.flatMap { dbModelRefs =>
													dbFactoryLikeRef.map { factoryLikeRef =>
														GenericClassReferences(modelRefs, dbPropsRefs.first,
															dbPropsRefs.second, dbModelRefs, factoryLikeRef)
													}
												}
											}
										}
										val classRefs = ClassReferences(classToWrite, modelRefs, dbFactoryRef,
											dbPropsOrModelRef, genericUniqueAccessRef, genericManyAccessRef,
											targetingRefs, genericReferences)
										
										classToWrite -> classRefs
									}
								}
							}
					}
			}
		}
	}
	
	private def writeCombo(combination: CombinationData, classRefsMap: MapAccess[Class, ClassReferences],
	                       commonComboTraitRef: Option[Reference], targetingByDefault: Boolean)
	                      (implicit setup: VaultProjectSetup, naming: NamingRules) =
	{
		val parentRefs = classRefsMap(combination.parentClass)
		val childRefs = classRefsMap(combination.childClass)
		
		// Writes the combo class / trait
		CombinedModelWriter(combination, parentRefs.model, childRefs.stored, commonComboTraitRef)
			.flatMap { combinedRefs =>
				// Writes the DB factory class
				CombinedFactoryWriter(combination, combinedRefs, parentRefs.dbFactory, childRefs.dbFactory,
					targetingByDefault)
					.flatMap { comboFactoryRef =>
						// Combo access traits are not written in targeting mode
						if (targetingByDefault)
							Success(())
						else
							parentRefs.genericUniqueAccessTrait
								.toTry { new IllegalStateException(
									"No generic unique access trait exists for a combined class") }
								.flatMap { genericUniqueAccessTraitRef =>
									parentRefs.genericManyAccessTrait
										.toTry { new IllegalStateException(
											"No generic access trait exists for a combined class") }
										.flatMap { genericManyAccessTraitRef =>
											AccessWriter.writeComboAccessPoints(combination, genericUniqueAccessTraitRef,
												genericManyAccessTraitRef, combinedRefs.combined, comboFactoryRef,
												parentRefs.dbModel, childRefs.dbModel)
										}
								}
					}
			}
	}
	
	// Generates references as if class files had been written
	private def generateReferencesForClass(classToWrite: Class, rootPackage: Package, targeting: Boolean)
	                                      (implicit naming: NamingRules) =
	{
		val dbPackage = rootPackage/"database"
		val accessPackage = dbPackage/"access"
		
		val modelRefs = ModelWriter.generateReferences(rootPackage/"model", classToWrite)
		val dbModelRefs = DbModelWriter.generateReferences(dbPackage/"storable", classToWrite)
		val (dbFactory, dbFactoryLike) = DbFactoryWriter
			.generateReferences(dbPackage/(if (targeting) "reader" else "factory"), classToWrite, targeting)
		val accessRefs = AccessWriter.generateReferences(accessPackage, classToWrite)
		
		val (dbModel, dbPropsRefs) = dbModelRefs match {
			case Left(_) =>
				val dbPropsRefs = DbPropsWriter.generateReferences(dbPackage, classToWrite)
				dbPropsRefs.first -> Some(dbPropsRefs)
			case Right(model) => model -> None
		}
		val generic = modelRefs.generic.flatMap { model =>
			dbPropsRefs.flatMap { dbProps =>
				dbModelRefs.leftOption.flatMap { dbModel =>
					dbFactoryLike.map { factory =>
						GenericClassReferences(model, dbProps.first, dbProps.second, dbModel, factory)
					}
				}
			}
		}
		
		ClassReferences(classToWrite, modelRefs, dbFactory, dbModel,
			accessRefs.map { _.first }, accessRefs.map { _.second },
			Some(TargetingWriter.generateReferencesFor(accessPackage, classToWrite)), generic)
	}
}
