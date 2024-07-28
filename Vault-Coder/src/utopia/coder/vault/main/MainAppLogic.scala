package utopia.coder.vault.main

import utopia.coder.controller.app.CoderAppLogic
import utopia.coder.controller.parsing.file.InputFiles
import utopia.coder.model.data.{Filter, LazyProjectPaths, Name, NamingRules, ProjectPaths}
import utopia.coder.model.enumeration.NameContext.FileName
import utopia.coder.model.scala.datatype.Reference
import utopia.flow.collection.CollectionExtensions._
import utopia.flow.operator.equality.EqualsExtensions._
import utopia.flow.parse.file.FileExtensions._
import utopia.flow.time.Today
import utopia.flow.util.console.{ArgumentSchema, CommandArguments}
import utopia.flow.util.logging.Logger
import utopia.flow.view.immutable.caching.Lazy
import utopia.coder.vault.controller.reader
import utopia.coder.vault.controller.reader.ClassReader
import utopia.coder.vault.controller.writer.database._
import utopia.coder.vault.controller.writer.documentation.{DocumentationWriter, TableDescriptionsWriter}
import utopia.coder.vault.controller.writer.model.{CombinedModelWriter, DescribedModelWriter, EnumerationWriter, ModelWriter}
import utopia.coder.vault.model.data.{Class, ClassReferences, CombinationData, GenericClassReferences, ModuleData, VaultProjectSetup}
import utopia.coder.vault.model.enumeration.Mutability
import utopia.coder.vault.util.Common
import utopia.flow.collection.immutable.{Empty, Pair, Single}
import utopia.flow.util.{Mutate, Version}

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
	
	override protected def projectsStoreLocation: Path = "projects.json"
	override protected def supportsAlternativeMergeRoots: Boolean = true
	
	// Adds the "no combos" -argument and the "module" argument
	override def argumentSchema = {
		val original = super.argumentSchema
		val firstFlagIndex = original.findIndexWhere { _.isFlag }.getOrElse { original.size }
		
		val module = ArgumentSchema("module", help = "Name of the targeted project module (default = all)")
		val noCombos = ArgumentSchema.flag("no-combos", "NC", help = "Whether combo-class writing should be disabled")
		
		((original.take(firstFlagIndex) :+ module) ++ original.drop(firstFlagIndex)) :+ noCombos
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
										.logToOptionWithMessage(s"Failed to parse module data from $inputPath")
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
										.logToOptionWithMessage(s"Failed to parse module data from $inputPath")
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
				
				writeModuleFiles(module, paths.output)
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
		SqlWriter(projectName, databaseName, version, classes, path("sql", "db", "structure"),
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
	
	private def writeModuleFiles(data: ModuleData, outputRoot: Path)
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
			.flatMap { _ => TablesWriter(data.classes) }
			.flatMap { tablesRef =>
				DescriptionLinkInterfaceWriter(data.classes, tablesRef).flatMap { descriptionLinkObjects =>
					// Next writes all required documents for each class in order
					writeClassesInOrder(data.classes, Map(), tablesRef, descriptionLinkObjects).flatMap { classRefs =>
						// Finally writes the combined models
						data.combinations.tryForeach { writeCombo(_, classRefs) }
					}
				}
			}
	}
	
	private def writeClassesInOrder(classesToWrite: Seq[Class], classReferenceMap: Map[Class, ClassReferences],
	                                tablesRef: Reference,
	                                descriptionLinkObjects: Option[(Reference, Reference, Reference)])
	                               (implicit setup: VaultProjectSetup, naming: NamingRules): Try[Map[Class, ClassReferences]] =
	{
		// Checks which classes may be written immediately
		val (pendingClasses, readyClasses) = classesToWrite
			.divideBy { _.parents.forall(classReferenceMap.contains) }.toTuple
		
		def write(classToWrite: Class) =
			writeClass(classToWrite, tablesRef, descriptionLinkObjects,
				classToWrite.parents.flatMap(classReferenceMap.get))
		
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
				writeClassesInOrder(pendingClasses, classReferenceMap ++ newRefs, tablesRef, descriptionLinkObjects)
			}
	}
	
	private def writeClass(classToWrite: Class, tablesRef: Reference,
	                       descriptionLinkObjects: Option[(Reference, Reference, Reference)],
	                       parentClassReferences: Seq[ClassReferences])
	         (implicit setup: VaultProjectSetup, naming: NamingRules): Try[(Class, ClassReferences)] =
	{
		ModelWriter(classToWrite, parentClassReferences).flatMap { modelRefs =>
			val dbPropsRefs = {
				if (classToWrite.isGeneric)
					DbPropsWriter(classToWrite, parentClassReferences).map { Some(_) }
				else
					Success(None)
			}
			dbPropsRefs.flatMap { dbPropsRefs =>
				DbModelWriter(classToWrite, parentClassReferences, modelRefs, tablesRef, dbPropsRefs)
					.flatMap { dbModelRefs =>
						val dbPropsRef = dbPropsRefs match {
							case None =>
								dbModelRefs.rightOrMap { _.model }
							case Some((refs, _)) => refs.first
						}
						DbFactoryWriter(classToWrite, parentClassReferences, modelRefs, dbPropsRef)
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
									val modelFactoryRef = dbModelRefs.rightOrMap { _.factory }
									AccessWriter(classToWrite, parentClassReferences, modelRefs.stored, dbFactoryRef,
										modelFactoryRef, descriptionReferences)
										.map { case (genericUniqueAccessRef, genericManyAccessRef) =>
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
											val classRefs = ClassReferences(modelRefs, dbFactoryRef, dbPropsRef,
												genericUniqueAccessRef, genericManyAccessRef, genericReferences)
											classToWrite -> classRefs
										}
								}
							}
					}
			}
		}
	}
	
	private def writeCombo(combination: CombinationData, classRefsMap: Map[Class, ClassReferences])
	                      (implicit setup: VaultProjectSetup, naming: NamingRules) =
	{
		val parentRefs = classRefsMap(combination.parentClass)
		val childRefs = classRefsMap(combination.childClass)
		CombinedModelWriter(combination, parentRefs.stored, parentRefs.data, childRefs.stored, parentRefs.factoryWrapper)
			.flatMap { combinedRefs =>
				CombinedFactoryWriter(combination, combinedRefs, parentRefs.dbFactory, childRefs.dbFactory)
					.flatMap { comboFactoryRef =>
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
	
	private def backup(directory: Path) = {
		// Moves the previously written files to a backup directory (which is cleared first)
		val backupDirectory = directory/"last-build"
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
}
