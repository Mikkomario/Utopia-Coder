package utopia.coder.vault.controller.writer.database

import utopia.coder.model.data.{Name, NamingRules}
import utopia.coder.model.enumeration.NameContext.{DatabaseName, TableName}
import utopia.coder.model.enumeration.NamingConvention.{CamelCase, Text}
import utopia.flow.collection.CollectionExtensions._
import utopia.flow.parse.file.FileExtensions._
import utopia.flow.parse.string.Regex
import utopia.flow.time.Today
import utopia.flow.util.StringExtensions._
import utopia.coder.vault.model.data.{Class, DbProperty, VaultProjectSetup}
import utopia.coder.vault.model.datatype.PropertyType
import utopia.coder.vault.model.datatype.StandardPropertyType.{ClassReference, EnumValue}
import utopia.flow.util.Version

import java.io.PrintWriter
import java.nio.file.Path
import scala.annotation.tailrec
import scala.io.Codec
import scala.util.Success

/**
  * Used for converting class data into SQL
  * @author Mikko Hilpinen
  * @since 1.9.2021, v0.1
  */
object SqlWriter
{
	// OTHER    ------------------------
	
	/**
	  * Writes the SQL document for the specified classes
	  * @param projectName Name of the project for which this SQL is written
	 * @param dbName Name of the database to use (optional)
	  * @param version Version of this document, if applicable
	 * @param classes    Classes to write
	  * @param targetPath Path to which write the sql document
	  * @param codec      Implicit codec used when writing
	  * @param prefixColumnNames Whether column names should be prefixed with a unique table-specific prefix
	 * @return Target path. Failure if writing failed.
	  */
	def apply(projectName: Name, dbName: Option[Name], version: Option[Version], classes: Seq[Class], targetPath: Path,
	          prefixColumnNames: Boolean)
	         (implicit codec: Codec, naming: NamingRules) =
	{
		// Doesn't write anything if no classes are included
		if (classes.nonEmpty) {
			// Writes the table declarations in an order that attempts to make sure foreign keys are respected
			// (referenced tables are written before referencing tables)
			val allClasses = classes ++ classes.flatMap { _.descriptionLinkClass }
			val classesByTableName = allClasses.map { c => c.tableName -> c }.toMap
			val references = classesByTableName.map { case (tableName, c) =>
				val refs = c.properties.flatMap { _.dataType match {
					case ClassReference(referencedTableName, _, _) =>
						// References to the class' own table are ignored
						Some(referencedTableName.table).filterNot { _ == tableName }
					case _ => None
				} }
				c.tableName -> refs.toSet
			}
			// Forms the table initials, also
			val initials = initialsFrom(references.flatMap { case (tableName, refs) => refs + tableName }.toSet)
			targetPath.writeUsing { writer =>
				// Writes the header
				writer.println("-- ")
				writer.println(s"-- Database structure for ${projectName.doc} models")
				version.foreach { v => writer.println(s"-- Version: $v") }
				writer.println(s"-- Last generated: ${Today.toString}")
				writer.println("--")
				
				// Writes the database introduction, if needed
				dbName.foreach { dbName =>
					val formattedDbName = dbName(DatabaseName)
					writer.println()
					writer.println(s"CREATE DATABASE IF NOT EXISTS `$formattedDbName` ")
					writer.println("\tDEFAULT CHARACTER SET utf8 DEFAULT COLLATE utf8_general_ci;")
					writer.println(s"USE `$formattedDbName`;")
				}
				
				// Groups the classes by package and writes them
				writeClasses(writer, initials, classesByTableName.groupBy { _._2.packageName }, references,
					prefixColumnNames)
			}
		}
		else
			Success(targetPath)
	}
	
	// classesByPackageAndTableName: first key is package name (header) and second key is table name
	// references: Keys and values are both table names
	@tailrec
	private def writeClasses(writer: PrintWriter, initialsMap: Map[String, String],
	                          classesByPackageAndTableName: Map[String, Map[String, Class]],
	                          references: Map[String, Set[String]], prefixProperties: Boolean)
	                        (implicit naming: NamingRules): Unit =
	{
		// Finds the classes which don't make any references to other remaining classes
		val remainingTableNames = classesByPackageAndTableName.flatMap { _._2.keys }.toSet
		val notReferencingTableNames = remainingTableNames
			.filterNot { tableName =>
				references(tableName)
					.exists { referencedTableName => remainingTableNames.contains(referencedTableName) }
			}
		// Case: All classes are referenced at least once (indicates a cyclic loop) => Writes them in alphabetical order
		if (notReferencingTableNames.isEmpty) {
			writer.println("\n-- WARNING: Following classes contain a cyclic loop\n")
			classesByPackageAndTableName.valuesIterator.flatMap { _.valuesIterator }.toVector.sortBy { _.name.singular }
				.foreach { writeClass(writer, _, initialsMap, prefixProperties) }
		}
		// Case: There are some classes which don't reference remaining classes => writes those
		else {
			// Writes a single package, including as many classes as possible
			// Prefers packages which can be finished off, also preferring larger class sets
			// Package name -> (currently writeable classes, classes which are dependent from other remaining packages)
			val packagesWithInfo = classesByPackageAndTableName.map { case (packageName, classesByTableName) =>
				val packageClassTables = classesByTableName.keySet
				// Writeable = Class only makes references inside this package
				// Dependent = Class makes references to other remaining packages
				val (writeableClasses, dependentClasses) = classesByTableName
					.divideBy { case (tableName, _) =>
						references.get(tableName)
							.exists { refs => ((refs & remainingTableNames) -- packageClassTables).nonEmpty }
					}
					.toTuple
				packageName -> (writeableClasses, dependentClasses)
			}
			// Finds the next package to target and starts writing classes within that package
			val (packageName, (writeableClasses, remainingPackageClasses)) = packagesWithInfo
				.bestMatch { _._2._2.isEmpty }.maxBy { _._2._1.size }
			val packageHeader = Name.interpret(packageName, CamelCase.lower).to(Text.allCapitalized).singular
			writer.println(s"\n--\t$packageHeader\t${"-" * 10}\n")
			val allRemainingPackageClasses = writePossibleClasses(writer, initialsMap, writeableClasses, references,
				prefixProperties) ++ remainingPackageClasses
			// Prepares the next recursive iteration
			val remainingClasses = {
				if (allRemainingPackageClasses.isEmpty)
					classesByPackageAndTableName - packageName
				else
					classesByPackageAndTableName + (packageName -> allRemainingPackageClasses)
			}
			if (remainingClasses.nonEmpty)
				writeClasses(writer, initialsMap, remainingClasses, references, prefixProperties)
		}
	}
	
	@tailrec
	private def writePossibleClasses(writer: PrintWriter, initialsMap: Map[String, String],
	                                 classesByTableName: Map[String, Class], references: Map[String, Set[String]],
	                                 prefixProperties: Boolean)
	                                (implicit naming: NamingRules): Map[String, Class] =
	{
		// Finds the classes which don't make any references to other remaining classes
		val remainingTableNames = classesByTableName.keySet
		val notReferencingTableNames = remainingTableNames
			.filterNot { tableName => references(tableName)
				.exists { referencedTableName => remainingTableNames.contains(referencedTableName) } }
		// Case: All classes make at least once reference => sends them back to the original method caller
		if (notReferencingTableNames.isEmpty)
			classesByTableName
		// Case: There are some classes which don't reference remaining classes => writes those
		else {
			// Writes the classes in alphabetical order
			notReferencingTableNames.toVector.sorted
				.foreach { table => writeClass(writer, classesByTableName(table), initialsMap, prefixProperties) }
			// Continues recursively. Returns the final group of remaining classes.
			val remainingClassesByTableName = classesByTableName -- notReferencingTableNames
			if (remainingClassesByTableName.nonEmpty)
				writePossibleClasses(writer, initialsMap, remainingClassesByTableName, references, prefixProperties)
			else
				remainingClassesByTableName
		}
	}
	
	private def writeClass(writer: PrintWriter, classToWrite: Class, initialsMap: Map[String, String],
	                       prefixProperties: Boolean)
	                      (implicit naming: NamingRules): Unit =
	{
		implicit val wr: PrintWriter = writer
		
		val tableName = classToWrite.tableName
		lazy val classInitials = initialsMap(tableName)
		def prefixColumn(column: DbProperty, parentType: PropertyType): String =
			prefixColumnName(column.columnName, parentType match {
				case ClassReference(table, _, _) => Some(table.table)
				case _ => None
			})
		def prefixColumnName(colName: String, referredTableName: => Option[String] = None): String = {
			if (prefixProperties) {
				referredTableName.flatMap(initialsMap.get) match {
					case Some(refInitials) => s"${classInitials}_${refInitials}_$colName"
					case None => s"${ classInitials }_$colName"
				}
			}
			else
				colName
		}
		val idName = prefixColumnName(classToWrite.idName.column)
		// [(Property -> [(DbProperty -> Full Column Name)])]
		val namedProps = classToWrite.properties
			.map { prop => prop -> prop.dbProperties.map { dbProp => dbProp -> prefixColumn(dbProp, prop.dataType) } }
		val columns = namedProps.flatMap { _._2 }
		
		writeDocumentation(classToWrite.description)
		// Writes property documentation
		val maxColumnNameLength: Int = columns.map { _._2.length }.maxOption.getOrElse(0)
		namedProps.foreach { case (prop, columns) =>
			if (prop.description.nonEmpty) {
				// If spans multiple columns, introduces all of them with the same description
				val name = {
					if (columns.size == 1)
						columns.head._2
					else
						s"${prop.name} (${columns.map { _._2 }.mkString(", ")})"
				}
				val propIntroduction = s"$name:".padTo(maxColumnNameLength + 1, ' ')
				val enumValuesPart = prop.dataType match {
					case EnumValue(enumeration) =>
						s"\nReferences enumeration ${ enumeration.name.enumName }\nPossible values are: ${
							enumeration.values.map { value => s"${ value.id } = ${ value.name.doc }" }.mkString(", ") }"
					case _ => ""
				}
				writeDocumentation(s"$propIntroduction ${prop.description}$enumValuesPart")
			}
		}
		// Writes the table
		writer.println(s"CREATE TABLE `$tableName`(")
		val idBase = s"\t`$idName` ${ classToWrite.idType.sqlType.toSql } PRIMARY KEY AUTO_INCREMENT"
		if (columns.isEmpty)
			writer.println(idBase)
		else {
			writer.println(s"$idBase, ")
			
			val propertyDeclarations = columns.map { case (prop, name) =>
				val defaultPart = prop.default.mapIfNotEmpty { " DEFAULT " + _ }
				s"`$name` ${ prop.sqlType.baseTypeSql }${ prop.sqlType.notNullPart }$defaultPart"
			}
			val comboIndexColumnNames = classToWrite.comboIndexColumnNames.map { _.map { prefixColumnName(_) } }
			val firstComboIndexColumns = comboIndexColumnNames.filter { _.size > 1 }.map { _.head }.toSet
			val individualIndexDeclarations = columns
				.filter { case (prop, name) => prop.isIndexed && !firstComboIndexColumns.contains(name) }
				.map { case (_, name) => s"INDEX ${ classInitials }_${ name }_idx (`$name`)" }
			val comboIndexDeclarations = comboIndexColumnNames.filter { _.size > 1 }
				.zipWithIndex.map { case (colNames, index) =>
				s"INDEX ${ classInitials }_combo_${ index + 1 }_idx (${ colNames.mkString(", ") })"
			}
			val foreignKeyDeclarations = namedProps.flatMap { case (prop, columns) =>
				prop.dataType match {
					case ClassReference(rawReferencedTableName, rawColumnName, referenceType) =>
						val refTableName = rawReferencedTableName.table
						val refInitials = initialsMap(refTableName)
						val refColumnName = {
							val base = rawColumnName.column
							if (prefixProperties)
								s"${ refInitials }_$base"
							else
								base
						}
						val columnName = columns.headOption match {
							case Some((_, name)) => name
							case None => prop.name.column
						}
						val constraintNameBase = {
							val nameWithoutId = columnName.replace("_id", "")
							val base = {
								if (prefixProperties)
									nameWithoutId
								else
									s"${ classInitials }_${ refInitials }_$nameWithoutId"
							}
							s"${ base }_ref"
						}
						Some(s"CONSTRAINT ${ constraintNameBase }_fk FOREIGN KEY ${
							constraintNameBase }_idx ($columnName) REFERENCES `$refTableName`(`$refColumnName`) ON DELETE ${
							if (referenceType.sqlConversions.forall { _.target.isNullable }) "SET NULL" else "CASCADE"
						}")
					case _ => None
				}
			}
			
			val allDeclarations = propertyDeclarations ++ individualIndexDeclarations ++ comboIndexDeclarations ++
				foreignKeyDeclarations
			allDeclarations.dropRight(1).foreach { line => writer.println(s"\t$line, ") }
			writer.println(s"\t${ allDeclarations.last }")
		}
		
		writer.println(")Engine=InnoDB DEFAULT CHARACTER SET utf8 DEFAULT COLLATE utf8_general_ci;")
		writer.println()
	}
	
	def initialsFrom(tableNames: Iterable[String])(implicit naming: NamingRules) = {
		val convention = naming(TableName)
		// Splits each table name into parts
		val allOptions = tableNames.map { name => name -> convention.split(name) }
		// Groups by the default initials (1 char from each part)
		allOptions.groupBy { _._2.map { _.head }.mkString }.flatMap { case (defaultInitials, options) =>
			// Case: Not unique => Makes unique
			if (options.hasSize > 1)
				makeUnique(options, 1)
			// Case: Unique => Preserves
			else
				options.map { _._1 -> defaultInitials }
		}
	}
	
	// Assumes that 1 char from each entry has been tested already
	// Also assumes that the specified set contains items of identical lengths
	@tailrec
	private def makeUnique(options: Iterable[(String, Seq[String])], testedChars: Int): Iterable[(String, String)] = {
		// Checks whether it is possible to find some index that (with this character count) makes the results distinct
		options.head._2.indices
			.find { i =>
				val partOptions = options.map { _._2(i) }
				val uniqueStarts = partOptions.map { _.take(testedChars + 1) }.toSet
				uniqueStarts.size == partOptions.size
			} match
		{
			// Case: Index found => Expands that index
			case Some(targetIndex) =>
				options.map { case (tableName, parts) =>
					tableName -> parts.zipWithIndex
						.map { case (part, i) => part.take(if (i == targetIndex) testedChars + 1 else 1) }
						.mkString
				}
			// Case: No index found => Increases the amount of characters taken
			case None => makeUnique(options, testedChars + 1)
		}
	}
	
	private def writeDocumentation(doc: String)(implicit writer: PrintWriter) = {
		doc.notEmpty.foreach { doc =>
			val lines = doc.linesIterator.map(Regex.newLine.filterNot).toVector
			writer.println(s"-- ${lines.head}")
			lines.tail.foreach { line => writer.println(s"-- \t\t$line") }
		}
	}
}
