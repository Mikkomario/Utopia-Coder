package utopia.coder.vault.controller.writer.database.sql

import utopia.coder.model.data.{Name, NamingRules}
import utopia.coder.model.enumeration.NameContext.{DatabaseName, TableName}
import utopia.coder.model.enumeration.NamingConvention.{CamelCase, Text}
import utopia.coder.vault.model.data.{Class, DbProperty}
import utopia.coder.vault.model.datatype.PropertyType
import utopia.coder.vault.model.datatype.StandardPropertyType.{ClassReference, EnumValue}
import utopia.flow.collection.CollectionExtensions._
import utopia.flow.collection.immutable.Empty
import utopia.flow.collection.mutable.iterator.{LazyInitIterator, PollableOnce}
import utopia.flow.parse.file.FileExtensions._
import utopia.flow.parse.string.Regex
import utopia.flow.time.Today
import utopia.flow.util.StringExtensions._
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
	// ATTRIBUTES   --------------------
	
	private val vocals = Set('a', 'e', 'i', 'o', 'u', 'y')
	
	
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
	          projectPrefix: String, prefixColumnNames: Boolean)
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
					projectPrefix.appendIfNotEmpty("_"), prefixColumnNames)
			}
		}
		else
			Success(targetPath)
	}
	
	/**
	 * Writes table information
	 * @param writer Writer to use
	 * @param initialsMap A map that contains a set of initials for each table
	 * @param classesByPackageAndTableName A two-levels deep map where keys are:
	 *                                          1. Package name (header)
	 *                                          1. Table name
	 *
	 *                                     And values are the matching classes.
	 * @param references A map that contains references between tables.
	 *                      - The keys are referencing table names
	 *                      - The values are referenced table names (many-to-one)
	 * @param projectPrefix Prefix added to all generated indices & foreign keys
	 * @param prefixProperties Whether to prefix column names
	 * @param naming Implicit naming rules
	 */
	private def writeClasses(writer: PrintWriter, initialsMap: Map[String, String],
	                         classesByPackageAndTableName: Map[String, Map[String, Class]],
	                         references: Map[String, Set[String]], projectPrefix: String, prefixProperties: Boolean)
	                        (implicit naming: NamingRules): Unit =
	{
		// Finds the classes which don't make any references to other remaining classes
		val remainingTableNames = classesByPackageAndTableName.valuesIterator.flatMap { _.keysIterator }.toSet
		val notReferencingTableNames = remainingTableNames.filterNot { tableName =>
			references(tableName).exists(remainingTableNames.contains)
		}
		
		// Case: All classes are referenced at least once (indicates a cyclic loop)
		//       => Writes one class in two parts
		if (notReferencingTableNames.isEmpty) {
			// Finds the most referenced remaining class
			remainingTableNames.iterator.flatMap(references.apply).countAll.toVector.reverseSortBy { _._2 }
				.iterator.map { _._1 }.filter(remainingTableNames.contains)
				.findMap { tableName =>
					classesByPackageAndTableName.iterator.findMap { case (packageName, tableClasses) =>
						tableClasses.get(tableName).map { tableClass => (packageName, tableName, tableClass) }
					}
				} match
			{
				case Some((partedPackage, partedTable, partedClass)) =>
					// Writes the class without the foreign keys
					writer.println(s"\n-- NB: $partedTable is split in order to resolve a cyclic loop\n")
					val preparedAlterTable = writeClass(writer, partedClass, initialsMap, projectPrefix,
						prefixProperties, excludeForeignKeys = true)
					
					val remainingClasses = (classesByPackageAndTableName(partedPackage) - partedTable).notEmpty match {
						case Some(remainingPackageClasses) =>
							classesByPackageAndTableName + (partedPackage -> remainingPackageClasses)
						case None => classesByPackageAndTableName - partedPackage
					}
					
					// Writes the remaining classes
					writeClasses(writer, initialsMap, remainingClasses, references, projectPrefix, prefixProperties)
					
					// Writes the delayed foreign keys
					preparedAlterTable.foreach { case (tableName, foreignKeyRows) =>
						writer.println(s"\n-- Adds missing foreign keys to $partedTable")
						writer.println(s"ALTER TABLE `$tableName` \n${
							foreignKeyRows.iterator.map { decl => s"\tADD $decl" }.mkString(", \n") }")
					}
				
				// Case: No referenced table was found (unexpected)
				//       => Writes the remaining classes in alphabetical order
				case None =>
					writer.println("\n-- WARNING: Following classes contain a cyclic loop\n")
					classesByPackageAndTableName.valuesIterator.flatMap { _.valuesIterator }
						.toVector.sortBy { _.name.singular }
						.foreach { writeClass(writer, _, initialsMap, projectPrefix, prefixProperties) }
			}
		}
		// Case: There are some classes which don't reference remaining classes => writes those
		else {
			// Writes a single package, including as many classes as possible
			// Prefers packages which can be finished off, also preferring larger class sets
			// Package name -> (currently writeable classes, classes which are dependent from other remaining packages)
			val packagesWithInfo: Map[String, (Map[String, Class], Map[String, Class])] =
				classesByPackageAndTableName.map { case (packageName, classesByTableName) =>
					val packageClassTables = classesByTableName.keySet
					// Writeable = Class only makes references inside this package
					// Dependent = Class makes references to other remaining packages
					val (writeableClasses, dependentClasses) = {
						if (classesByTableName.keysIterator.exists(notReferencingTableNames.contains))
							classesByTableName
								.divideBy { case (tableName, _) =>
									references.get(tableName).exists { refs =>
										((refs & remainingTableNames) -- packageClassTables).nonEmpty
									}
								}
								.toTuple
						// Case: None of these classes may be written at this time => Can't initiate this package
						else
							(Map[String, Class](), classesByTableName)
					}
					packageName -> (writeableClasses, dependentClasses)
				}
			// Finds the next package to target and starts writing classes within that package
			val (packageName, (writeableClasses, remainingPackageClasses)) = packagesWithInfo
				.bestMatch { _._2._2.isEmpty }.maxBy { _._2._1.size }
			
			val packageHeader = Name.interpret(packageName, CamelCase.lower).to(Text.allCapitalized).singular
			writer.println(s"\n--\t$packageHeader\t${"-" * 10}\n")
			
			val allRemainingPackageClasses = writePossibleClasses(writer, initialsMap, writeableClasses, references,
				projectPrefix, prefixProperties) ++ remainingPackageClasses
			
			// Prepares the next recursive iteration
			val remainingClasses = {
				if (allRemainingPackageClasses.isEmpty)
					classesByPackageAndTableName - packageName
				else
					classesByPackageAndTableName + (packageName -> allRemainingPackageClasses)
			}
			if (remainingClasses.nonEmpty)
				writeClasses(writer, initialsMap, remainingClasses, references, projectPrefix, prefixProperties)
		}
	}
	
	@tailrec
	private def writePossibleClasses(writer: PrintWriter, initialsMap: Map[String, String],
	                                 classesByTableName: Map[String, Class], references: Map[String, Set[String]],
	                                 projectPrefix: String, prefixProperties: Boolean)
	                                (implicit naming: NamingRules): Map[String, Class] =
	{
		// Finds the classes which don't make any references to other remaining classes
		val remainingTableNames = classesByTableName.keySet
		val notReferencingTableNames = remainingTableNames.filterNot { tableName =>
			references(tableName).exists { referencedTableName => remainingTableNames.contains(referencedTableName) }
		}
		// Case: All classes make at least once reference => sends them back to the original method caller
		if (notReferencingTableNames.isEmpty)
			classesByTableName
		// Case: There are some classes which don't reference remaining classes => writes those
		else {
			// Writes the classes in alphabetical order
			notReferencingTableNames.toVector.sorted.foreach { table =>
				writeClass(writer, classesByTableName(table), initialsMap, projectPrefix, prefixProperties)
			}
			// Continues recursively. Returns the final group of remaining classes.
			val remainingClassesByTableName = classesByTableName -- notReferencingTableNames
			if (remainingClassesByTableName.nonEmpty)
				writePossibleClasses(writer, initialsMap, remainingClassesByTableName, references, projectPrefix,
					prefixProperties)
			else
				remainingClassesByTableName
		}
	}
	
	// Returns possible declarations to delay, containing 2 values:
	//      1. Name of the altered table
	//      2. declarations to add
	private def writeClass(writer: PrintWriter, classToWrite: Class, initialsMap: Map[String, String],
	                       projectPrefix: String, prefixProperties: Boolean, excludeForeignKeys: Boolean = false)
	                      (implicit naming: NamingRules) =
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
		val delayed = {
			if (columns.isEmpty) {
				writer.println(idBase)
				None
			}
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
					.map { case (_, name) => s"INDEX $projectPrefix${ classInitials }_${ name }_idx (`$name`)" }
				val comboIndexDeclarations = comboIndexColumnNames.filter { _.size > 1 }
					.zipWithIndex.map { case (colNames, index) =>
						s"INDEX $projectPrefix${ classInitials }_combo_${ index + 1 }_idx (${ colNames.mkString(", ") })"
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
							Some(s"CONSTRAINT $projectPrefix${ constraintNameBase }_fk FOREIGN KEY $projectPrefix${
								constraintNameBase }_idx ($columnName) REFERENCES `$refTableName`(`$refColumnName`) ON DELETE ${
								if (referenceType.sqlConversions.forall { _.target.isNullable }) "SET NULL" else "CASCADE"
							}")
						case _ => None
					}
				}
				
				// May delay foreign key -writing
				val (declarationsToWrite, declarationsToDelay) = {
					val standard = Vector.concat(propertyDeclarations, individualIndexDeclarations,
						comboIndexDeclarations)
					foreignKeyDeclarations.notEmpty match {
						case Some(foreignKeys) =>
							if (excludeForeignKeys)
								standard -> Some(tableName -> foreignKeys)
							else
								(standard ++ foreignKeys, None)
						
						case None => standard -> None
					}
				}
				
				declarationsToWrite.dropRight(1).foreach { line => writer.println(s"\t$line, ") }
				writer.println(s"\t${ declarationsToWrite.last }")
				
				declarationsToDelay
			}
		}
		
		writer.println(")Engine=InnoDB DEFAULT CHARACTER SET utf8 DEFAULT COLLATE utf8_general_ci;")
		writer.println()
		
		delayed
	}
	
	def initialsFrom(tableNames: Iterable[String])(implicit naming: NamingRules) = {
		val convention = naming(TableName)
		// Splits each table name into parts
		val tableNamesWithParts = tableNames.map { name => name -> convention.split(name) }
		// Groups by the default initials (1 char from each part)
		val (uniquePrimaryResults, duplicates) = tableNamesWithParts.groupToSeqsBy { _._2.map { _.head }.mkString }
			.divideWith { case (initials, originals) =>
				originals.oneOrMany match {
					case Left((unique, _)) => Left(unique -> initials)
					case Right(duplicates) => Right(duplicates -> initials.length)
				}
			}
		
		// Case: There were no duplicates => Finishes
		if (duplicates.isEmpty)
			uniquePrimaryResults.toMap
		// Case: There were duplicates => Continues using more advanced initial-forming
		else {
			val usedInPrimary = uniquePrimaryResults.iterator.map { _._2 }.toSet
			(uniquePrimaryResults.iterator ++
				_initialsFrom(
					duplicates.map { case (duplicates, minLength) =>
						duplicates.map { case (original, parts) => (original, parts, minLength) } },
					usedInPrimary))
				.toMap
		}
	}
	private def _initialsFrom(overlappingGroups: Iterable[Seq[(String, Seq[String], Int)]], used: Set[String]): Seq[(String, String)] = {
		// Generates unique initials for each duplicate-group
		val (uniqueResults, duplicateEntries) = overlappingGroups.iterator.flatMap { makeUnique(_, used) }
			// Checks whether any duplicates were formed between groups
			.groupMapToSeqs { _._3 } { case (original, parts, _) => original -> parts }
			.divideWith { case (initials, originals) =>
				originals.oneOrMany match {
					case Left((original, _)) => Left(original -> initials)
					case Right(duplicates) => Right(duplicates -> initials.length)
				}
			}
		
		// Case: There are no more duplicates => Finishes
		if (duplicateEntries.isEmpty)
			uniqueResults
		// Case: There are some duplicates => Continues recursively
		else {
			val nowUsed = used ++ uniqueResults.view.map { _._2 }
			uniqueResults ++
				_initialsFrom(
					duplicateEntries.map { case (originals, lastLength) =>
						originals.map { case (original, parts) => (original, parts, lastLength) }
					},
					nowUsed)
		}
	}
	
	private def makeUnique(items: Seq[(String, Seq[String], Int)], used: Set[String]): Seq[(String, Seq[String], String)] =
	{
		// Increases the "other prefix" complexity between iterations
		val otherComplexityRange = {
			if (items.forall { _._2.hasSize(1) })
				1 to 1
			else {
				val longestWordLength = items.iterator.flatMap { _._2 }.map { _.length }.max
				1 to (longestWordLength + 1)
			}
		}
		otherComplexityRange.iterator
			.flatMap { otherPrefixLevel =>
				// Finds the first word that can be used to separate these options
				targetIndicesIteratorFor(items.view.map { _._2 }).flatMap { i =>
					val targetWords = items.map { _._2.getOrElse(i, "") }
					// Case: This word index is distinct
					//       => Generate unique shortened versions of that word, in order to find unique sets of initials
					if (targetWords.view.distinct.size == items.size) {
						// The other words are prefixed using a specific level (which may be increased between iterations)
						val before = items.map { _._2.view.take(i).map { shorten(_, otherPrefixLevel) }.mkString }
						val after = items.map { _._2.view.drop(i + 1).map { shorten(_, otherPrefixLevel) }.mkString }
						
						// Attempts to convert words in that index into a unique shorter versions
						uniqueVersionsOf(targetWords)
							.map { shortenedWords =>
								items.iterator.zipWithIndex
									.map { case ((original, parts, minLength), i) =>
										(original, parts, s"${ before(i) }${ shortenedWords(i) }${ after(i) }",
											minLength)
									}
									.toOptimizedSeq
							}
							// Minimum total length must be reached
							.filter { _.forall { case (_, _, initials, minLength) => initials.length >= minLength } }
							// Generated initials must be distinct
							.filter { initials => initials.iterator.map { _._3 }.distinct.size == initials.size }
							// There must be no overlap with the previously generated initials-sets
							.filter { _.forNone { case (_, _, initials, _) => used.contains(initials) } }
					}
					// Case: This word is not distinct => Continues to the next word
					else
						Empty
				}
			}
			// Selects the first valid result
			.next().map { case (original, parts, initials, _) => (original, parts, initials) }
	}
	
	private def shorten(word: String, prefixLevel: Int) = prefixLevel match {
		case 1 => word.take(1)
		case 2 => word.take(2)
		case 3 => twoCharConsonantPrefixFor(word)
		case prefixLevel =>
			val suffixLen = prefixLevel - 2
			val consonants = word.view.tail.filterNot(vocals.contains).take(suffixLen).mkString
			if (consonants.length == suffixLen)
				s"${ word.head }$consonants"
			else
				word.take(suffixLen + 1)
	}
	
	private def uniqueVersionsOf(words: Seq[String]) = {
		val count = words.size
		
		// Approach 1: Checks if the initials are different and uses those
		val option1 = Some(words.map { _.take(1) }).filter { _.iterator.distinct.size == count }
		// Approach 2: Checks if the first 2 characters are distinct
		val option2 = LazyInitIterator {
			Some(words.map { _.take(2) }).filter { _.iterator.distinct.size == count }
		}
		// Approach 3: Checks if 1st char + 1 consonant are distinct
		val option3 = LazyInitIterator {
			Some(words.map(twoCharConsonantPrefixFor)).filter { _.iterator.distinct.size == count }
		}
		// Approach 4: Finds a distinct consonant and appends that
		val options4 = PollableOnce {
			appendUniqueCharsTo(words.map { word =>
				val prefix = twoCharConsonantPrefixFor(word)
				val otherConsonants = word.drop(1).filterNot(vocals.contains).drop(1)
				prefix -> otherConsonants
			})
		}
		// Approach 5: Finds and appends distinct characters from each version
		val options5 = PollableOnce { appendUniqueCharsTo(words.map { word => word.take(2) -> word.drop(2) }) }
		
		option1.iterator ++ option2 ++ option3 ++ options4 ++ options5
	}
	
	private def twoCharConsonantPrefixFor(word: String) = word.view.drop(1).find { !vocals.contains(_) } match {
		case Some(consonant) => s"${word.head}$consonant"
		case None => word.take(2)
	}
	
	private def appendUniqueCharsTo(options: Seq[(String, String)]) = {
		// Appends characters until the options become distinct
		options.zipWithIndex.map { case ((prefix, chars), i) =>
			val others = options.iterator.zipWithIndex
				.filter { case ((prefix2, _), i2) => prefix2 == prefix && i2 != i }
				.map { _._1._2 }.toOptimizedSeq
			val suffix = (1 to chars.length)
				.findMap { takeLen =>
					val suffix = chars.take(takeLen)
					if (others.exists { _.take(takeLen) == suffix })
						None
					else
						Some(suffix)
				}
				.getOrElse(chars)
			s"$prefix$suffix"
		}
	}
	
	private def targetIndicesIteratorFor(options: Iterable[Iterable[_]]) = {
		val count = options.size
		val sizes = options.map { _.size }
		val lengthRange = sizes.minMax
		// Allows one shorter entry, but the others must all have an element at the included position
		(0 until lengthRange.first).iterator ++
			(lengthRange.first until lengthRange.second)
				.takeWhile { len => sizes.existsCount(count - 1) { _ >= len } }
	}
	
	private def writeDocumentation(doc: String)(implicit writer: PrintWriter) = {
		doc.notEmpty.foreach { doc =>
			val lines = doc.linesIterator.map(Regex.newLine.filterNot).toVector
			writer.println(s"-- ${lines.head}")
			lines.tail.foreach { line => writer.println(s"-- \t\t$line") }
		}
	}
}
