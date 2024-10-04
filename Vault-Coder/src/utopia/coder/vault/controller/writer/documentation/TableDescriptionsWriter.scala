package utopia.coder.vault.controller.writer.documentation

import utopia.coder.model.data.NamingRules
import utopia.coder.vault.model.data.{Class, Property}
import utopia.flow.generic.casting.ValueConversions._
import utopia.flow.generic.model.immutable.{Model, Value}
import utopia.flow.parse.file.FileExtensions._
import utopia.flow.util.StringExtensions._

import java.nio.file.Path
import scala.collection.immutable.VectorBuilder
import scala.io.Codec

/**
 * Used for writing json documents which contain detailed descriptive information of the project's database structure.
 * May be used for providing instructions for a software user, LLM, etc.
 *
 * @author Mikko Hilpinen
 * @since 24.07.2024, v1.11
 */
object TableDescriptionsWriter
{
	/**
	 * Writes a newline-delimited json document containing descriptions of all classes and all properties
	 * @param classes Classes to describe
	 * @param targetPath Path to the document to write
	 * @param codec Implicit encoding to apply
	 * @param naming Implicit naming logic to apply
	 * @return Success or failure
	 */
	def apply(classes: Iterable[Class], targetPath: Path)(implicit codec: Codec, naming: NamingRules) = {
		// Converts the classes into models and writes a ndjson file containing them
		targetPath.writeUsing { writer =>
			classes.foreach { classToWrite =>
				writer.println(
					Model.from(
						"name" -> classToWrite.name.doc, "table" -> classToWrite.tableName,
						"package" -> classToWrite.packageName,
							"description" -> classToWrite.description,
							"properties" -> classToWrite.properties.toVector.map { propertyModelFrom(_) })
						.withoutEmptyValues.toJson)
			}
		}
	}
	
	private def propertyModelFrom(prop: Property)(implicit naming: NamingRules) = {
		val propsBuilder = new VectorBuilder[(String, Value)]()
		propsBuilder += ("name" -> prop.name.doc)
		// TODO: Doesn't support column prefixes yet
		prop.oneOrManyDbVariants match {
			case Left(dbProp) =>
				propsBuilder += ("dataType" -> (dbProp.sqlType.baseTypeSql: Value))
				propsBuilder += ("column" -> (dbProp.name.column: Value))
				
			case Right(dbProps) =>
				val columnValues = dbProps.view
					.map { prop => Model.from("name" -> prop.columnName, "dataType" -> prop.sqlType.baseTypeSql) }
					.toVector
				propsBuilder += ("columns" -> (columnValues: Value))
		}
		prop.description.ifNotEmpty.foreach { desc => propsBuilder += ("description" -> desc) }
		
		Model(propsBuilder.result())
	}
}
