package utopia.coder.vault.model.data

import utopia.coder.model.data
import utopia.coder.model.data.{Name, NamingRules}
import utopia.coder.vault.model.datatype.StandardPropertyType.BasicPropertyType.{IntNumber, LongNumber}
import utopia.coder.vault.model.enumeration.IntSize.Default
import utopia.coder.model.enumeration.NamingConvention.CamelCase
import utopia.coder.vault.model.datatype.StandardPropertyType.{ClassReference, CreationTime, Deprecation, EnumValue, Expiration, UpdateTime}
import utopia.coder.model.enumeration.NameContext.{ColumnName, DbModelPropName}
import utopia.flow.collection.immutable.{Empty, Pair}

object Class
{
	/**
	  * Id / index name to use by default
	  */
	val defaultIdName = Name("id", "ids", CamelCase.lower)
	
	/**
	  * Creates a new class with automatic table-naming
	  * @param name Name of this class (in code)
	  * @param properties Properties in this class
	  * @param packageName Name of the package in which to wrap this class (default = empty)
	  * @param customSubPackageName Name to apply for this class's package when writing access points.
	  *                                   Empty if to be auto-generated (default).
	  * @param idName Name of this class' id property (default = id)
	  * @param description A description of this class (default = empty)
	  * @param author Author who wrote this class (may be empty)
	  * @param useLongId Whether to use long instead of int in the id property (default = false)
	  * @param writeGenericAccess Whether a generic access trait should be written for this class (includes combos)
	  *                           (default = false)
	  * @param isGeneric Whether this is a generic class / trait which may be extended by other classes
	 *                  (default = false)
	 * @return A new class
	  */
	def apply(name: Name, properties: Seq[Property], packageName: String = "", customSubPackageName: String = "",
	          comboIndexColumnNames: Seq[Seq[String]] = Empty, idName: Name = defaultIdName,
	          description: String = "", author: String = "", useLongId: Boolean = false,
	          writeGenericAccess: Boolean = false, isGeneric: Boolean = false): Class =
		apply(name, None, idName, properties, packageName, customSubPackageName, comboIndexColumnNames, None,
			description, author, useLongId, writeGenericAccess, isGeneric)
	
	/**
	  * Creates a new class with automatic table-naming with description support
	  * @param name Name of this class (in code)
	  * @param properties Properties in this class
	  * @param packageName Name of the package in which to wrap this class (default = empty)
	  * @param customSubPackageName Name to apply for this class's package when writing access points.
	  *                             Empty if to be auto-generated (default).
	  * @param idName Name of this class' id property (default = id)
	  * @param description A description of this class (default = empty)
	  * @param author Author who wrote this class (may be empty)
	  * @param descriptionLinkName Name of the property that refers to this class from a description link
	  *                            (default = autogenerated)
	  * @param useLongId Whether to use long instead of int in the id property (default = false)
	  * @param writeGenericAccess Whether a generic access trait should be written for this class (includes combos)
	  *                           (default = false)
	  * @param isGeneric Whether this is a generic class / trait which may be extended by other classes
	 *                  (default = false)
	 * @return A new class
	  */
	// WET WET
	def described(name: Name, properties: Seq[Property], packageName: String = "", customSubPackageName: String = "",
	              comboIndexColumnNames: Seq[Seq[String]] = Empty, idName: Name = defaultIdName,
	              description: String = "", author: String = "", descriptionLinkName: Option[Name] = None,
	              useLongId: Boolean = false, writeGenericAccess: Boolean = false, isGeneric: Boolean = false): Class =
	{
		apply(name, None, idName, properties, packageName, customSubPackageName, comboIndexColumnNames,
			Some[Name](descriptionLinkName.getOrElse { name + "id" }), description, author, useLongId,
			writeGenericAccess, isGeneric)
	}
}

/**
  * Represents a base class, from which multiple specific classes and interfaces are created
  * @author Mikko Hilpinen
  * @since 30.8.2021, v0.1
  * @param name Name of this class (in code)
  * @param customTableName Overridden name of this class' table (optional)
  * @param idName Name of this class' id (index) property
  * @param properties Properties in this class
  * @param packageName Name of the package in which to wrap this class (may be empty)
  * @param customAccessSubPackageName Name to apply for this class's package when writing access points.
  *                                   Empty if to be auto-generated.
  *                                   "-" if sub-packaging is to be omitted altogether.
  * @param comboIndexColumnNames Combo-indices for this class. Each item (vector) represents a single combo-index.
  *                              The items (strings) in combo-indices represent column names.
  * @param descriptionLinkName Name of the property that refers to this class from a description link (optional)
  * @param description A description of this class
  * @param useLongId Whether to use long instead of int in the id property
  * @param writeGenericAccess Whether a generic access trait should be written for this class (includes combos)
 * @param isGeneric Whether this is a generic class / trait which may be extended by other classes
  */
// TODO: customTableName should be Option[Name]
case class Class(name: Name, customTableName: Option[String], idName: Name, properties: Seq[Property],
                 packageName: String, customAccessSubPackageName: String, comboIndexColumnNames: Seq[Seq[String]],
                 descriptionLinkName: Option[Name], description: String, author: String, useLongId: Boolean,
                 writeGenericAccess: Boolean, isGeneric: Boolean)
{
	// ATTRIBUTES   ---------------------------------
	
	/**
	  * @return Class that links descriptions with instances of this class. None if not applicable to this class.
	  */
	lazy val descriptionLinkClass = descriptionLinkName.map { linkColumnName =>
		val tableName = customTableName match {
			case Some(n) => n: Name
			case None => name
		}
		val props = Pair(
			Property(linkColumnName, ClassReference(tableName, idName, idType),
				description = s"Id of the described $name"),
			Property(data.Name("descriptionId", CamelCase.lower), ClassReference("description"),
				description = "Id of the linked description")
		)
		Class(name + "description", props, "description",
			description = s"Links ${name.plural} with their descriptions", author = author)
	}
	
	
	// COMPUTED ------------------------------------
	
	/**
	  * @return Database-matching properties associated with this class
	  */
	def dbProperties = properties.view.flatMap { _.dbProperties }
	
	/**
	  * @return Whether this class uses integer type ids
	  */
	def useIntId = !useLongId
	
	/**
	  * @return Type of the ids used in this class
	  */
	def idType =  if (useLongId) LongNumber else IntNumber(Default)
	
	/**
	  * @return Whether this class supports description linking
	  */
	def isDescribed = descriptionLinkName.nonEmpty
	
	/**
	  * @return Whether this class records a row / instance creation time that is also an index
	  */
	def recordsIndexedCreationTime = properties.exists { p => p.dataType match {
		case CreationTime | UpdateTime => p.isIndexed
		case _ => false
	} }
	
	/**
	  * @return Whether this class supports deprecation or expiration
	  */
	def isDeprecatable = properties.exists { _.dataType match {
		case Deprecation | Expiration => true
		case _ => false
	} }
	/**
	  * @return Whether this class supports expiration
	  */
	def isExpiring = properties.exists { _.dataType.isInstanceOf[Expiration.type] }
	/**
	  * @return Whether this class supports deprecation
	  */
	def isNullDeprecatable = properties.exists { _.dataType.isInstanceOf[Deprecation.type] }
	
	/**
	  * @return Whether this class refers to one or more enumerations in its properties
	  */
	@deprecated("Deprecated for removal. Check whether conversion may fail instead.", "v1.6")
	def refersToEnumerations = properties.exists { _.dataType match {
		case _: EnumValue => true
		case _ => false
	} }
	
	/**
	  * @return Whether a conversion from a database-originated model may fail due to a property parsing failure
	  */
	def fromDbModelConversionMayFail = properties.exists { _.dataType.yieldsTryFromValue }
	/**
	  * @return Whether a conversion from a json model may fail due to a property parsing failure
	  */
	def fromJsonMayFail = properties.exists { _.dataType.yieldsTryFromJsonValue }
	
	/**
	  * @return The property in this class which contains instance creation time. None if no such property is present.
	  */
	def timestampProperty = dbProperties.find { _.sqlType.baseTypeSql == "TIMESTAMP" }
	
	/**
	  * @return Property in this class which contains instance deprecation time. None if no such property is present.
	  */
	def deprecationProperty = properties.find { _.dataType match {
		case Deprecation => true
		case _ => false
	} }
	
	/**
	  * @return Property in this class which contains instance expiration time. None if no such property is present.
	  */
	def expirationProperty = properties.find { _.dataType match {
		case Expiration => true
		case _ => false
	} }
	
	/**
	  * @param naming Implicit naming rules
	  * @return Table name used for this class
	  */
	def tableName(implicit naming: NamingRules) = customTableName.getOrElse { name.table }
	/**
	  * @param naming Implicit naming rules
	  * @return Name used for this class' id property in database model string literals
	  */
	def idDatabasePropName(implicit naming: NamingRules) =
		naming(DbModelPropName).convert(idName.column, naming(ColumnName))
}
