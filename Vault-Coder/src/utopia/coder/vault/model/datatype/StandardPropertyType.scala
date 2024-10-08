package utopia.coder.vault.model.datatype

import utopia.coder.model.data.{Name, NamingRules}
import utopia.coder.model.enumeration.NamingConvention.CamelCase
import utopia.coder.model.scala.code.CodePiece
import utopia.coder.model.scala.datatype.Reference.Flow._
import utopia.coder.model.scala.datatype.Reference._
import utopia.coder.model.scala.datatype.{Reference, ScalaType}
import utopia.coder.model.scala.template.ValueConvertibleType
import utopia.flow.collection.CollectionExtensions._
import utopia.flow.collection.immutable.{Empty, Pair, Single}
import utopia.flow.generic.casting.ValueConversions._
import utopia.flow.operator.equality.EqualsExtensions._
import utopia.flow.util.StringExtensions._
import utopia.coder.vault.controller.writer.model.EnumerationWriter
import utopia.coder.vault.model.data.{Class, Enum}
import utopia.coder.vault.model.datatype.StandardPropertyType.BasicPropertyType.{Date, DateTime, DoubleNumber, IntNumber, LongNumber}
import utopia.coder.vault.model.datatype.StandardPropertyType.TimeDuration.{fromValueReferences, toValueReferences}
import utopia.coder.vault.model.enumeration.{IntSize, Mutability}
import utopia.coder.vault.model.enumeration.IntSize.Default
import utopia.coder.vault.model.enumeration.Mutability.{Immutable, Mutable}
import utopia.coder.vault.util.VaultReferences._

import java.util.concurrent.TimeUnit

/**
  * Lists property types that introduced in this module
  * @author Mikko Hilpinen
  * @since 24.5.2023, v1.10
  */
object StandardPropertyType
{
	// OTHER    -------------------------------
	
	/**
	  * @param typeName     A property type name / string (case-insensitive)
	  * @param length       Associated property length (optional)
	  * @param propertyName Name specified for the property in question (optional)
	  * @return A property type matching that specification. None if no match was found.
	  */
	def interpret(typeName: String, length: Option[Int] = None, propertyName: Option[Name] = None): Option[PropertyType] =
	{
		// Text length may be specified within parentheses after the type (E.g. "String(3)")
		def appliedLength = typeName.afterFirst("(").untilFirst(")").untilFirst("-")
			.int.orElse(length).getOrElse(255)
		
		val lowerTypeName = typeName.toLowerCase
		lowerTypeName.untilFirst("(") match {
			case "requiredstring" | "nonemptystring" | "stringnotempty" | "textnotempty" =>
				Some(NonEmptyText(appliedLength))
			case "version" => Some(VersionType(appliedLength))
			case "creation" | "created" => Some(CreationTime)
			case "updated" | "modification" | "update" => Some(UpdateTime)
			case "deprecation" | "deprecated" => Some(Deprecation)
			case "expiration" | "expired" => Some(Expiration)
			case "value" | "val" => Some(GenericValue(appliedLength))
			case "model" | "values" => Some(GenericModel(appliedLength))
			case "days" => Some(DayCount)
			case "daterange" | "dates" => Some(DateRange)
			case "angle" => Some(AngleType)
			case "vector2d" | "doublevector" => Some(DoubleVector2D)
			case "latlong" | "latitudelongitude" | "geolocation" | "gps" => Some(LatitudeLongitudePair)
			case _ =>
				if (lowerTypeName.startsWith("text") || lowerTypeName.startsWith("string") || lowerTypeName.startsWith("varchar"))
					Some(Text(appliedLength))
				else if (lowerTypeName.startsWith("path") || lowerTypeName.startsWith("file"))
					Some(FilePath(appliedLength))
				else if (lowerTypeName.startsWith("option"))
					interpretGenericType(lowerTypeName, length, propertyName).map { _.optional }
				else if (lowerTypeName.startsWith("vector") ||
					(lowerTypeName.startsWith("[") && lowerTypeName.endsWith("]")))
					interpretGenericType(lowerTypeName, length, propertyName).map { t => VectorType(t, appliedLength) }
				else if (lowerTypeName.startsWith("pair"))
					interpretGenericType(lowerTypeName, length, propertyName).map(Paired)
				else if (lowerTypeName.startsWith("span") || lowerTypeName.startsWith("range")) {
					interpretGenericType(lowerTypeName, length, propertyName).map {
						case t: IntNumber => Spanning(t, isNumeric = true)
						case t => Spanning(t, isNumeric = t == DoubleNumber || t == LongNumber)
					}
				}
				else if (lowerTypeName.startsWith("duration"))
					Some(TimeDuration(timeUnitFromString(typeName.afterFirst("[").untilLast("]"), TimeUnit.MILLISECONDS)))
				else if (lowerTypeName.startsWith("velocity"))
					Some(LinearVelocity(timeUnitFromString(typeName.afterFirst("[").untilLast("]"), TimeUnit.SECONDS)))
				else if (lowerTypeName.startsWith("distance"))
					Some(LengthDistance(typeName.afterFirst("[").untilLast("]")))
				else
					BasicPropertyType.interpret(lowerTypeName, length, propertyName)
						.orElse {
							// If nothing else works, attempts to find the match with the specified property name
							propertyName.flatMap { name =>
								val options = Vector(Text(length.getOrElse(255)), CreationTime, UpdateTime,
									Deprecation, Expiration,
									GenericValue(length.getOrElse(255)), DayCount, TimeDuration.millis,
									LinearVelocity.perSecond)
								options.filter { _.defaultPropertyName ~== name }
									.maxByOption { _.defaultPropertyName.singular.length }
							}
						}
		}
	}
	
	// Interprets the generic type parameter. E.g. Option[String] would be interpreted as String (i.e. Text)
	private def interpretGenericType(lowerTypeName: String, length: Option[Int], propertyName: Option[Name]) =
		interpret(lowerTypeName.afterFirst("[").untilLast("]"), length, propertyName)
		
	private def timeUnitToString(unit: TimeUnit) = unit match {
		case TimeUnit.NANOSECONDS => "nano"
		case TimeUnit.MICROSECONDS => "microsecond"
		case TimeUnit.MILLISECONDS => "milli"
		case TimeUnit.SECONDS => "second"
		case TimeUnit.MINUTES => "minute"
		case TimeUnit.HOURS => "hour"
		case TimeUnit.DAYS => "day"
		case _ => unit.toString.toLowerCase
	}
	
	private def timeUnitFromString(str: String, default: => TimeUnit) = str.toLowerCase match {
		case "ms" | "milli" | "millis" | "millisecond" | "milliseconds" => TimeUnit.MILLISECONDS
		case "s" | "sec" | "second" | "seconds" => TimeUnit.SECONDS
		case "m" | "min" | "minute" | "minutes" => TimeUnit.MINUTES
		case "h" | "hour" | "hours" => TimeUnit.HOURS
		case "d" | "day" | "days" => TimeUnit.DAYS
		case _ => default
	}
	
	
	// NESTED   -----------------------------
	
	/**
	  * Basic property types are linked with simple data types and they are not nullable
	  */
	sealed trait BasicPropertyType extends ConcreteSingleColumnPropertyType
	{
		// ABSTRACT ------------------------------
		
		/**
		  * @return Name of the Value's property that converts to this data type (optional version, e.g. "int")
		  */
		def fromValuePropName: String
		
		/**
		  * @return The name of the DataType object used by the Value version of this type.
		  *         E.g. "StringType"
		  */
		def valueDataTypeName: String
		
		
		// IMPLEMENTED  --------------------------
		
		override def valueDataType = dataType / valueDataTypeName
		
		override def defaultMutability: Option[Mutability] = None
		
		override def yieldsTryFromValue = false
		override def yieldsTryFromJsonValue: Boolean = false
		
		override def toValueCode(instanceCode: String) = CodePiece(instanceCode, Set(valueConversions))
		override def toJsonValueCode(instanceCode: String): CodePiece = toValueCode(instanceCode)
		override def optionToValueCode(optionCode: String, isToJson: Boolean) =
			CodePiece(optionCode, Set(valueConversions))
		
		override def fromValueCode(valueCode: String, isFromJson: Boolean) =
			CodePiece(s"$valueCode.get${ fromValuePropName.capitalize }")
		override def optionFromValueCode(valueCode: String, isFromJson: Boolean) = s"$valueCode.$fromValuePropName"
		
		override def writeDefaultDescription(className: Name, propName: Name)(implicit naming: NamingRules) = ""
	}
	
	object BasicPropertyType
	{
		// COMPUTED -----------------------
		
		private def objectValues = Vector(LongNumber, DoubleNumber, Bool, DateTime, Date, Time)
		
		
		// OTHER    -----------------------
		
		/**
		  * @param typeName        A property type name / string
		  * @param specifiedLength Associated property length, if specified (optional)
		  * @param propertyName    Name specified for the property (optional)
		  * @return Basic property type matching that specification. None if no match was found.
		  */
		def interpret(typeName: String, specifiedLength: Option[Int] = None, propertyName: Option[Name] = None) =
		{
			val lowerName = typeName
			
			def _findWith(searches: Iterable[BasicPropertyType => Boolean]) =
				searches.findMap { search => objectValues.find(search) }
			
			if (lowerName.contains("int")) {
				// Int size may be specified in parentheses after the type name
				// E.g. "Int(Tiny)" => TINYINT or "INT(320)" => SMALLINT
				val lengthPart = lowerName.afterFirst("(").untilFirst(")").ifNotEmpty
				val (size, maxValue) = lengthPart match {
					case Some(s) =>
						IntSize.values.find { size => (size.toString ~== s) || (size.toSql ~== s) } match {
							// Case: Size is specified by name
							case Some(size) => size -> None
							case None =>
								s.int match {
									// Case: Size is specified by maximum value
									case Some(maxValue) =>
										IntSize.values.find { _.maxValue >= maxValue } match {
											case Some(size) => size -> Some(maxValue)
											case None => Default -> None
										}
									// Case: Size can't be parsed
									case None => Default -> None
								}
						}
					case None =>
						// If parentheses are not used, checks the "length" property as well,
						// comparing it to integer maximum length (characters-wise)
						specifiedLength match {
							case Some(maxLength) =>
								IntSize.values.find { _.maxLength >= maxLength } match {
									// Case: Max length is specified and fits into an integer
									case Some(size) =>
										size -> Vector.fill(maxLength)('9').mkString.int.filter { _ < size.maxValue }
									// Case: Max length is specified but is too large
									case None => Default -> None
								}
							// Case: No size or length is specified
							case None => Default -> None
						}
				}
				Some(IntNumber(size, maxValue))
			}
			else
				_findWith(Vector(
					v => v.fromValuePropName.toLowerCase == lowerName,
					v => v.toScala.toString.toLowerCase == lowerName,
					v => v.sqlType.baseTypeSql.toLowerCase == lowerName,
					v => v.defaultPropertyName.variants.exists { _.toLowerCase == lowerName }
				)).orElse {
					// Attempts to find with property name also
					propertyName.flatMap { name =>
						val lowerName = name.singularIn(CamelCase.lower).toLowerCase
						if (lowerName.startsWith("is") || lowerName.startsWith("was"))
							Some(Bool)
						else
							objectValues.filter { _.defaultPropertyName ~== name }
								.maxByOption { _.defaultPropertyName.singular.length }
					}
				}
		}
		
		
		// NESTED   -----------------------------
		
		/**
		  * Long / Bigint property type
		  */
		case object LongNumber extends BasicPropertyType
		{
			override val sqlType = SqlPropertyType("BIGINT")
			override lazy val valueDataTypeName = "LongType"
			
			override def isFilterGenerationSupported: Boolean = true
			override def supportsDefaultJsonValues = true
			
			override def scalaType = ScalaType.long
			
			override def emptyValue = CodePiece.empty
			override def nonEmptyDefaultValue = CodePiece.empty
			
			override def fromValuePropName = "long"
			override def defaultPropertyName = "number"
		}
		/**
		  * Double property type
		  */
		case object DoubleNumber extends BasicPropertyType
		{
			override val sqlType = SqlPropertyType("DOUBLE")
			override lazy val valueDataTypeName = "DoubleType"
			
			override def scalaType = ScalaType.double
			
			override def nonEmptyDefaultValue = CodePiece.empty
			override def emptyValue = CodePiece.empty
			
			override def isFilterGenerationSupported: Boolean = false
			override def supportsDefaultJsonValues = true
			
			override def fromValuePropName = "double"
			override def defaultPropertyName = "amount"
		}
		
		/**
		  * Boolean property type
		  */
		case object Bool extends BasicPropertyType
		{
			override val sqlType = SqlPropertyType("BOOLEAN", "FALSE")
			override lazy val valueDataTypeName = "BooleanType"
			
			override def isFilterGenerationSupported: Boolean = true
			override def supportsDefaultJsonValues = true
			
			override def scalaType = ScalaType.boolean
			
			override def nonEmptyDefaultValue = "false"
			override def emptyValue = CodePiece.empty
			
			override def fromValuePropName = "boolean"
			override def defaultPropertyName = "flag"
		}
		
		/**
		  * Date + Time (UTC) / Instant / Datetime property type.
		  */
		case object DateTime extends BasicPropertyType
		{
			override val sqlType = SqlPropertyType("DATETIME")
			override lazy val valueDataTypeName = "InstantType"
			
			override def isFilterGenerationSupported: Boolean = false
			override def supportsDefaultJsonValues = false
			
			override def scalaType = Reference.instant
			
			override def nonEmptyDefaultValue = now.targetCode
			override def emptyValue = CodePiece.empty
			
			override def fromValuePropName = "instant"
			override def defaultPropertyName = "timestamp"
		}
		
		/**
		  * Date / LocalDate type
		  */
		case object Date extends BasicPropertyType
		{
			override val sqlType = SqlPropertyType("DATE")
			override lazy val valueDataTypeName = "LocalDateType"
			
			override def isFilterGenerationSupported: Boolean = true
			override def supportsDefaultJsonValues = false
			
			override def scalaType = Reference.localDate
			
			override def nonEmptyDefaultValue = today.targetCode
			override def emptyValue = CodePiece.empty
			
			override def fromValuePropName = "localDate"
			override def defaultPropertyName = "date"
		}
		
		/**
		  * Time / LocalTime type
		  */
		case object Time extends BasicPropertyType
		{
			override val sqlType = SqlPropertyType("TIME")
			override lazy val valueDataTypeName = "LocalTimeType"
			
			override def supportsDefaultJsonValues = false
			override def isFilterGenerationSupported: Boolean = false
			
			override def scalaType = Reference.localTime
			
			override def nonEmptyDefaultValue = now.targetCode
			override def emptyValue = CodePiece.empty
			
			override def fromValuePropName = "localTime"
			override def defaultPropertyName = "time"
		}
		
		/*
		case class Text(length: Int = 255) extends BasicPropertyType {
			
			override val sqlType = SqlPropertyType(s"VARCHAR($length)")
			override val emptyValue = "\"\""
			
			override def scalaType = ScalaType.string
			
			override def nonEmptyDefaultValue = CodePiece.empty
			
			override def fromValuePropName = "string"
			override def defaultPropertyName = if (length < 100) "name" else "text"
		}*/
		
		object IntNumber
		{
			/**
			  * Creates a new int type with a specific maximum value
			  * @param maxValue Maximum integer value
			  * @return Type incorporating that maximum value
			  */
			def apply(maxValue: Int): IntNumber = apply(IntSize.fitting(maxValue).getOrElse(Default), Some(maxValue))
		}
		
		/**
		  * Standard integer property type
		  */
		case class IntNumber(size: IntSize = Default, maxValue: Option[Int] = None) extends BasicPropertyType
		{
			override lazy val sqlType = SqlPropertyType(maxValue match {
				case Some(max) => s"${ size.toSql }(${ max.toString.length })"
				case None => size.toSql
			})
			override lazy val valueDataTypeName = "IntType"
			
			override def isFilterGenerationSupported: Boolean = true
			override def supportsDefaultJsonValues = true
			
			override def scalaType = ScalaType.int
			
			override def nonEmptyDefaultValue = CodePiece.empty
			override def emptyValue = CodePiece.empty
			
			override def fromValuePropName = "int"
			override def defaultPropertyName = Name("index", "indices", CamelCase.lower)
		}
	}
	
	/**
	  * Property that always sets to the instance creation time
	  */
	case object CreationTime extends ConcreteSingleColumnPropertyType
	{
		// ATTRIBUTES   ------------------------
		
		override lazy val sqlType = SqlPropertyType("TIMESTAMP", "CURRENT_TIMESTAMP", indexByDefault = true)
		override lazy val defaultPropertyName = Name("created", "creationTimes", CamelCase.lower)
		
		
		// IMPLEMENTED  ------------------------
		
		override def scalaType = Reference.instant
		override def valueDataType = instantType
		
		override def emptyValue = CodePiece.empty
		override def nonEmptyDefaultValue = now.targetCode
		
		override def defaultMutability: Option[Mutability] = Some(Immutable)
		
		override def isFilterGenerationSupported: Boolean = false
		override def supportsDefaultJsonValues = false
		override def yieldsTryFromValue = false
		override def yieldsTryFromJsonValue: Boolean = false
		
		override def fromValueCode(valueCode: String, isFromJson: Boolean) = CodePiece(s"$valueCode.getInstant")
		
		override def toValueCode(instanceCode: String) = CodePiece(instanceCode, Set(valueConversions))
		override def toJsonValueCode(instanceCode: String): CodePiece = toValueCode(instanceCode)
		
		override def optionFromValueCode(valueCode: String, isFromJson: Boolean) = s"$valueCode.instant"
		override def optionToValueCode(optionCode: String, isToJson: Boolean) =
			CodePiece(optionCode, Set(valueConversions))
		
		override def writeDefaultDescription(className: Name, propName: Name)(implicit naming: NamingRules) =
			s"Time when this ${ className.doc } was added to the database"
	}
	/**
	  * Property that always sets to the instance creation time
	  */
	case object UpdateTime extends SingleColumnPropertyTypeWrapper
	{
		// ATTRIBUTES   -------------------------
		
		override lazy val sqlConversion =
			wrapped.sqlConversion.modifyTarget(defaultValue = "CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP")
		override lazy val defaultPropertyName = Name("lastUpdated", "lastUpdateTimes", CamelCase.lower)
		
		
		// IMPLEMENTED  -------------------------
		
		override protected def wrapped = CreationTime
		
		override def defaultMutability: Option[Mutability] = Some(Mutable)
		
		override def optional = DateTime.optional
		override def concrete = this
		
		override def writeDefaultDescription(className: Name, propName: Name)(implicit naming: NamingRules) =
			s"Time when this ${ className.doc } was last updated"
	}
	
	/**
	  * Property that is null until the instance is deprecated, after which the property contains a timestamp of that
	  * deprecation event
	  */
	case object Deprecation extends SingleColumnPropertyTypeWrapper
	{
		// ATTRIBUTES   -------------------------
		
		override lazy val sqlConversion = wrapped.sqlConversion.modifyTarget(indexByDefault = true)
		override lazy val defaultPropertyName = Name("deprecatedAfter", "deprecationTimes", CamelCase.lower)
		
		
		// IMPLEMENTED  -------------------------
		
		protected def wrapped = DateTime.OptionWrapped
		
		override def defaultMutability: Option[Mutability] = Some(Mutable)
		
		override def optional = this
		override def concrete = Expiration
		
		override def writeDefaultDescription(className: Name, propName: Name)(implicit naming: NamingRules) =
			s"Time when this ${ className.doc } became deprecated. None while this ${ className.doc } is still valid."
	}
	/**
	  * Contains a time threshold for instance deprecation
	  */
	case object Expiration extends SingleColumnPropertyTypeWrapper
	{
		override lazy val sqlConversion = wrapped.sqlConversion.modifyTarget(indexByDefault = true)
		override lazy val defaultPropertyName = Name("expires", "expirationTimes", CamelCase.lower)
		
		protected def wrapped = DateTime
		
		override def defaultMutability: Option[Mutability] = None
		
		override def optional = Deprecation
		override def concrete = this
		
		override def writeDefaultDescription(className: Name, propName: Name)(implicit naming: NamingRules) =
			s"Time when this ${ className.doc } expires / becomes invalid"
	}
	
	/**
	  * Represents a number of days
	  */
	case object DayCount extends ConcreteSingleColumnPropertyType
	{
		// ATTRIBUTES   -----------------------
		
		override val sqlType = SqlPropertyType("INT", "0", "days")
		override lazy val nonEmptyDefaultValue = CodePiece("Days.zero", Set(days))
		override lazy val valueDataType = dataType / "DaysType"
		
		
		// IMPLEMENTED  ----------------------
		
		override def scalaType = days
		
		override def emptyValue = CodePiece.empty
		
		override def defaultMutability: Option[Mutability] = None
		override def defaultPropertyName = "duration"
		
		override def isFilterGenerationSupported: Boolean = true
		override def supportsDefaultJsonValues = true
		override def yieldsTryFromValue = false
		override def yieldsTryFromJsonValue: Boolean = false
		
		override def toValueCode(instanceCode: String) =
			CodePiece(s"$instanceCode.length", Set(valueConversions))
		override def toJsonValueCode(instanceCode: String): CodePiece = CodePiece(instanceCode, Set(valueConversions))
		override def optionToValueCode(optionCode: String, isToJson: Boolean) = {
			if (isToJson)
				CodePiece(optionCode, Set(valueConversions))
			else
				CodePiece(s"$optionCode.map { _.length }", Set(valueConversions))
		}
		
		override def fromValueCode(valueCode: String, isFromJson: Boolean) = {
			// Json uses a direct value conversion
			if (isFromJson)
				CodePiece(s"$valueCode.getDays")
			else
				CodePiece(s"Days($valueCode.getInt)", Set(days))
		}
		override def optionFromValueCode(valueCode: String, isFromJson: Boolean) = {
			if (isFromJson)
				CodePiece(s"$valueCode.days")
			else
				CodePiece(s"$valueCode.int.map { Days(_) }", Set(days))
		}
		
		override def writeDefaultDescription(className: Name, propName: Name)(implicit naming: NamingRules) = ""
	}
	
	object Text
	{
		private val emptyValue = "\"\""
	}
	// This text is never wrapped in an option. An empty string is considered an empty value.
	case class Text(length: Int = 255) extends DirectlySqlConvertiblePropertyType
	{
		// ATTRIBUTES   ---------------------
		
		override val sqlType = {
			val typeName = {
				if (length > 16777215)
					"LONGTEXT"
				else if (length > 65535)
					"MEDIUMTEXT"
				else
					s"VARCHAR($length)"
			}
			SqlPropertyType(typeName, isNullable = true)
		}
		
		
		// IMPLEMENTED  --------------------
		
		override def scalaType = ScalaType.string
		override def valueDataType = stringType
		
		override def emptyValue = Text.emptyValue
		override def nonEmptyDefaultValue = CodePiece.empty
		
		override def concrete = this
		
		override def defaultMutability: Option[Mutability] = None
		override def defaultPropertyName = "text"
		
		override def isFilterGenerationSupported: Boolean = true
		override def supportsDefaultJsonValues = true
		override def yieldsTryFromValue = false
		override def yieldsTryFromJsonValue: Boolean = false
		
		override def toValueCode(instanceCode: String) = CodePiece(instanceCode, Set(valueConversions))
		override def toJsonValueCode(instanceCode: String): CodePiece = toValueCode(instanceCode)
		
		override def fromValueCode(valueCode: String, isFromJson: Boolean) = s"$valueCode.getString"
		override def fromValuesCode(valuesCode: String) = s"$valuesCode.flatMap { _.string }"
		override def fromConcreteCode(concreteCode: String): CodePiece = concreteCode
		
		override def writeDefaultDescription(className: Name, propName: Name)(implicit naming: NamingRules) = ""
	}
	
	// Works exactly like Text, except that
	// a) No default empty value is given (this type is not considered optional)
	// b) NOT NULL is added to the generated sql type
	case class NonEmptyText(length: Int = 255) extends SingleColumnPropertyType
	{
		// ATTRIBUTES   ------------------------
		
		private val allowingEmpty = Text(length)
		
		
		// IMPLEMENTED  ------------------------
		
		override def scalaType = allowingEmpty.scalaType
		override def valueDataType = stringType
		override def sqlConversion: SqlTypeConversion = SqlConversion
		
		// Empty value is not allowed
		override def emptyValue = CodePiece.empty
		override def nonEmptyDefaultValue = CodePiece.empty
		
		override def concrete = this
		override def optional = allowingEmpty
		
		override def defaultMutability: Option[Mutability] = None
		override def defaultPropertyName = if (length < 100) "name" else "text"
		
		override def isFilterGenerationSupported: Boolean = true
		override def supportsDefaultJsonValues = true
		override def yieldsTryFromValue = allowingEmpty.yieldsTryFromValue
		override def yieldsTryFromJsonValue: Boolean = allowingEmpty.yieldsTryFromJsonValue
		
		// Delegates value conversion
		override def fromValueCode(valueCode: String, isFromJson: Boolean) =
			allowingEmpty.fromValueCode(valueCode, isFromJson)
		override def fromValuesCode(valuesCode: String) = allowingEmpty.fromValuesCode(valuesCode)
		override def fromConcreteCode(concreteCode: String): CodePiece = concreteCode
		
		override def toValueCode(instanceCode: String) = allowingEmpty.toValueCode(instanceCode)
		override def toJsonValueCode(instanceCode: String): CodePiece = allowingEmpty.toJsonValueCode(instanceCode)
		
		override def writeDefaultDescription(className: Name, propName: Name)(implicit naming: NamingRules) = ""
		
		
		// NESTED   -------------------------
		
		// Simply modifies the "NOT NULL" -part from a standard string sql conversion, otherwise works exactly the same
		private object SqlConversion extends SqlTypeConversion
		{
			// COMPUTED ---------------------
			
			private def parent = NonEmptyText.this
			
			
			// IMPLEMENTED  -----------------
			
			override def origin = parent.scalaType
			
			override def intermediate = parent.allowingEmpty
			
			override def target = intermediate.sqlType.notNullable
			
			override def midConversion(originCode: String) = originCode
		}
	}
	
	/**
	 * Represents a path to a file or a directory
	 * @param length Maximum number of characters allocated for this path
	 */
	// TODO: This type requires a custom Option to String conversion
	case class FilePath(length: Int = 255) extends FacadePropertyType
	{
		// ATTRIBUTES   ------------------------
		
		override protected val delegate: PropertyType = Text(length)
		
		
		// IMPLEMENTED  ------------------------
		
		override def scalaType: ScalaType = path
		
		override def emptyValue: CodePiece = CodePiece.empty
		override def nonEmptyDefaultValue: CodePiece = CodePiece.empty
		override def defaultPropertyName: Name = "file"
		override def defaultPartNames: Seq[Name] = Vector.empty
		override def defaultMutability: Option[Mutability] = None
		
		override def supportsDefaultJsonValues: Boolean = true
		override protected def yieldsTryFromDelegate: Boolean = false
		
		override protected def toDelegateCode(instanceCode: String): CodePiece =
			CodePiece(s"$instanceCode.toJson", Set(fileExtensions))
		override protected def fromDelegateCode(delegateCode: String): CodePiece =
			CodePiece(s"$delegateCode: Path", Set(path, fileExtensions))
		
		override def writeDefaultDescription(className: Name, propName: Name)(implicit naming: NamingRules): String = ""
	}
	
	// TODO: This type requires a custom Option to String conversion
	case class VersionType(length: Int = 128) extends FacadePropertyType
	{
		// ATTRIBUTES   ----------------------
		
		override protected val delegate: PropertyType = Text(length)
		
		override lazy val nonEmptyDefaultValue: CodePiece = CodePiece("Version(1)", Set(flow.version))
		
		
		// IMPLEMENTED  ----------------------
		
		override def scalaType: ScalaType = flow.version
		
		override def emptyValue: CodePiece = CodePiece.empty
		
		override def defaultPropertyName: Name = "version"
		override def defaultPartNames: Seq[Name] = Empty
		
		override def defaultMutability: Option[Mutability] = None
		
		override def supportsDefaultJsonValues: Boolean = true
		override protected def yieldsTryFromDelegate: Boolean = false
		
		override protected def toDelegateCode(instanceCode: String): CodePiece = s"$instanceCode.toString"
		
		override protected def fromDelegateCode(delegateCode: String): CodePiece =
			CodePiece(s"Version($delegateCode)", Set(flow.version))
		
		override def writeDefaultDescription(className: Name, propName: Name)(implicit naming: NamingRules): String =
			s"Version of this ${ className.doc }"
	}
	
	case class GenericValue(length: Int = 255) extends DirectlySqlConvertiblePropertyType
	{
		// ATTRIBUTES   ----------------------
		
		override lazy val sqlType = SqlPropertyType(s"VARCHAR($length)", isNullable = true)
		override lazy val valueDataType = dataType / "AnyType"
		
		
		// IMPLEMENTED  ---------------------
		
		override def scalaType = value
		
		override def isFilterGenerationSupported: Boolean = true
		override def yieldsTryFromValue = false
		override def yieldsTryFromJsonValue: Boolean = false
		override def supportsDefaultJsonValues = true
		
		override def nonEmptyDefaultValue = CodePiece.empty
		override def emptyValue = CodePiece("Value.empty", Set(value))
		
		override def concrete = this
		
		override def defaultPropertyName = "value"
		override def defaultMutability: Option[Mutability] = None
		
		// Converts the value to a json string before converting it back to a value
		override def toValueCode(instanceCode: String) =
			CodePiece(s"$instanceCode.mapIfNotEmpty { _.toJson }", Set(valueConversions))
		override def toJsonValueCode(instanceCode: String): CodePiece = instanceCode
		override def fromValueCode(valueCode: String, isFromJson: Boolean) = {
			// When the value originates from the database, expects it to be represented as a json string,
			// which still needs parsing
			if (isFromJson)
				valueCode
			else
				CodePiece(s"$valueCode.mapIfNotEmpty { v => JsonBunny.sureMunch(v.getString) }",
					Set(bunnyMunch.jsonBunny))
		}
		// Expects a vector of json string values
		override def fromValuesCode(valuesCode: String) =
			fromValueCode("v").mapText { fromValue => s"$valuesCode.map { v => $fromValue }" }
		override def fromConcreteCode(concreteCode: String): CodePiece = concreteCode
		
		override def writeDefaultDescription(className: Name, propName: Name)(implicit naming: NamingRules) =
			s"Generic ${ propName.doc } of this ${ className.doc }"
	}
	
	// WET WET (from GenericValue)
	case class GenericModel(length: Int = 255) extends DirectlySqlConvertiblePropertyType
	{
		// ATTRIBUTES   -----------------------
		
		override lazy val sqlType = SqlPropertyType(s"VARCHAR($length)", isNullable = true)
		override lazy val valueDataType = dataType / "ModelType"
		
		
		// IMPLEMENTED  ----------------------
		
		override def scalaType = model
		
		override def isFilterGenerationSupported: Boolean = false
		override def yieldsTryFromValue = false
		override def yieldsTryFromJsonValue: Boolean = false
		
		override def nonEmptyDefaultValue = CodePiece.empty
		override def emptyValue = CodePiece("Model.empty", Set(model))
		
		override def supportsDefaultJsonValues = true
		
		override def concrete = this
		
		override def defaultPropertyName = "values"
		override def defaultMutability: Option[Mutability] = None
		
		// Converts the value to a json string before converting it back to a value
		// Empty models are not represented by json, are empty
		override def toValueCode(instanceCode: String) =
			CodePiece(s"$instanceCode.notEmpty.map { _.toJson }", Set(valueConversions))
		override def toJsonValueCode(instanceCode: String): CodePiece = instanceCode
		
		override def fromValueCode(valueCode: String, isFromJson: Boolean) = {
			// When the value originates from the database, expects it to be represented as a json string,
			// which still needs parsing
			if (isFromJson)
				s"$valueCode.getModel"
			else
				CodePiece(s"$valueCode.notEmpty match { case Some(v) => JsonBunny.sureMunch(v.getString).getModel; case None => Model.empty }",
					Set(bunnyMunch.jsonBunny, model))
		}
		// Expects a vector of json string values
		override def fromValuesCode(valuesCode: String) =
			fromValueCode("v").mapText { fromValue => s"$valuesCode.map { v => $fromValue }" }
		override def fromConcreteCode(concreteCode: String): CodePiece = concreteCode
		
		override def writeDefaultDescription(className: Name, propName: Name)(implicit naming: NamingRules) = ""
	}
	
	case object TimeDuration
	{
		private val fromValueReferences = Set(Reference.timeUnit, Reference.finiteDuration)
		private val toValueReferences = Set(valueConversions, Reference.timeUnit)
		
		val millis = apply(TimeUnit.MILLISECONDS)
		val seconds = apply(TimeUnit.SECONDS)
		val minutes = apply(TimeUnit.MINUTES)
		val hours = apply(TimeUnit.HOURS)
		
		def values = Vector(millis, seconds, minutes, hours)
	}
	
	/**
	  * Represents a duration of time
	  * @param unit Unit used when storing this duration to the database
	  */
	case class TimeDuration(unit: TimeUnit) extends ConcreteSingleColumnPropertyType
	{
		// ATTRIBUTES   ------------------------
		
		override lazy val sqlType = SqlPropertyType(unit match {
			case TimeUnit.NANOSECONDS | TimeUnit.MICROSECONDS | TimeUnit.MILLISECONDS => "BIGINT"
			case _ => "INT"
		}, "0", s"${ timeUnitToString(unit) }s")
		override lazy val valueDataType = dataType / "DurationType"
		
		
		// COMPUTED ----------------------------
		
		private def unitConversionCode = s".toUnit(TimeUnit.${ unit.name })"
		
		
		// IMPLEMENTED  ------------------------
		
		override def scalaType = Reference.finiteDuration
		
		override def emptyValue = CodePiece.empty
		override def nonEmptyDefaultValue = CodePiece("Duration.Zero", Set(Reference.duration))
		
		override def defaultPropertyName = "duration"
		override def defaultMutability: Option[Mutability] = None
		
		override def isFilterGenerationSupported: Boolean = false
		override def supportsDefaultJsonValues = true
		
		override def yieldsTryFromValue = false
		override def yieldsTryFromJsonValue: Boolean = false
		
		override def toValueCode(instanceCode: String) =
			CodePiece(s"$instanceCode$unitConversionCode", toValueReferences)
		override def toJsonValueCode(instanceCode: String): CodePiece = CodePiece(instanceCode, Set(valueConversions))
		override def optionToValueCode(optionCode: String, isToJson: Boolean) = {
			if (isToJson)
				CodePiece(optionCode, Set(valueConversions))
			else
				CodePiece(s"$optionCode.map { _$unitConversionCode }", toValueReferences)
		}
		
		override def fromValueCode(valueCode: String, isFromJson: Boolean) = {
			if (isFromJson)
				s"$valueCode.getDuration"
			else
				CodePiece(s"FiniteDuration($valueCode.getLong, TimeUnit.${ unit.name })", fromValueReferences)
		}
		override def optionFromValueCode(valueCode: String, isFromJson: Boolean) = {
			if (isFromJson)
				s"$valueCode.duration"
			else
				CodePiece(s"$valueCode.long.map { FiniteDuration(_, TimeUnit.${ unit.name }) }", fromValueReferences)
		}
		
		override def writeDefaultDescription(className: Name, propName: Name)(implicit naming: NamingRules) =
			s"Duration of this ${ className.doc }"
	}
	
	/**
	  * Property that refers another class / table
	  * @param referencedTableName  Name of the referenced table
	  * @param referencedColumnName Name of the column being referred to (default = id)
	  * @param referencedType       The type of the referenced column (default = standard integer)
	  */
	case class ClassReference(referencedTableName: Name, referencedColumnName: Name = Class.defaultIdName,
	                          referencedType: PropertyType = IntNumber())
		extends PropertyTypeWrapper
	{
		override protected def wrapped = referencedType
		
		override def sqlConversions =
			super.sqlConversions.map { _.modifyTarget(indexByDefault = false) }
		
		override def optional =
			if (referencedType.isOptional) this else copy(referencedType = referencedType.optional)
		
		override def concrete =
			if (referencedType.isConcrete) this else copy(referencedType = referencedType.concrete)
		
		override def defaultPropertyName: Name = referencedTableName + "id"
		override def defaultPartNames: Seq[Name] = Empty
		override def defaultMutability: Option[Mutability] = if (referencedType.isOptional) Some(Mutable) else None
		
		override def writeDefaultDescription(className: Name, propName: Name)(implicit naming: NamingRules) =
			s"${ referencedColumnName.doc.capitalize } of the ${ referencedTableName.doc } linked with this ${ className.doc }"
	}
	
	/**
	  * Property type that accepts enumeration values
	  * @param enumeration Enumeration from which the values are picked
	  */
	case class EnumValue(enumeration: Enum)(implicit naming: NamingRules) extends PropertyType
	{
		// ATTRIBUTES   ----------------------------
		
		override lazy val sqlConversions: Seq[SqlTypeConversion] =
			enumeration.idType.sqlConversions.map { new EnumIdSqlConversion(_) }
		
		private lazy val findForId = s"${ enumeration.name.enumName }.${ EnumerationWriter.findForIdName(enumeration) }"
		// private lazy val forIdName = EnumerationWriter.forIdName(enumeration)
		
		
		// IMPLEMENTED  ---------------------------
		
		private def colNameSuffix = enumeration.idPropName.column
		
		override def scalaType = enumeration.reference
		override def valueDataType = enumeration.idType.valueDataType
		
		override def isFilterGenerationSupported: Boolean = true
		override def supportsDefaultJsonValues = true
		
		override def optional: PropertyType = Optional
		override def concrete = this
		
		override def nonEmptyDefaultValue = enumeration.defaultValue match {
			case Some(default) =>
				val valueName = default.name.enumValue
				CodePiece(valueName, Set(enumeration.reference / valueName))
			case None => CodePiece.empty
		}
		override def emptyValue = CodePiece.empty
		
		override def defaultPropertyName = enumeration.name
		override def defaultPartNames: Seq[Name] = Empty
		override def defaultMutability: Option[Mutability] = None
		
		override def yieldsTryFromValue = enumeration.hasNoDefault
		override def yieldsTryFromJsonValue: Boolean = yieldsTryFromValue
		
		override def toValueCode(instanceCode: String) =
			enumeration.idType.toValueCode(s"$instanceCode.${ enumeration.idPropName.prop }")
		override def toJsonValueCode(instanceCode: String): CodePiece = toValueCode(instanceCode)
		
		// NB: Doesn't support multi-column enumeration id types
		override def fromValueCode(valueCodes: Seq[String]) =
			CodePiece(s"${ enumeration.name.enumName }.fromValue(${ valueCodes.head })", Set(enumeration.reference))
		override def fromJsonValueCode(valueCode: String): CodePiece = fromValueCode(Single(valueCode))
		override def fromValuesCode(valuesCode: String) = {
			val idFromValueCode = enumeration.idType.fromValueCode(Vector("v"))
			idFromValueCode.mapText { convertToId =>
				s"$valuesCode.map { v => $convertToId }.flatMap($findForId)"
			}.referringTo(enumeration.reference)
		}
		override def fromConcreteCode(concreteCode: String): CodePiece = concreteCode
		
		override def writeDefaultDescription(className: Name, propName: Name)(implicit naming: NamingRules) =
			s"${ enumeration.name.doc.capitalize } of this ${ className.doc }"
		
		private object Optional extends PropertyType
		{
			private lazy val idType = enumeration.idType.optional
			override lazy val sqlConversions: Seq[SqlTypeConversion] =
				idType.sqlConversions.map { new EnumIdOptionSqlConversion(_) }
			
			override def scalaType = ScalaType.option(EnumValue.this.scalaType)
			override def valueDataType = EnumValue.this.valueDataType
			
			override def isFilterGenerationSupported: Boolean = true
			override def supportsDefaultJsonValues = false
			
			override def nonEmptyDefaultValue = CodePiece.empty
			override def emptyValue = CodePiece.none
			
			override def defaultPropertyName = EnumValue.this.defaultPropertyName
			override def defaultPartNames: Seq[Name] = EnumValue.this.defaultPartNames
			override def defaultMutability: Option[Mutability] = EnumValue.this.defaultMutability
			
			override def optional = this
			override def concrete = EnumValue.this
			
			override def yieldsTryFromValue = false
			override def yieldsTryFromJsonValue: Boolean = false
			
			override def toValueCode(instanceCode: String) =
				idType.toValueCode(s"e.${ enumeration.idPropName.prop }")
					.mapText { idToValue => s"$instanceCode.map[Value] { e => $idToValue }.getOrElse(Value.empty)" }
					.referringTo(value)
			override def toJsonValueCode(instanceCode: String): CodePiece = toValueCode(instanceCode)
			
			override def fromValueCode(valueCodes: Seq[String]) =
				idType.fromValueCode(valueCodes)
					.mapText { id =>
						// Types which are at the same time concrete and non-concrete, are handled a bit differently
						def fromConcrete = s"$findForId($id)"
						
						idType match {
							case Text(_) => fromConcrete
							case NonEmptyText(_) => fromConcrete
							case GenericValue(_) => fromConcrete
							case t =>
								if (t.concrete == t)
									fromConcrete
								else
									s"$id.flatMap($findForId)"
						}
					}
					.referringTo(enumeration.reference)
			override def fromJsonValueCode(valueCode: String): CodePiece = fromValueCode(Single(valueCode))
			override def fromValuesCode(valuesCode: String) =
				idType.fromValuesCode(valuesCode)
					.mapText { ids => s"$ids.flatMap($findForId)" }
					.referringTo(enumeration.reference)
			override def fromConcreteCode(concreteCode: String): CodePiece = s"Some($concreteCode)"
			
			override def writeDefaultDescription(className: Name, propName: Name)(implicit naming: NamingRules) =
				EnumValue.this.writeDefaultDescription(className, propName)
			
			private class EnumIdOptionSqlConversion(idConversion: SqlTypeConversion) extends SqlTypeConversion
			{
				override lazy val target = idConversion.target.copy(columnNameSuffix = colNameSuffix)
				
				override def origin = scalaType
				override def intermediate = Optional
				
				override def midConversion(originCode: String) = originCode
			}
		}
		
		private class EnumIdSqlConversion(idConversion: SqlTypeConversion) extends SqlTypeConversion
		{
			override lazy val target = idConversion.target.copy(columnNameSuffix = colNameSuffix)
			
			override def origin = scalaType
			override def intermediate = optional
			
			override def midConversion(originCode: String) = s"Some($originCode)"
		}
	}
	
	/**
	  * A data type which yields Vectors of a specific type
	  * @param innerType    The type of individual items in this vector
	  * @param columnLength The maximum database column length used when storing this vector. Default = 255.
	  */
	case class VectorType(innerType: PropertyType, columnLength: Int = 255) extends DirectlySqlConvertiblePropertyType
	{
		// ATTRIBUTES   --------------------------
		
		override val scalaType = ScalaType.basic("Vector")(innerType.scalaType)
		override val valueDataType = dataType / "VectorType"
		override val sqlType: SqlPropertyType = {
			// WET WET (from Text)
			val typeName = {
				if (columnLength > 16777215)
					"LONGTEXT"
				else if (columnLength > 65535)
					"MEDIUMTEXT"
				else
					s"VARCHAR($columnLength)"
			}
			SqlPropertyType(typeName, isNullable = true)
		}
		
		
		// IMPLEMENTED  ---------------------------
		
		override def defaultPropertyName = "values"
		override def defaultMutability: Option[Mutability] = None
		
		override def emptyValue = CodePiece("Vector.empty")
		override def nonEmptyDefaultValue = CodePiece.empty
		
		override def concrete = this
		
		override def isFilterGenerationSupported: Boolean = innerType.isFilterGenerationSupported
		override def yieldsTryFromValue = innerType.yieldsTryFromJsonValue
		override def yieldsTryFromJsonValue = innerType.yieldsTryFromJsonValue
		override def supportsDefaultJsonValues = true
		
		// Converts to json when storing to DB
		// Empty vectors are treated as empty values
		override def toValueCode(instanceCode: String) = {
			innerType.toJsonValueCode("v").mapText { itemToValue =>
				s"NotEmpty($instanceCode) match { case Some(v) => ((v.map[Value] { v => $itemToValue }: Value).toJson): Value; case None => Value.empty }"
			}.referringTo(Vector(valueConversions, notEmpty, value))
		}
		override def toJsonValueCode(instanceCode: String) =
			innerType.toJsonValueCode("v").mapText { itemToValue =>
				s"$instanceCode.map[Value] { v => $itemToValue }"
			}.referringTo(valueConversions)
		
		override def fromValueCode(valueCode: String, isFromJson: Boolean) = {
			// Case: Parsing from json => Converts directly to a vector
			if (isFromJson) {
				// Case: Item parsing may fail => Vector parsing may fail as well
				if (yieldsTryFromJsonValue)
					innerType.fromJsonValueCode("v").mapText { itemFromValue =>
						s"$valueCode.tryVectorWith { v => $itemFromValue }"
					}
				// Case: Item parsing always succeeds => Simply maps each item
				else
					innerType.fromJsonValueCode("v").mapText { itemFromValue =>
						s"$valueCode.getVector.map { v => $itemFromValue }"
					}
			}
			// Case: Parsing from a database model => Has to first parse the json into a vector
			else {
				// Case: Item parsing may fail => Vector and json parsing may fail as well
				if (yieldsTryFromValue)
					innerType.fromJsonValueCode("v").mapText { itemFromValue =>
						s"$valueCode.notEmpty match { case Some(v) => JsonBunny.munch(v.getString).flatMap { v => v.tryVectorWith { v => $itemFromValue } }; case None => Success(Vector.empty) }"
					}.referringTo(Vector(bunnyMunch.jsonBunny, success))
				// Case: Item parsing always succeeds => Simply maps the parsed vector
				else {
					innerType.fromJsonValueCode("v").mapText { itemFromValue =>
						s"$valueCode.notEmpty match { case Some(v) => JsonBunny.sureMunch(v.getString).getVector.map { v => $itemFromValue }; case None => Vector.empty }"
					}.referringTo(bunnyMunch.jsonBunny)
				}
			}
		}
		override def fromValuesCode(valuesCode: String) =
			innerType.fromJsonValueCode("v").mapText { itemFromValue =>
				// Case: Individual item parsing may fail => Ignores failures because has to succeed
				if (innerType.yieldsTryFromJsonValue)
					s"$valuesCode.flatMap { v => $itemFromValue.toOption }"
				// Case: Individual item parsing always succeeds => Simply maps the input vector
				else
					s"$valuesCode.map { v => $itemFromValue }"
			}
		override def fromConcreteCode(concreteCode: String): CodePiece = concreteCode
		
		override def writeDefaultDescription(className: Name, propName: Name)(implicit naming: NamingRules) = ""
	}
	
	abstract class PairLikeType(innerType: PropertyType, pairType: Reference, propertyNames: Pair[String],
	                            singleValueConstructorName: String)
		extends PropertyType
	{
		// ATTRIBUTES   ----------------------
		
		override lazy val sqlConversions: Vector[SqlTypeConversion] =
			propertyNames.flatMap { propName => innerType.sqlConversions.map { ElementConversion(propName, _) } }
				.toVector
		
		override lazy val nonEmptyDefaultValue: CodePiece =
			innerType.nonEmptyDefaultValue.mapIfNotEmpty {
				_.mapText { innerDefault =>
					s"${ pairType.target }.$singleValueConstructorName($innerDefault)"
				}.referringTo(pairType)
			}
		
		
		// ABSTRACT --------------------------
		
		protected def toPairCode(instanceCode: String): CodePiece
		protected def fromPairCode(pairCode: String): CodePiece
		
		
		// IMPLEMENTED  ----------------------
		
		override def valueDataType: Reference = dataType/"PairType"
		
		override def defaultPropertyName: Name = {
			val inner = innerType.defaultPropertyName
			Name(inner.plural, inner.plural, inner.style)
		}
		override def defaultPartNames = propertyNames.map { Name(_) }
		override def defaultMutability: Option[Mutability] = None
		
		// TODO: This type doesn't support option-wrapping at this time - Add when needed
		override def optional: PropertyType = this
		override def concrete: PropertyType = this
		
		override def isFilterGenerationSupported: Boolean = innerType.isFilterGenerationSupported
		override def supportsDefaultJsonValues: Boolean = true
		
		override def emptyValue: CodePiece = CodePiece.empty
		
		override def yieldsTryFromValue: Boolean = innerType.yieldsTryFromValue
		override def yieldsTryFromJsonValue: Boolean = innerType.yieldsTryFromJsonValue
		
		override def toValueCode(instanceCode: String): CodePiece = toValueCode(instanceCode, isForJson = false)
		override def toJsonValueCode(instanceCode: String): CodePiece = toValueCode(instanceCode, isForJson = true)
		
		override def fromValueCode(valueCodes: Seq[String]): CodePiece = {
			// Spits the input values into two parts
			val codesPerPart = valueCodes.size / 2
			val splitValueCodes = Pair(valueCodes.take(codesPerPart), valueCodes.drop(codesPerPart))
			// Forms each part from the available values
			splitValueCodes.mapAndMerge(innerType.fromValueCode) { (firstPartCode, secondPartCode) =>
				val codeText = {
					// Case: Parts are provided as instances of Try => uses flatMap
					if (innerType.yieldsTryFromValue)
						s"$firstPartCode.flatMap { v1 => $secondPartCode.map { v2 => ${pairType.target}(v1, v2) } }"
					// Case: Parts are available directly => Wraps them
					else
						s"${pairType.target}($firstPartCode, $secondPartCode)"
				}
				CodePiece(codeText, firstPartCode.references ++ secondPartCode.references + pairType)
			}
		}
		override def fromJsonValueCode(valueCode: String): CodePiece = {
			// Converts the value into a pair of values and
			// converts the values into correct types
			val pairCode = innerType.fromJsonValueCode("v").mapText { innerFromValue =>
				if (innerType.yieldsTryFromJsonValue)
					s"$valueCode.tryPairWith { v => $innerFromValue }"
				else
					s"$valueCode.getPair.map { v => $innerFromValue }"
			}
			// Converts the pair into the correct type
			fromPairCode(pairCode.text).referringTo(pairCode.references)
		}
		override def fromValuesCode(valuesCode: String): CodePiece = fromJsonValueCode("v").mapText { fromValue =>
			if (yieldsTryFromJsonValue)
				s"$valuesCode.flatMap { v => $fromValue.toOption }"
			else
				s"$valuesCode.map { v => $fromValue }"
		}
		override def fromConcreteCode(concreteCode: String): CodePiece = concreteCode
		
		override def writeDefaultDescription(className: Name, propName: Name)(implicit naming: NamingRules): String = ""
		
		
		// OTHER    ----------------------------
		
		private def innerToValue(instanceCode: String, isForJson: Boolean) =
			if (isForJson) innerType.toJsonValueCode(instanceCode) else innerType.toValueCode(instanceCode)
		
		private def toValueCode(instanceCode: String, isForJson: Boolean) = {
			val toPair = toPairCode(instanceCode)
			innerToValue("v", isForJson).mapText { innerToValue =>
				s"$toPair.map[Value] { v => $innerToValue }"
			}.referringTo(toPair.references + valueConversions + Reference.flow.value)
		}
		
		
		// NESTED   ----------------------------
		
		// Converts a single element (e.g. the first element of a pair)
		// For wrapped multi-column types, converts a single element of a single element
		// (e.g. the first part of the first element of a pair)
		private case class ElementConversion(propertyName: String, innerConversion: SqlTypeConversion)
			extends SqlTypeConversion
		{
			override def origin: ScalaType = pairType
			override def intermediate: ValueConvertibleType = innerConversion.intermediate
			override def target: SqlPropertyType = innerConversion.target
			
			override def midConversion(originCode: String): CodePiece =
				innerConversion.midConversion(s"$originCode.$propertyName").referringTo(pairType)
		}
	}
	case class Paired(innerType: PropertyType) extends PairLikeType(innerType, pair,
		Pair("first", "second"), "twice")
	{
		override lazy val scalaType: ScalaType = pair(innerType.scalaType)
		
		override protected def toPairCode(instanceCode: String): CodePiece = instanceCode
		override protected def fromPairCode(pairCode: String): CodePiece = pairCode
	}
	case class Spanning(innerType: PropertyType, isNumeric: Boolean = false)
		extends PairLikeType(innerType, if (isNumeric) numericSpan else span,
			Pair("start", "end"), "singleValue")
	{
		private val reference = if (isNumeric) numericSpan else span
		override lazy val scalaType: ScalaType = reference(innerType.scalaType)
		
		override protected def toPairCode(instanceCode: String): CodePiece = s"$instanceCode.ends"
		override protected def fromPairCode(pairCode: String): CodePiece = {
			CodePiece(s"${reference.target}($pairCode)", Set(reference))
		}
	}
	case object DateRange extends PairLikeType(Date, dateRange, Pair("start", "end"), "single")
	{
		override def scalaType: ScalaType = dateRange
		
		override protected def toPairCode(instanceCode: String): CodePiece = s"$instanceCode.ends"
		override protected def fromPairCode(pairCode: String): CodePiece =
			CodePiece(s"${dateRange.target}.exclusive($pairCode)", Set(dateRange))
	}
	
	case object AngleType extends FacadePropertyType
	{
		// ATTRIBUTES   ------------------------
		
		override lazy val sqlConversions = super.sqlConversions.map { _.modifyTarget(columnNameSuffix = "degrees") }
		
		
		// IMPLEMENTED  ------------------------
		
		override protected def delegate: PropertyType = DoubleNumber
		
		override def scalaType: ScalaType = paradigm.angle
		
		override def defaultMutability: Option[Mutability] = None
		
		override def emptyValue: CodePiece = CodePiece.empty
		override def nonEmptyDefaultValue: CodePiece = CodePiece.empty
		override def defaultPropertyName: Name = "direction"
		override def defaultPartNames: Seq[Name] = Seq.empty
		
		override def supportsDefaultJsonValues: Boolean = true
		override protected def yieldsTryFromDelegate: Boolean = false
		
		override protected def toDelegateCode(instanceCode: String): CodePiece = s"$instanceCode.degrees"
		
		override protected def fromDelegateCode(delegateCode: String): CodePiece =
			CodePiece(s"Angle.degrees($delegateCode)", Set(paradigm.angle))
		
		override def writeDefaultDescription(className: Name, propName: Name)(implicit naming: NamingRules): String = ""
	}
	
	case object DoubleVector2D extends PairLikeType(DoubleNumber, paradigm.vector2D, Pair("x", "y"), "twice")
	{
		override def scalaType: ScalaType = paradigm.vector2D
		
		override def valueDataType = paradigm.dataType/"Vector2DType"
		override def defaultPropertyName = "vector"
		
		override protected def toPairCode(instanceCode: String): CodePiece = s"$instanceCode.xyPair"
		override protected def fromPairCode(pairCode: String): CodePiece = s"Vector2D($pairCode)"
		
		override def toJsonValueCode(instanceCode: String) = s"$instanceCode.toValue"
		override def fromJsonValueCode(valueCode: String) =
			CodePiece(s"$valueCode.getVector2D", Set(paradigm.paradigmValue))
	}
	
	object LinearVelocity
	{
		lazy val perSecond = LinearVelocity(TimeUnit.SECONDS)
	}
	case class LinearVelocity(unit: TimeUnit) extends FacadePropertyType
	{
		// ATTRIBUTES   ----------------------
		
		override lazy val sqlConversions =
			super.sqlConversions.map { _.withAdditionalColumnNameSuffix(s"per_${ timeUnitToString(unit) }") }
		
		
		// IMPLEMENTED  ---------------------
		
		override protected def delegate: PropertyType = DoubleNumber
		override def scalaType: ScalaType = paradigm.linearVelocity
		
		override def emptyValue: CodePiece = CodePiece.empty
		override def nonEmptyDefaultValue: CodePiece = CodePiece("LinearVelocity.zero", Set(paradigm.linearVelocity))
		override def defaultPropertyName: Name = Name("velocity", "velocities", CamelCase.lower)
		override def defaultPartNames: Seq[Name] = Empty
		override def defaultMutability: Option[Mutability] = None
		
		override protected def yieldsTryFromDelegate: Boolean = false
		override def supportsDefaultJsonValues: Boolean = true
		
		override protected def toDelegateCode(instanceCode: String): CodePiece =
			CodePiece(s"$instanceCode.over(Duration(1, TimeUnit.${ unit.toString }))", Set(duration, timeUnit))
		
		override protected def fromDelegateCode(delegateCode: String): CodePiece =
			CodePiece(s"LinearVelocity($delegateCode)(TimeUnit.${ unit.toString })",
				Set(paradigm.linearVelocity, timeUnit))
		
		override def writeDefaultDescription(className: Name, propName: Name)(implicit naming: NamingRules): String = ""
	}
	
	object LengthDistance
	{
		def meters: LengthDistance = apply("Meter", "m")
		def kilometers = apply("KiloMeter", "km")
		
		def apply(rawUnit: String): LengthDistance = rawUnit.toLowerCase match {
			case "m" | "meter" => apply("Meter", "m")
			case "mm" | "millimeter" | "millis" | "milli" => apply("MilliMeter", "mm")
			case "cm" | "centimeter" => apply("CentiMeter", "cm")
			case "km" | "kilo" | "kilometer" => apply("KiloMeter", "km")
			case "inches" | "inch" | "in" => apply("Inch", "in")
			case "ft" | "feet" => apply("Feet", "ft")
			case "mile" | "mi" | "miles" => apply("Mile", "mi")
			case "nm" | "nautical" | "nauticalmile" | "nauticalmiles" => apply("NauticalMile", "nm")
			case _ =>
				println(s"WARNING: Specified unit \"$rawUnit\" doesn't match any registered distance unit. Defaults to meters")
				meters
		}
	}
	case class LengthDistance(unitName: String, abbreviation: String) extends FacadePropertyType
	{
		// ATTRIBUTES   --------------------------
		
		override lazy val sqlConversions = super.sqlConversions.map { _.modifyTarget(columnNameSuffix = abbreviation) }
		
		
		// IMPLEMENTED  --------------------------
		
		override protected def delegate: PropertyType = DoubleNumber
		override def scalaType: ScalaType = paradigm.distance
		
		override def emptyValue: CodePiece = CodePiece.empty
		override def nonEmptyDefaultValue: CodePiece = CodePiece.empty
		
		override def defaultPropertyName: Name = "distance"
		override def defaultPartNames: Seq[Name] = Empty
		override def defaultMutability: Option[Mutability] = None
		
		override def supportsDefaultJsonValues: Boolean = true
		override protected def yieldsTryFromDelegate: Boolean = false
		
		override protected def toDelegateCode(instanceCode: String): CodePiece =
			CodePiece(s"$instanceCode.toUnit(DistanceUnit.$unitName)", Set(paradigm.distanceUnit))
		override protected def fromDelegateCode(delegateCode: String): CodePiece =
			CodePiece(s"Distance($delegateCode, DistanceUnit.$unitName)", Set(paradigm.distance, paradigm.distanceUnit))
		
		override def writeDefaultDescription(className: Name, propName: Name)(implicit naming: NamingRules): String = ""
	}
	
	case object LatitudeLongitudePair extends FacadePropertyType
	{
		override protected val delegate: PropertyType = Paired(DoubleNumber)
		
		override def scalaType: ScalaType = terra.latLong
		
		override def emptyValue: CodePiece = CodePiece.empty
		override def nonEmptyDefaultValue: CodePiece = CodePiece.empty
		
		override def defaultPropertyName: Name = "latLong"
		override def defaultPartNames: Seq[Name] = Vector("latitude", "longitude")
		override def defaultMutability: Option[Mutability] = None
		
		override def supportsDefaultJsonValues: Boolean = true
		override protected def yieldsTryFromDelegate: Boolean = false
		
		override protected def toDelegateCode(instanceCode: String): CodePiece = s"$instanceCode.latLongDegrees"
		override protected def fromDelegateCode(delegateCode: String): CodePiece =
			CodePiece(s"LatLong.degrees($delegateCode)", Set(terra.latLong))
		
		override def writeDefaultDescription(className: Name, propName: Name)(implicit naming: NamingRules): String = ""
	}
}
