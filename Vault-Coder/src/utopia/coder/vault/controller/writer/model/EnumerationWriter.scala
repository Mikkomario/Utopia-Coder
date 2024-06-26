package utopia.coder.vault.controller.writer.model

import utopia.coder.model.data
import utopia.coder.model.data.NamingRules
import utopia.coder.model.enumeration.NamingConvention.CamelCase
import utopia.coder.model.scala.code.CodePiece
import utopia.coder.model.scala.datatype.Reference.Flow._
import utopia.coder.model.scala.datatype.{Extension, Reference, ScalaType}
import utopia.coder.model.scala.declaration.PropertyDeclarationType.{ComputedProperty, ImmutableValue}
import utopia.coder.model.scala.declaration.{File, MethodDeclaration, ObjectDeclaration, PropertyDeclaration, TraitDeclaration}
import utopia.coder.model.scala.{DeclarationDate, Parameter}
import Reference._
import utopia.flow.util.StringExtensions._
import utopia.coder.vault.model.data.{Enum, VaultProjectSetup}
import utopia.coder.vault.model.datatype.StandardPropertyType.Text
import utopia.flow.collection.immutable.Single

import scala.io.Codec

/**
  * Used for writing enumeration files
  * @author Mikko Hilpinen
  * @since 25.9.2021, v1.1
  */
object EnumerationWriter
{
	// ATTRIBUTES   ------------------------
	
	lazy val findPrefix = data.Name("find", CamelCase.lower)
	lazy val forPrefix = data.Name("for", CamelCase.lower)
	
	
	// OTHER    ----------------------------
	
	/**
	  * @param e Enumeration
	  * @param naming Implicit naming rules
	  * @return Name of the 'findForId' -function
	  */
	def findForIdName(e: Enum)(implicit naming: NamingRules) = (findPrefix + forPrefix + e.idPropName).function
	/**
	  * @param e Enumeration
	  * @param naming Implicit naming rules
	  * @return Name of the 'forId' -function
	  */
	def forIdName(e: Enum)(implicit naming: NamingRules) = (forPrefix + e.idPropName).function
	
	/**
	  * Writes an enumeration as a scala file
	  * @param e  Enumeration to write
	  * @param setup Project setup to use (implicit)
	  * @param codec Codec to use (implicit)
	  * @return Enum reference on success. Failure if writing failed.
	  */
	def apply(e: Enum)(implicit setup: VaultProjectSetup, codec: Codec, naming: NamingRules) = {
		val enumName = e.name.enumName
		val idPropName = e.idPropName.prop
		val _findForIdName = findForIdName(e)
		val _forIdName = forIdName(e)
		// Enumeration doesn't need to be imported in its own file
		val enumDataType = ScalaType.basic(enumName)
		
		// Uses Pair in .values property that contains only two values
		val values = {
			val (typeToImport, valuesType, collectionConstructor) = {
				if (e.values.size == 2)
					(Some(pair), pair(enumDataType), "Pair")
				else
					(None, ScalaType.vector(enumDataType), "Vector")
			}
			ImmutableValue("values", typeToImport.toSet,
				explicitOutputType = Some(valuesType),
				description = s"All available ${e.name} values"
			)(s"$collectionConstructor(${ e.values.map { v => v.name.enumValue }.mkString(", ") })")
		}
		
		val defaultProp = e.defaultValue.map { v =>
			ComputedProperty("default", description = s"The default ${ e.name.doc } (i.e. ${ v.name.doc })")(
				v.name.enumValue)
		}
		//noinspection LegacyStringFormatting
		// forId implementation differs when the enumeration has a default value
		val (forIdEndCode, forIdDescriptionPostfix) = e.defaultValue match {
			case Some(default) => CodePiece(".getOrElse(default)") -> s", or the default ${e.name} (${default.name})"
			case None =>
				CodePiece(s".toTry { new NoSuchElementException(s${
					s"No value of ${ e.name.enumName } matches ${ e.idPropName.prop } '${ "$" + idPropName }'".quoted
				}) }", Set(collectionExtensions, Reference.noSuchElementException)) ->
					". Failure if no matching value was found."
		}
		// NB: Current implementation doesn't really work for multi-column id types
		val findForValueCode = e.idType.fromValueCode(Vector("idVal")).flatMapText { id =>
			val yieldsTry = e.idType.yieldsTryFromValue
			val parseIdVal = {
				if (yieldsTry)
					s"$id.toOption.flatMap(${_findForIdName})"
				else
					s"${_findForIdName}($id)"
			}
			// Matches against literal enumeration values as a backup, but not if the keys are of type String
			if (e.idType.isInstanceOf[Text])
				CodePiece(parseIdVal)
			else {
				val idValueType = e.idType.valueDataType
				CodePiece(s"value.castTo(${
					idValueType.target}, StringType) match { case Left(idVal) => $parseIdVal; case Right(stringVal) => val str = stringVal.getString; values.find { _.toString ~== str } }",
					Set(flow.equalsExtensions, idValueType, stringType))
			}
		}
		val fromValueCode = forIdEndCode.mapText { end =>
			s"findForValue(value)${ end.replace("$" + idPropName, "$value") }"
		}
		val valueParam = Parameter("value", value,
			description = s"A value representing an ${ e.name.doc } ${ e.idPropName.doc }")
		val enumValueToValueCode = e.idType.toValueCode(idPropName)
		
		File(e.packagePath,
			// Writes the enumeration trait first
			TraitDeclaration(enumName,
				extensions = Single(valueConvertible),
				// Each value contains an id so that it can be referred from the database
				properties = Vector(
					PropertyDeclaration.newAbstract(idPropName, e.idType.toScala,
						description = s"${e.idPropName} used to represent this ${e.name} in database and json"),
					ComputedProperty("toValue", enumValueToValueCode.references, isOverridden = true)(
						enumValueToValueCode.text)
				),
				description = e.description.nonEmptyOrElse(s"Common trait for all ${ e.name.doc } values"),
				author = e.author, since = DeclarationDate.versionedToday, isSealed = true
			),
			// Enumeration values are nested within a companion object
			ObjectDeclaration(enumName,
				// Contains the .values -property
				properties = Single(values) ++ defaultProp,
				// Contains an id to enum value -function (one with Try, another with Option)
				methods = Set(
					MethodDeclaration(_findForIdName,
						returnDescription = s"${ e.name.doc } matching the specified ${ e.idPropName.doc }. None if the ${
							e.idPropName.doc } didn't match any ${ e.name.doc }", isLowMergePriority = true)(
						Parameter(idPropName, e.idType.toScala, description = s"${ e.idPropName.doc } representing a ${ e.name.doc }"))(
						s"values.find { _.$idPropName == $idPropName }"),
					MethodDeclaration(_forIdName,
						codeReferences = forIdEndCode.references,
						returnDescription = s"${ e.name.doc } matching that ${ e.idPropName.doc }$forIdDescriptionPostfix")(
						Parameter(idPropName, e.idType.toScala, description = s"${ e.idPropName.doc } matching a ${ e.name.doc }"))(
						s"${_findForIdName}($idPropName)${forIdEndCode.text}"),
					MethodDeclaration("findForValue", findForValueCode.references,
						returnDescription = s"${ e.name.doc } matching the specified value. None if the value didn't match any ${
							e.name.doc }", isLowMergePriority = true)(valueParam)(findForValueCode.text),
					MethodDeclaration("fromValue", fromValueCode.references,
						returnDescription = s"${ e.name.doc } matching the specified value, when the value is interpreted as an ${
							e.name.doc } ${ e.idPropName.doc }$forIdDescriptionPostfix")(valueParam)(fromValueCode.text)
				),
				// Contains an object for each value
				nested = e.values.map { value =>
					ObjectDeclaration(value.name.enumValue, Single(Extension(enumDataType)),
						// The objects don't contain other properties except for 'id'
						properties = Vector(
							ImmutableValue(idPropName, value.id.references, isOverridden = true)(value.id.text)),
						description = value.description, isCaseObject = true
					)
				}.toSet
			)
		).write()
	}
}
