package utopia.coder.vault.util

import utopia.coder.model.data.NamingRules
import utopia.flow.util.StringExtensions._
import utopia.coder.vault.model.data.{Class, Property}
import utopia.coder.model.scala.Visibility.{Protected, Public}
import utopia.coder.model.scala.Parameter
import utopia.coder.model.scala.code.{Code, CodeBuilder, CodePiece}
import utopia.coder.model.scala.datatype.Reference
import utopia.coder.model.scala.declaration.MethodDeclaration
import Reference.Flow._
import utopia.flow.collection.immutable.Empty

/**
  * Used for constructing class-specific methods
  * @author Mikko Hilpinen
  * @since 9.10.2021, v1.2
  */
object ClassMethodFactory
{
	/**
	  * Creates a new method that parses an instance from a model
	  * @param targetClass Class being parsed
	  * @param validatedModelCode Code that provides a Try containing a validated model based on the input parameter
	  * @param methodName Name of this method
	  * @param param (Model) parameter accepted by this method (default = template model named 'model')
	  * @param isFromJson Whether the input model is from json (true) or from a database model (false).
	  *                   Default = database model.
	  * @param wrapAssignments A function that accepts assignments that provide enough data for a data instance
	  *                        creation (e.g. "param1Value, param2Value, param3Value") and produces the code
	  *                        resulting in the desired output type (like stored model, for example)
	  * @return A method for parsing class data from models
	  */
	def classFromModel(targetClass: Class, validatedModelCode: CodePiece,
	                   methodName: String = "apply",
	                   param: Parameter = Parameter("model", templateModel(property)),
	                   isFromJson: Boolean = false)
	                  (wrapAssignments: CodePiece => CodePiece)
	                  (implicit naming: NamingRules) =
		MethodDeclaration.usingCode(methodName,
			classFromModelCode(targetClass, validatedModelCode, isFromJson, modelIsTry = true)(wrapAssignments),
			isOverridden = true)(param)
	
	/**
	  * Creates a new method that parses instances from a validated model.
	 * NB: Doesn't support class inheritance.
	  * @param targetClass Class being parsed
	  * @param methodName Name of the method (default = fromValidatedModel)
	  * @param isFromJson Whether the input model is from json (true) or from a database model (false).
	  *                   Default = database model.
	  * @param wrapAssignments A function that accepts assignments that provide enough data for a data instance
	  *                        creation (e.g. "param1Value, param2Value, param3Value") and produces the code
	  *                        resulting in the desired output type (like stored model, for example)
	  * @param naming Naming rules to apply
	  * @return A method declaration
	  */
	def classFromValidatedModel(targetClass: Class, methodName: String = "fromValidatedModel",
	                            isFromJson: Boolean = false)
	                           (wrapAssignments: CodePiece => CodePiece)
	                           (implicit naming: NamingRules) =
	{
		val param: Parameter = Parameter("valid", model)
		// Case: Class contains no properties
		if (targetClass.properties.isEmpty) {
			val code = wrapAssignments(CodePiece.empty)
			MethodDeclaration(methodName, code.references, visibility = Protected, isOverridden = true)(param)(code.text)
		}
		// Case: No try-based or potentially failing reads are used => implements a simpler fromValidatedModel
		else {
			val modelName = param.name
			lazy val dbPropsAccessor = dbPropsAccessorFor(targetClass)
			val dataCreation = targetClass.properties
				.map { prop => propFromValidModelCode(prop, modelName, dbPropsAccessor, isFromJson) }
				.reduceLeft { _.append(_, ", ") }
			val code = wrapAssignments(dataCreation)
			MethodDeclaration(methodName, code.references, visibility = Protected, isOverridden = true)(param)(code.text)
		}
	}
	
	/**
	 * Creates a new method code that parses an instance from a model
	 * @param targetClass Class being parsed
	 * @param validatedModelCode Code that provides a validated model based on the input parameter
	 * @param isFromJson Whether the input model is from json (true) or from a database model (false).
	 *                   Default = database model.
	 * @param modelIsTry Whether 'validatedModelCode' yields a Try (default = false)
	 * @param wrapAssignments A function that accepts assignments that provide enough data for a data instance
	 *                        creation (e.g. "param1Value, param2Value, param3Value") and produces the code
	 *                        resulting in the desired output type (like stored model, for example)
	 * @return A method for parsing class data from models
	 */
	def classFromModelCode(targetClass: Class, validatedModelCode: CodePiece, isFromJson: Boolean = false,
	                       modelIsTry: Boolean = false)
	                      (wrapAssignments: CodePiece => CodePiece)
	                      (implicit naming: NamingRules): Code =
	{
		// Case: Class contains no properties
		if (targetClass.properties.isEmpty)
			wrapAssignments(CodePiece.empty)
		else
			tryApplyCode(targetClass, validatedModelCode, isFromJson, modelIsTry)(wrapAssignments)
	}
	
	private def tryApplyCode(classToWrite: Class, validatedModelCode: CodePiece,
	                         isFromJson: Boolean, modelIsTry: Boolean)
	                        (wrapAssignments: CodePiece => CodePiece)
	                        (implicit naming: NamingRules) =
	{
		// Divides the class properties into try-based values and standard values
		val tryProperties = classToWrite.properties.filter { _.dataType.yieldsTryFromValue }
		
		val builder = new CodeBuilder()
		
		// Unwraps the validated model (optional feature)
		val initialIndentCount = {
			if (modelIsTry) {
				val validateMapMethod = if (tryProperties.isEmpty) ".map" else ".flatMap"
				builder += validatedModelCode + validateMapMethod + " { valid => "
				builder.indent()
				1
			}
			else
				0
		}
		
		lazy val dbPropsAccessor = dbPropsAccessorFor(classToWrite)
		declareTryProps(builder, tryProperties.dropRight(1), "flatMap", dbPropsAccessor, isFromJson)
		declareTryProps(builder, tryProperties.lastOption, "map", dbPropsAccessor, isFromJson)
		val innerIndentCount = initialIndentCount + tryProperties.size
		
		// Writes the instance creation now that the "try-properties" properties have been declared
		// Imports the db properties, if needed
		val assignments = classToWrite.properties.map { prop =>
			// Case: Try-based property / value or an inherited and parsed db property => already defined
			if (prop.dataType.yieldsTryFromValue || (!isFromJson && prop.isExtension))
				CodePiece(prop.originalName.prop)
			// Case: Normal property / value => reads the value from the model
			else
				propFromValidModelCode(prop, validatedModelCode.text, dbPropAccessor = dbPropsAccessor,
					isFromJson = isFromJson)
		}.reduceLeft { _.append(_, ", ") }
		builder += wrapAssignments(assignments)
		
		// Closes open blocks
		(0 until innerIndentCount).foreach { _ => builder.closeBlock() }
		
		// References the enumerations used
		builder.result()
	}
	
	// NB: Indents for each declared property
	private def declareTryProps(builder: CodeBuilder, tryProps: Iterable[Property], mapMethod: String,
	                            dbPropAccessor: => String, isFromJson: Boolean)
	                           (implicit naming: NamingRules) =
		tryProps.foreach { prop =>
			val fromValueCode = propFromValidModelCode(prop, dbPropAccessor = dbPropAccessor, isFromJson = isFromJson)
			builder += fromValueCode + s".$mapMethod { ${prop.name.prop} => "
			builder.indent()
		}
	
	private def propFromValidModelCode(prop: Property, modelName: String = "valid", dbPropAccessor: => String,
	                                   isFromJson: Boolean)
	                                  (implicit naming: NamingRules) =
	{
		// Uses different property names based on whether parsing from json or from a database model
		if (isFromJson)
			prop.dataType.fromJsonValueCode(s"$modelName(${prop.jsonPropName.quoted})")
		else {
			val dbProps = dbPropAccessor
			prop.dataType.fromValueCode(prop.dbProperties.map { prop => s"$modelName($dbProps.${prop.name.prop}.name)" })
		}
	}
	
	// Not the most elegant implementation, but for now it works (utilized in XDbFactory)
	private def dbPropsAccessorFor(classToWrite: Class) =
		if (classToWrite.isGeneric || classToWrite.isExtension) "dbProps" else "this.model"
}
