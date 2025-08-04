package utopia.coder.vault.controller.writer.model

import utopia.coder.model.data.{Name, NamingRules}
import utopia.coder.model.scala.code.{CodeBuilder, CodePiece}
import utopia.coder.model.scala.datatype.Reference
import utopia.coder.model.scala.datatype.Reference._
import utopia.coder.vault.model.data
import utopia.flow.collection.CollectionExtensions._
import utopia.flow.util.StringExtensions._

/**
 * Provides functions for writing various from model -conversion functions
 *
 * @author Mikko Hilpinen
 * @since 11.07.2025, v1.13
 */
object ParseModelCode
{
	/**
	 * Creates a parse-from-model function implementation code
	 * @param c Class for which this method is written
	 * @param dataRef Reference (code) to the data class (constructor)
	 * @param storedRef Reference to the stored model version.
	 *                  None (default) if only the data class version should be constructed.
	 * @param modelName Name of the (flow) Model property. Default = model.
	 * @param propNames Name / code for the database properties -accessor. Only used when writing DB model parsing code.
	 *                  Default = this.model
	 * @param openParentBlock A code block -opener that starts this code.
	 *                        E.g. "validate(model).flatMap { model => ".
	 *                        Default = empty.
	 * @param json Whether writing a JSON parsing -function. Default = false.
	 * @param yieldTry Whether this function should yield an instance of Try.
	 *                 If false (default), this function is *not allowed* to yield a Try.
	 * @param naming Implicit naming rules to apply
	 * @return A code for the model-parsing method implementation
	 */
	def apply(c: data.Class, dataRef: CodePiece, storedRef: Option[Reference] = None,
	          modelName: String = "model", propNames: String = "this.model",
	          openParentBlock: CodePiece = CodePiece.empty, json: Boolean = false, yieldTry: Boolean = false)
	         (implicit naming: NamingRules) =
	{
		// A function for acquiring the property name, which is used when pulling data from the model
		val propName = {
			if (json)
				{ prop: Name => prop.jsonProp.quoted }
			else
				{ prop: Name => s"$propNames.${ prop.prop }.name" }
		}
		// Code for acquiring and parsing the value of each property, except the ID property
		val getProps = c.properties.map { prop =>
			val getValue = {
				// Case: Parsing JSON => Acquires & parses the value
				if (json)
					prop.dataType.fromJsonValueCode(s"$modelName(${ propName(prop.name) })")
				else
					prop.parents.headOption match {
						// Case: Accessing a value from the parent
						//       => Expects the value to be predefined as a method parameter,
						//          although possibly with a different name
						case Some(parent) => CodePiece(parent.name.prop)
						// Case: Parsing a DB model => May parse and combine multiple DB properties
						case None =>
							prop.dataType.fromValueCode(prop.dbProperties.map { prop =>
								s"$modelName(${ propName(prop.name) })"
							})
					}
			}
			prop -> getValue
		}
		// Determines which properties yield Try and must be combined with map or flatMap
		// Also, prepares the final assignments
		val (tryDefinitions, finalAssignments) = getProps.splitFlatMap { case (prop, getValue) =>
			val propYieldsTry = prop.dataType.yieldsTryFromValueIn(json)
			val (tryDef, assignCode) = {
				// Case: map or flatMap is appropriate => In the assignment, refers to the predefined value
				if (yieldTry && propYieldsTry) {
					val propName = prop.name.prop
					Some(propName -> getValue) -> CodePiece(propName)
				}
				// Case: map is not appropriate or available => Parses the value upon the final assignment
				else {
					val appliedGetValue = {
						// Case: Parsing still yields a Try
						//       => must unwrap it and throw if needed (because this method is not allowed to return a Try)
						if (propYieldsTry)
							getValue.mapText { _ + ".get" }
						else
							getValue
					}
					None -> appliedGetValue
				}
			}
			// Names the property assignments
			tryDef -> Some(assignCode.mapText { assign => s"${ prop.name.prop } = $assign" })
		}
		
		// Opens all the necessary blocks of code,
		// consisting of the custom-defined line of code (if specified), and Try property definitions.
		val builder = new CodeBuilder()
		openParentBlock.notEmpty.foreach(builder.openBlockWith)
		if (tryDefinitions.nonEmpty) {
			// Uses map for the last Try definition, and flatMap for the others
			val definitionsWithMethods = tryDefinitions.dropRight(1).map { _ -> "flatMap" } :+
				(tryDefinitions.last -> "map")
			definitionsWithMethods.foreach { case ((propName, getValue), mapCall) =>
				builder.openBlockWith(getValue.mapText { getValue => s"$getValue.$mapCall { $propName => " })
			}
		}
		
		// Prepares the parameters for the actual constructor
		val assignValues = finalAssignments.reduceOption { _.append(_, ", ") }.getOrElse(CodePiece.empty)
		// Prepares the actual constructor
		val constructResult = {
			// Case: A generic trait => Utilizes the generic apply(...) function
			if (!json && c.isGeneric)
				getIdFor(c, modelName, propNames, json).flatMapText { id =>
					assignValues.mapText { assign => s"apply($modelName, $id, $assign)" }
				}
			// Case: A concrete class => Either constructs a stored instance, or the data instance
			else {
				val constructData = assignValues.flatMapText { assign => dataRef.mapText { data => s"$data($assign)" } }
				// Prepares code for constructing the final instance (either XData or X (i.e. the stored version))
				storedRef match {
					// Case: Constructing the stored version
					case Some(storedRef) =>
						// Writes the X(id, XData(...)) -code
						storedRef.targetCode.flatMapText { stored =>
							getIdFor(c, modelName, propNames, json).flatMapText { id =>
								constructData.mapText { data => s"$stored($id, $data)" }
							}
						}
					
					// Case: Just constructing the XData instance
					case None => constructData
				}
			}
		}
		
		// Case: Must return a Try, but no Try block has been opened => Wraps the result in a Success(...)
		if (yieldTry && openParentBlock.isEmpty && tryDefinitions.isEmpty)
			builder += constructResult.flatMapText { result => CodePiece(s"Success($result)", Set(success)) }
		// Case: No special wrapping required
		else
			builder += constructResult
			
		// Returns the code
		builder.closeOpenBlocks().result()
	}
	
	// Determines how the ID property is accessed and parsed
	private def getIdFor(c: data.Class, modelName: String, propNames: String, json: Boolean) = {
		// Case: id already parsed and defined by the parent class => Simply refers to it
		if (c.isExtension)
			CodePiece("id")
		// Case: ID must be parsed
		else {
			val propName = {
				if (json)
					"\"id\""
				else
					s"$propNames.id.name"
			}
			c.idType.fromValueCode(s"$modelName($propName)", json)
		}
	}
}
