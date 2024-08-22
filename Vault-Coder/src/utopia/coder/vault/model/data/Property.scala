package utopia.coder.vault.model.data

import utopia.coder.model.data.{Name, Named, NamingRules}
import utopia.flow.collection.CollectionExtensions._
import utopia.flow.operator.equality.EqualsExtensions._
import utopia.coder.vault.model.datatype.StandardPropertyType.ClassReference
import utopia.coder.vault.model.datatype.{PropertyType, SingleColumnPropertyType}
import utopia.coder.model.scala.code.CodePiece
import utopia.coder.vault.model.enumeration.Mutability
import utopia.flow.collection.immutable.{Empty, Single}
import utopia.flow.collection.mutable.iterator.OptionsIterator

object Property
{
	/**
	  * Creates a new property with automatic naming
	  * @param name Name of this property
	  * @param dataType Type of this property
	  * @param customDefaultValue Default value passed for this property, as code (empty if no default (default))
	  * @param customMutability User-defined mutability of this property. Default = None = not defined (i.e. use defaults)
	 * @param description Description of this property (Default = empty)
	  * @return A new property
	  */
	def singleColumn(name: Name, dataType: SingleColumnPropertyType, customDefaultValue: CodePiece = CodePiece.empty,
	                 dbPropertyOverrides: DbPropertyOverrides = DbPropertyOverrides.empty,
	                 customMutability: Option[Mutability] = None, withAccessName: String = "",
	                 inAccessName: String = "", description: String = ""): Property =
		apply(name, dataType, customDefaultValue, Vector(dbPropertyOverrides), customMutability, Empty, withAccessName,
			inAccessName, description)
}

/**
  * Classes define certain typed properties that are present in every instance
  * @author Mikko Hilpinen
  * @since 30.8.2021, v0.1
  * @param name Name of this property
  * @param dataType Type of this property
  * @param customDefaultValue Default value (code) that overrides the datatype-specified default.
  *                           Empty if no override should be made. Default = empty.
  * @param dbPropertyOverrides User-defined overrides applied to the database-properties matching this class-property.
  *                            Default = empty = no overrides.
  * @param customMutability User-defined mutability of this property. Default = None = not defined (i.e. use defaults)
 * @param withAccessName (Custom) name of the access (filter) method that targets an individual value
  * @param inAccessName (Custom) name of the access (filter) method that targets multiple values using an "in" query
  * @param description Description of this property (may be empty). Default = empty = no description.
  */
case class Property(name: Name, dataType: PropertyType, customDefaultValue: CodePiece = CodePiece.empty,
                    dbPropertyOverrides: Seq[DbPropertyOverrides] = Empty,
                    customMutability: Option[Mutability] = None, parents: Seq[Property] = Empty,
                    withAccessName: String = "", inAccessName: String = "", description: String = "")
	extends Named
{
	// ATTRIBUTES   ------------------------
	
	/**
	  * Database-matching properties associated with this class-property
	  */
	lazy val dbProperties =  {
		val conversions = dataType.sqlConversions
		val conversionCount = conversions.size
		// Case: Single-column property => generates a single dbProperty
		if (conversionCount == 1)
			Vector(DbProperty(name, conversions.head,
				dbPropertyOverrides.headOption.getOrElse(DbPropertyOverrides.empty)))
		// Case: Multi-column property => generates multiple dbProperties with distinct names
		else {
			lazy val defaultNames = dataType.defaultPartNames
			conversions.indices.map { i =>
				// Makes sure custom names are defined
				// If not, uses default provided by the datatype
				// If not even those are available, modifies the common property name by adding a suffix
				lazy val defaultName = defaultNames.lift(i).getOrElse { name + (i + 1).toString }
				val overrides = dbPropertyOverrides.getOption(i) match {
					// Case: Custom overrides exist => makes sure they include a custom name
					case Some(o) => if (o.name.isDefined) o else o.copy(name = Some(defaultName))
					// Case: No custom override exists => creates one that overrides the default property name
					case None => DbPropertyOverrides(Some(defaultName))
				}
				DbProperty(name, conversions(i), overrides)
			}.toVector
		}
	}
	
	
	// COMPUTED ----------------------------
	
	/**
	  * @return Whether this property matches a single database-column
	  */
	def isSingleColumn = dataType.isSingleColumn
	/**
	  * @return Whether this property matches multiple database-columns
	  */
	def isMultiColumn = dataType.isMultiColumn
	
	/**
	  * @return Whether there exists an index based on this property
	  */
	def isIndexed = dbProperties.exists { _.isIndexed }
	
	/**
	 * @param settings Implicit project settings that define the common default mutability
	 * @return Mutability of this property
	 */
	def mutability(implicit settings: VaultProjectSetup) =
		customMutability.getOrElse { dataType.defaultMutability.getOrElse(settings.defaultMutability) }
	/**
	 * @param settings Implicit project settings that define the common default mutability
	 * @return Whether this property should be allowed (and expected) to change
	 */
	def isMutable(implicit settings: VaultProjectSetup) = mutability.isMutable
	/**
	 * @param settings Implicit project settings that define the common default mutability
	 * @return Whether this property should not be allowed to change
	 */
	def isImmutable(implicit settings: VaultProjectSetup) = mutability.isImmutable
	
	/**
	  * @return Some(DbProperty) if this property only matches to a single database-property. None otherwise.
	  */
	def onlyDbVariant = if (isSingleColumn) dbProperties.headOption else None
	/**
	  * @return Left(DbProperty) if this property only has a single db-variant. Right(Vector(DbProperty))
	  *         if this property has multiple db-variants.
	  */
	def oneOrManyDbVariants =
		if (isSingleColumn) Left(dbProperties.head) else Right(dbProperties)
	
	/**
	  * @return The default value assigned for this property. Empty if no default is provided.
	  */
	def defaultValue = customDefaultValue.notEmpty.getOrElse(dataType.defaultValue)
	
	/**
	  * @return Name of the table referenced by this property. None if no references are made.
	  */
	def referencedTableName = dataType match {
		case ClassReference(tableName, _, _) => Some(tableName)
		case _ => None
	}
	
	/**
	 * @return Whether this property extends another property in a parent class
	 */
	def isExtension = parents.nonEmpty
	/**
	 * @return Whether this property extends another property in a parent class, renamed
	 */
	def isRenamedExtension = parents.exists { _.name != name }
	/**
	 * @return Whether this property extends another property in a parent class without renaming it
	 */
	def isDirectExtension = parents.exists { _.name == name }
	
	/**
	 * @return Name of this property, as defined in the highest level parent (if extension is used).
	 *         If this property extends no other property, returns the name of this property.
	 */
	def originalName = OptionsIterator.iterate(Some(this)) { _.parents.headOption }.last.name
	/**
	 * @return Names of this property defined in the parent classes
	 */
	def alternativeNames = parents.view.map { _.name }.toSet - name
	
	/**
	 * @return If this property extends a parent property, renaming it,
	 *         returns the parent property name as well as the name of this (overridden / implementing) property
	 */
	def rename = parents.view.map { _.name }.find { _ != name }.map { _ -> name }
	
	/**
	  * @return A concrete (ie. non-optional) version of this property, if possible.
	  *         This if already a concrete property.
	  *         Please note that custom default values may be erased during this conversion.
	  */
	def concrete = {
		if (dataType.isConcrete)
			this
		else
			copy(dataType = dataType.concrete, customDefaultValue = CodePiece.empty)
	}
	
	/**
	 * @return A property which inherits this one
	 */
	def newChild = copy(parents = Single(this))
	
	/**
	  * @param naming Implicit naming rules
	  * @return Name of this property in json models
	  */
	def jsonPropName(implicit naming: NamingRules) = name.jsonProp
	
	/**
	  * @return Code for this property converted to a value.
	  */
	def toValueCode(implicit naming: NamingRules): CodePiece = dataType.toValueCode(name.prop)
	/**
	  * @param naming Implicit naming rules to apply
	  * @return Code that converts this property (properly named) into a value.
	  */
	def toJsonValueCode(implicit naming: NamingRules): CodePiece = dataType.toJsonValueCode(name.prop)
	
	
	// OTHER    -----------------------
	
	/**
	 * Makes this property extend the specified parent properties
	 * @param parents Parents to extend from highest to lowest priority
	 * @return Copy of this property with the specified extensions added
	 */
	def extending(parents: Seq[Property]) = {
		parents.headOption match {
			// Case: Lists parents => Replaces all empty values with those defined in parents, if available
			case Some(primaryParent) =>
				// If the singular name form matches the parent property name,
				// applies pluralization from the parent instead
				val newName = {
					if (name.isEmpty || (name.singularIn(primaryParent.name.style) ~== primaryParent.name.singular))
						primaryParent.name
					else
						name
				}
				// Parent property data types may be overridden with referencing types
				val newDataType = dataType match {
					case ref: ClassReference => ref
					case _ => primaryParent.dataType
				}
				
				val orderedImplementations = this +: parents
				copy(
					name = newName,
					dataType = newDataType,
					customDefaultValue = orderedImplementations.view.map { _.customDefaultValue }
						.find { _.nonEmpty }.getOrElse(customDefaultValue),
					dbPropertyOverrides = orderedImplementations.view.reverse.map { _.dbPropertyOverrides }
						.reduceLeftOption { (parentOverrides, childOverrides) =>
							parentOverrides.zipPad(childOverrides, DbPropertyOverrides.empty)
								.map { case (p, c) => p + c }
						}
						.getOrElse(Empty),
					customMutability = orderedImplementations.view.flatMap { _.customMutability }.headOption,
					parents = parents ++ parents,
					withAccessName = orderedImplementations.view.map { _.withAccessName }
						.find { _.nonEmpty }.getOrElse(withAccessName),
					inAccessName = orderedImplementations.view.map { _.inAccessName }
						.find { _.nonEmpty }.getOrElse(inAccessName),
					description = orderedImplementations.view.map { _.description }
						.find { _.nonEmpty }.getOrElse(description)
				)
				
			// Case: No parents listed => No change
			case None => this
		}
	}
}
