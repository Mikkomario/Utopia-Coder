package utopia.coder.vault.model.data

import utopia.coder.model.data.Name
import utopia.flow.util.UncertainBoolean
import utopia.flow.util.StringExtensions._

object DbPropertyOverrides
{
	/**
	  * Empty (ie. non-modifying) overrides
	  */
	val empty = apply()
}

/**
  * Contains user-specified default-setting overrides concerning an individual database-property
  * @author Mikko Hilpinen
  * @since 18.7.2022, v1.5.1
  * @param name Custom name to use for this property
  * @param columnName Custom column name to use for the described property. Empty if no custom (default)
  * @param default Custom default value of the described property. Empty if no custom value (default)
  * @param lengthRule Length rule to apply to the described column / property. Empty if no rule applied (default)
  * @param indexing Custom (overriding) indexing rules to apply to the described property
  *                       (default = undefined = use data type default)
  */
case class DbPropertyOverrides(name: Option[Name] = None, columnName: String = "", default: String = "",
                               lengthRule: String = "", indexing: UncertainBoolean = UncertainBoolean)
{
	/**
	 * Combines this set of overrides with another.
	 * The values defined in 'other' take priority.
	 * @param other Another set of database property overrides.
	 * @return Combination of this overrides, preferring 'other' in cases where both overrides define a value.
	 */
	def +(other: DbPropertyOverrides) = copy(
		name = other.name.orElse(name),
		columnName = other.columnName.nonEmptyOrElse(columnName),
		default = other.default.nonEmptyOrElse(default),
		lengthRule = other.lengthRule.nonEmptyOrElse(lengthRule),
		indexing = other.indexing.orElse(indexing))
}