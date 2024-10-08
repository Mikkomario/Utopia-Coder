package utopia.coder.vault.model.data

import utopia.coder.model.data.{Filter, Name, NamingRules}
import utopia.flow.collection.immutable.{Empty, Pair}
import utopia.flow.operator.MaybeEmpty
import utopia.flow.util.Version
import utopia.coder.model.scala.Package
import utopia.coder.vault.model.enumeration.Mutability

/**
  * Contains project/module classes, enumerations etc.
  * @author Mikko Hilpinen
  * @since 14.10.2021, v1.2
  * @param moduleName Name of this project (db part if has two names)
  * @param modelPackage Package that contains the models and enumerations for this project
  * @param databasePackage Package that contains the database interactions for this project
  * @param databaseName Name of the database to introduce (optional)
  * @param enumerations Enumerations in this project
  * @param classes Classes in this project
  * @param combinations Combinations in this project
  * @param instances Introduced class instances in this project
  * @param namingRules Naming rules to use in this project
  * @param version Project version
  * @param defaultMutability Whether defined properties should be mutable or immutable by default
 * @param modelCanReferToDB Whether model classes are allowed to refer to database classes
  * @param prefixColumnNames Whether column names should have a prefix
  */
case class ModuleData(moduleName: Name, modelPackage: Package, databasePackage: Package,
                      databaseName: Option[Name], enumerations: Seq[Enum],
                      classes: Seq[Class], combinations: Seq[CombinationData], instances: Seq[Instance],
                      namingRules: NamingRules, version: Option[Version], defaultMutability: Mutability,
                      modelCanReferToDB: Boolean, prefixColumnNames: Boolean)
	extends MaybeEmpty[ModuleData]
{
	// COMPUTED ------------------------------
	
	/**
	  * @return A copy of this module data with only classes remaining
	  */
	def onlyClasses = copy(enumerations = Empty)
	/**
	  * @return A copy of this module data with only classes remaining (excluding combinations)
	  */
	def onlyBaseClasses = copy(enumerations = Empty, combinations = Empty)
	/**
	  * @return A copy of this module data with only enumerations remaining
	  */
	def onlyEnumerations = copy(classes = Empty, combinations = Empty)
	
	/**
	  * @return Copy of this data set without any combo-classes included
	  */
	def withoutCombos = copy(combinations = Empty)
	
	
	// IMPLEMENTED  ---------------------------
	
	override def self = this
	
	/**
	  * @return Whether this data set is completely empty
	  */
	override def isEmpty = enumerations.isEmpty && classes.isEmpty && combinations.isEmpty
	
	
	// OTHER    -------------------------------
	
	/**
	  * @param filter A filter to apply
	  * @return Copy of this module's data that keeps items that are somehow matched with that filter
	  */
	def filter(filter: Filter) = {
		val baseFilteredClasses = classes.filter { c => filter(c.name) || filter(c.packageName) }
		val (filteredClasses, filteredCombos) = comboInclusiveClasses(baseFilteredClasses) { c => filter(c.name) }
		copy(classes = filteredClasses, enumerations = enumerations.filter { e => filter(e.name) },
			combinations = filteredCombos)
	}
	/**
	  * @param filter A filter to apply
	  * @param includeCombos Whether combo objects and combining classes should be included as well (default = false)
	  * @return A copy of this data with only classes remaining which match the filter by their name
	  */
	def filterByClassName(filter: Filter, includeCombos: Boolean = false) = {
		val f = { c: Class => filter(c.name) }
		if (includeCombos) filterByClass { c => filter(c.name) } else filterClassesOnly(f)
	}
	/**
	  * @param filter A filter to apply
	  * @param includeCombos Whether combo objects and combining classes should be included as well (default = false)
	  * @return A copy of this data with only classes remaining which match the filter by their package name
	  */
	def filterByPackage(filter: Filter, includeCombos: Boolean = false) = {
		val f = { c: Class => filter(c.packageName) }
		if (includeCombos) filterByClass(f) else filterClassesOnly(f)
	}
	
	/**
	  * @param f A filter to apply
	  * @return A copy of this data with only classes remaining which match the filter
	  */
	def filterByClass(f: Class => Boolean) = filterByClassOrCombo(f) { _ => false }
	/**
	  * @param f A filter to apply to classes
	  * @return Classes accepted by the specified filter. Won't include any combos or related classes.
	  */
	def filterClassesOnly(f: Class => Boolean) =
		copy(enumerations = Empty, classes = parentInclusiveClasses(classes.filter(f)), combinations = Empty)
	/**
	  * @param includeClass A function that returns true for classes that should be included in the results
	  *                     (also includes their related combinations)
	  * @param includeCombo A function that returns true for combinations that should be included in the results
	  *                     (also includes their classes)
	  * @return A copy of this data with only classes remaining which match the filter
	  */
	def filterByClassOrCombo(includeClass: Class => Boolean)(includeCombo: CombinationData => Boolean) = {
		val remainingClasses = classes.filter(includeClass)
		val (filteredClasses, filteredCombos) = comboInclusiveClasses(remainingClasses)(includeCombo)
		copy(enumerations = Empty, classes = filteredClasses, combinations = filteredCombos)
	}
	
	/**
	  * @param filter A filter to apply
	  * @return A copy of this data with only enumerations remaining which match the filter by their name
	  */
	def filterByEnumName(filter: Filter) =
		copy(enumerations = enumerations.filter { e => filter(e.name) }, classes = Empty, combinations = Empty)
	
	private def comboInclusiveClasses(filteredClasses: Seq[Class])
	                                 (comboInclusionCondition: CombinationData => Boolean) =
	{
		val filteredCombos = combinations.filter { c =>
			comboInclusionCondition(c) || filteredClasses.contains(c.childClass) ||
				filteredClasses.contains(c.parentClass)
		}
		val comboClasses = filteredCombos.flatMap { c => Pair(c.parentClass, c.childClass) }
		
		parentInclusiveClasses((filteredClasses ++ comboClasses).distinct) -> filteredCombos
	}
	
	private def parentInclusiveClasses(filteredClasses: Seq[Class]): Seq[Class] = {
		val referencedParents = filteredClasses.flatMap { _.parents }
		// Case: Some parents were added => Makes sure their parents are included, also
		if (referencedParents.nonEmpty)
			(filteredClasses ++ parentInclusiveClasses(referencedParents)).distinct
		// Case: No parents were added
		else
			filteredClasses
	}
}