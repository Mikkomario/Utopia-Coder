package utopia.coder.vault.controller.writer.database

import utopia.coder.model.data.NamingRules
import utopia.coder.model.scala.DeclarationDate
import utopia.coder.model.scala.code.CodePiece
import utopia.coder.model.scala.datatype.Reference
import utopia.coder.model.scala.declaration.PropertyDeclarationType.LazyValue
import utopia.coder.model.scala.declaration.{File, ObjectDeclaration}
import utopia.coder.vault.model.data.{Class, VaultProjectSetup}
import utopia.coder.vault.util.VaultReferences.Citadel._

import scala.io.Codec
import scala.util.{Success, Try}

/**
  * Writes objects that provide access to different description link model factories and description link factories
  * @author Mikko Hilpinen
  * @since 9.10.2021, v1.2
  */
object DescriptionLinkInterfaceWriter
{
	/**
	  * Writes description link model factory and description link factory access objects
	  * @param classes   Project classes
	  * @param tablesRef Reference to the tables object
	  * @param setup     Implicit project setup
	  * @param codec     Implicit codec to use when writing files
	  * @return Reference to the link model factories + reference to the link factories +
	  *         reference to the linked description factories. Failure if file writing failed.
	  *         None if there weren't any classes that used descriptions.
	  */
	def apply(classes: Seq[Class], tablesRef: Reference)
	         (implicit setup: VaultProjectSetup, codec: Codec,
	          naming: NamingRules): Try[Option[(Reference, Reference, Reference)]] =
	{
		val targets = classes.flatMap { c => c.descriptionLinkClass.map { dc => (c, dc) } }
			.sortBy { _._1.name.singular }
		if (targets.nonEmpty)
		{
			// Each file contains a property for each described class
			// First writes database models object
			val modelProps = targets.map { case (base, desc) =>
				tableWrappingPropertyFor(base, desc, tablesRef, descriptionLinkModelFactory,
					s"Database interaction model factory for ${ base.name.doc } description links")
			}
			File(setup.dbModelPackage/"description",
				ObjectDeclaration((setup.dbModuleName + "DescriptionLinkModel").objectName,
					properties = modelProps,
					description = s"Used for interacting with description links for models in ${setup.dbModuleName}",
					since = DeclarationDate.versionedToday
				)
			).write().flatMap { modelsRef =>
				// Next writes the description link factories object
				val linkFactoryProps = targets.map { case (base, desc) =>
					tableWrappingPropertyFor(base, desc, tablesRef, descriptionLinkFactory,
						s"Factory for reading ${base.name} description links")
				}
				File(setup.factoryPackage/"description",
					ObjectDeclaration((setup.dbModuleName + "DescriptionLinkFactory").objectName,
						properties = linkFactoryProps,
						description = s"Used for accessing description link factories for the models in ${
							setup.dbModuleName}", since = DeclarationDate.versionedToday
					)
				).write().flatMap { linksRef =>
					// Finally writes the linked description factories object
					val descriptionFactoryProps = targets.zip(linkFactoryProps)
						.map { case ((base, _), linkFactoryProp) =>
							propertyFor(base, linkedDescriptionFactory,
								CodePiece(s"${linksRef.target}.${linkFactoryProp.name}", Set(linksRef)),
								s"Factory for reading descriptions linked with ${base.name.pluralDoc}")
						}
					File(setup.factoryPackage/"description",
						ObjectDeclaration((setup.dbModuleName + "LinkedDescriptionFactory").objectName,
							properties = descriptionFactoryProps,
							description = s"Used for reading linked descriptions for models in ${setup.dbModuleName}",
							since = DeclarationDate.versionedToday
						)
					).write().map { descriptionsRef => Some((modelsRef, linksRef, descriptionsRef)) }
				}
			}
		}
		else
			Success(None)
	}
	
	private def tableWrappingPropertyFor(baseClass: Class, descriptionClass: Class, tablesRef: Reference,
	                                     wrapperRef: Reference, description: String)
	                                    (implicit naming: NamingRules) =
		propertyFor(baseClass, wrapperRef,
			CodePiece(s"${ tablesRef.target }.${ descriptionClass.name.prop }", Set(tablesRef)),
			description)
	
	private def propertyFor(baseClass: Class, wrapperRef: Reference, wrappedCode: CodePiece, description: String)
	                       (implicit naming: NamingRules) =
		LazyValue(baseClass.name.prop, wrappedCode.references + wrapperRef,
			description = description)(s"${wrapperRef.target}(${wrappedCode.text})")
}
