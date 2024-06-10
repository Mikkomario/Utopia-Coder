package utopia.coder.model.scala.declaration

import utopia.coder.model.scala.Visibility.Public
import utopia.coder.model.scala.code.Code
import utopia.coder.model.scala.datatype.{Extension, GenericType}
import utopia.coder.model.scala.{Annotation, DeclarationDate, Visibility}
import utopia.flow.collection.immutable.Empty

/**
  * Used for declaring traits
  * @author Mikko Hilpinen
  * @since 2.9.2021, v0.1
  */
case class TraitDeclaration(name: String, genericTypes: Seq[GenericType] = Empty,
                            extensions: Seq[Extension] = Empty, types: Seq[TypeDeclaration] = Empty,
                            properties: Seq[PropertyDeclaration] = Empty,
                            methods: Set[MethodDeclaration] = Set(), nested: Set[InstanceDeclaration] = Set(),
                            visibility: Visibility = Public, annotations: Seq[Annotation] = Empty,
                            description: String = "", author: String = "",
                            headerComments: Seq[String] = Empty, since: DeclarationDate = DeclarationDate.today,
                            isSealed: Boolean = false)
	extends InstanceDeclaration
{
	override protected def constructorParams = None
	
	override def creationCode = Code.empty
	
	override def keyword = if (isSealed) "sealed trait" else "trait"
	
	override protected def makeCopy(visibility: Visibility, genericTypes: Seq[GenericType],
	                                extensions: Seq[Extension], types: Seq[TypeDeclaration], creationCode: Code,
	                                properties: Seq[PropertyDeclaration], methods: Set[MethodDeclaration],
	                                nested: Set[InstanceDeclaration], annotations: Seq[Annotation],
	                                description: String, author: String,
	                                headerComments: Seq[String], since: DeclarationDate) =
	{
		if (creationCode.nonEmpty) {
			println(s"WARNING: the following code is removed from trait $name upon merging:")
			println(creationCode)
		}
		TraitDeclaration(name, genericTypes, extensions, types, properties, methods, nested, visibility, annotations,
			description, author, headerComments, since, isSealed)
	}
}
