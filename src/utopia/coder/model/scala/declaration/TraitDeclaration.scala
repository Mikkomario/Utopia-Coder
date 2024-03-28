package utopia.coder.model.scala.declaration

import utopia.coder.model.scala.Visibility.Public
import utopia.coder.model.scala.code.Code
import utopia.coder.model.scala.datatype.{Extension, GenericType}
import utopia.coder.model.scala.{Annotation, DeclarationDate, Visibility}

/**
  * Used for declaring traits
  * @author Mikko Hilpinen
  * @since 2.9.2021, v0.1
  */
case class TraitDeclaration(name: String, genericTypes: Seq[GenericType] = Vector(),
                            extensions: Vector[Extension] = Vector(), types: Vector[TypeDeclaration] = Vector.empty,
                            properties: Vector[PropertyDeclaration] = Vector(),
                            methods: Set[MethodDeclaration] = Set(), nested: Set[InstanceDeclaration] = Set(),
                            visibility: Visibility = Public, annotations: Seq[Annotation] = Vector(),
                            description: String = "", author: String = "",
                            headerComments: Vector[String] = Vector(), since: DeclarationDate = DeclarationDate.today,
                            isSealed: Boolean = false)
	extends InstanceDeclaration
{
	override protected def constructorParams = None
	
	override def creationCode = Code.empty
	
	override def keyword = if (isSealed) "sealed trait" else "trait"
	
	override protected def makeCopy(visibility: Visibility, genericTypes: Seq[GenericType],
	                                extensions: Vector[Extension], types: Vector[TypeDeclaration], creationCode: Code,
	                                properties: Vector[PropertyDeclaration], methods: Set[MethodDeclaration],
	                                nested: Set[InstanceDeclaration], annotations: Seq[Annotation],
	                                description: String, author: String,
	                                headerComments: Vector[String], since: DeclarationDate) =
	{
		if (creationCode.nonEmpty) {
			println(s"WARNING: the following code is removed from trait $name upon merging:")
			println(creationCode)
		}
		TraitDeclaration(name, genericTypes, extensions, types, properties, methods, nested, visibility, annotations,
			description, author, headerComments, since, isSealed)
	}
}
