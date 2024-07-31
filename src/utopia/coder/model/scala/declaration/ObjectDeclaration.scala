package utopia.coder.model.scala.declaration

import utopia.coder.model.scala.code.Code
import utopia.coder.model.scala.Visibility.Public
import utopia.coder.model.scala.datatype.{Extension, GenericType}
import utopia.coder.model.scala.{Annotation, DeclarationDate, Visibility}
import utopia.flow.collection.immutable.Empty

/**
  * Used for declaring objects in scala files
  * @author Mikko Hilpinen
  * @since 30.8.2021, v0.1
  */
case class ObjectDeclaration(name: String, extensions: Seq[Extension] = Empty,
                             types: Seq[TypeDeclaration] = Empty,
                             creationCode: Code = Code.empty, properties: Seq[PropertyDeclaration] = Empty,
                             methods: Set[MethodDeclaration] = Set(), nested: Set[InstanceDeclaration] = Set(),
                             visibility: Visibility = Public, annotations: Seq[Annotation] = Empty,
                             description: String = "", author: String = "",
                             headerComments: Seq[String] = Empty, since: DeclarationDate = DeclarationDate.today,
                             isCaseObject: Boolean = false)
	extends InstanceDeclaration
{
	override def declarationType: InstanceDeclarationType = InstanceDeclarationType.ObjectD
	
	override def keyword = if (isCaseObject) "case object" else "object"
	
	override protected def constructorParams = None
	
	// Objects can't have generic type parameters since they're never abstract
	override def genericTypes = Empty
	
	override protected def makeCopy(visibility: Visibility, genericTypes: Seq[GenericType],
	                                extensions: Seq[Extension], types: Seq[TypeDeclaration], creationCode: Code,
	                                properties: Seq[PropertyDeclaration], methods: Set[MethodDeclaration],
	                                nested: Set[InstanceDeclaration], annotations: Seq[Annotation],
	                                description: String, author: String,
	                                headerComments: Seq[String], since: DeclarationDate) =
		ObjectDeclaration(name, extensions, types, creationCode, properties, methods, nested, visibility, annotations,
			description, author, headerComments, since, isCaseObject)
}
