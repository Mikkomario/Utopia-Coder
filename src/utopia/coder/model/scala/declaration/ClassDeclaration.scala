package utopia.coder.model.scala.declaration

import utopia.coder.model.scala.Visibility.Public
import utopia.coder.model.scala.code.Code
import utopia.coder.model.scala.datatype.{Extension, GenericType}
import utopia.coder.model.scala.{Annotation, DeclarationDate, Parameters, Visibility}
import utopia.flow.collection.immutable.Empty

/**
  * Used for declaring scala classes
  * @author Mikko Hilpinen
  * @since 30.8.2021, v0.1
  */
case class ClassDeclaration(name: String, genericTypes: Seq[GenericType] = Empty,
                            constructionParams: Parameters = Parameters.empty, extensions: Seq[Extension] = Empty,
                            types: Seq[TypeDeclaration] = Empty,
                            creationCode: Code = Code.empty, properties: Seq[PropertyDeclaration] = Empty,
                            methods: Set[MethodDeclaration] = Set(), nested: Set[InstanceDeclaration] = Set(),
                            visibility: Visibility = Public, annotations: Seq[Annotation] = Empty,
                            description: String = "", author: String = "", headerComments: Seq[String] = Empty,
                            since: DeclarationDate = DeclarationDate.today, isCaseClass: Boolean = false)
	extends InstanceDeclaration
{
	override val keyword = {
		if (isCaseClass)
			"case class"
		else if ((properties ++ methods).exists { _.isAbstract })
			"abstract class"
		else
			"class"
	}
	
	override def declarationType: InstanceDeclarationType = InstanceDeclarationType.ClassD
	
	override protected def constructorParams = Some(constructionParams)
	
	override protected def makeCopy(visibility: Visibility, genericTypes: Seq[GenericType],
	                                extensions: Seq[Extension], types: Seq[TypeDeclaration], creationCode: Code,
	                                properties: Seq[PropertyDeclaration], methods: Set[MethodDeclaration],
	                                nested: Set[InstanceDeclaration], annotations: Seq[Annotation],
	                                description: String, author: String,
	                                headerComments: Seq[String], since: DeclarationDate) =
		ClassDeclaration(name, genericTypes, constructionParams, extensions, types, creationCode, properties, methods,
			nested, visibility, annotations, description, author, headerComments, since, isCaseClass)
}
