package utopia.coder.model.scala.declaration

import utopia.coder.model.merging.Mergeable
import utopia.coder.model.scala.code.Code
import utopia.coder.model.scala.Visibility.{Protected, Public}
import utopia.coder.model.scala.datatype.{GenericType, Reference, ScalaType}
import utopia.coder.model.scala.{Annotation, Parameters, Visibility}
import utopia.flow.collection.immutable.Empty
import utopia.flow.util.Mutate

object MethodDeclaration
{
	/**
	  * Creates a new method declaration
	  * @param name Method name
	  * @param codeReferences References made within the code (default = empty)
	  * @param visibility Method visibility (default = public)
	  * @param genericTypes Generic types to use within this method (default = empty)
	  * @param explicitOutputType Data type returned by this method, when explicitly defined (optional)
	  * @param annotations Annotations that apply to this method (default = empty)
	  * @param description Description of this method (default = empty)
	  * @param returnDescription Description of the return value of this method (default = empty)
	  * @param headerComments Lines of comments to insert before the declaration (default = empty)
	  * @param isOverridden Whether this method overrides a base member (default = false)
	  * @param isImplicit Whether this is an implicit function (default = false)
	  * @param isLowMergePriority Whether this method should be overwritten with an existing code when merging
	  *                           (default = true)
	  * @param params Method parameters (0-n)
	  * @param firstLine First line of code
	  * @param moreLines More lines of code (0-n)
	  * @return A new method
	  */
	def apply(name: String, codeReferences: Set[Reference] = Set(), visibility: Visibility = Public,
	          genericTypes: Seq[GenericType] = Empty, explicitOutputType: Option[ScalaType] = None,
	          annotations: Seq[Annotation] = Empty, description: String = "", returnDescription: String = "",
	          headerComments: Seq[String] = Empty, isOverridden: Boolean = false, isImplicit: Boolean = false,
	          isLowMergePriority: Boolean = false)
	         (params: Parameters = Parameters.empty)
	         (firstLine: String, moreLines: String*): MethodDeclaration =
		apply(visibility, name, genericTypes, params,
			Code.from(firstLine +: moreLines).referringTo(codeReferences),
			explicitOutputType, annotations, description, returnDescription, headerComments, isOverridden, isImplicit,
			isLowMergePriority)
	
	/**
	 * Creates a new method declaration
	 * @param name Method name
	 * @param code Code within this method's implementation
	 * @param visibility Method visibility (default = public)
	 * @param genericTypes Generic types to use within this method (default = empty)
	 * @param explicitOutputType Data type returned by this method, when explicitly defined (optional)
	 * @param annotations Annotations that apply to this method (default = empty)
	 * @param description Description of this method (default = empty)
	 * @param returnDescription Description of the return value of this method (default = empty)
	 * @param headerComments Lines of comments to insert before the declaration (default = empty)
	 * @param isOverridden Whether this method overrides a base member (default = false)
	 * @param isImplicit Whether this is an implicit function (default = false)
	 * @param isLowMergePriority Whether this method should be overwritten with an existing code when merging
	 *                           (default = true)
	 * @param params Method parameters (0-n)
	 * @return A new method
	 */
	def usingCode(name: String, code: Code, visibility: Visibility = Public, genericTypes: Seq[GenericType] = Empty,
	              explicitOutputType: Option[ScalaType] = None,
	              annotations: Seq[Annotation] = Empty, description: String = "", returnDescription: String = "",
	              headerComments: Seq[String] = Empty, isOverridden: Boolean = false, isImplicit: Boolean = false,
	              isLowMergePriority: Boolean = false)
	             (params: Parameters = Parameters.empty) =
		apply(visibility, name, genericTypes, params, code, explicitOutputType, annotations, description,
			returnDescription, headerComments, isOverridden, isImplicit, isLowMergePriority)
	
	/**
	  * Creates a new abstract method declaration
	  * @param name Method name
	  * @param outputType Return type of this method
	  * @param genericTypes Generic types to use within this method (default = empty)
	  * @param annotations       Annotations that apply to this method (default = empty)
	  * @param description       Description of this method (default = empty)
	  * @param returnDescription Description of the return value of this method (default = empty)
	  * @param isProtected Whether protected visibility should be used (default = empty)
	  * @param isOverridden       Whether this method overrides a base member (default = false)
	  * @param isImplicit         Whether this is an implicit function (default = false)
	  * @param isLowMergePriority Whether this method should be overwritten with an existing code when merging
	  *                           (default = true)
	  * @param params             Method parameters (0-n)
	  * @return A new abstract method declaration
	  */
	def newAbstract(name: String, outputType: ScalaType, genericTypes: Seq[GenericType] = Empty,
	                annotations: Seq[Annotation] = Empty, description: String = "", returnDescription: String = "",
	                isProtected: Boolean = false, isOverridden: Boolean = false, isImplicit: Boolean = false,
	                isLowMergePriority: Boolean = false)
	               (params: Parameters = Parameters.empty) =
		apply(if (isProtected) Protected else Public, name, genericTypes, params, Code.empty, Some(outputType),
			annotations, description, returnDescription, Empty, isOverridden, isImplicit, isLowMergePriority)
}

/**
  * Represents a scala method
  * @author Mikko Hilpinen
  * @since 30.8.2021, v0.1
  * @param visibility Visibility of this method
  * @param name Method name
  * @param genericTypes Generic types to declare within this method
  * @param parameters Parameters accepted by this method
  * @param bodyCode Code executed within this method
  * @param explicitOutputType Data type returned by this method, when explicitly defined (optional)
  * @param annotations Annotations that apply to this method
  * @param description Description of this method (may be empty)
  * @param returnDescription Description of the return value of this method (may be empty)
  * @param headerComments Lines of comments to insert before the declaration (default = empty)
  * @param isOverridden Whether this method overrides a base version
  */
case class MethodDeclaration(visibility: Visibility, name: String, genericTypes: Seq[GenericType],
                             parameters: Parameters, bodyCode: Code, explicitOutputType: Option[ScalaType],
                             annotations: Seq[Annotation], description: String, returnDescription: String,
                             headerComments: Seq[String], isOverridden: Boolean, isImplicit: Boolean,
                             isLowMergePriority: Boolean)
	extends FunctionDeclaration[MethodDeclaration] with Mergeable[MethodDeclaration, MethodDeclaration]
{
	// ATTRIBUTES   --------------------------
	
	override lazy val identifier = super.identifier
	
	
	// IMPLEMENTED  --------------------------
	
	override def keyword = "def"
	override protected def params = Some(parameters)
	
	override protected def makeCopy(visibility: Visibility, genericTypes: Seq[GenericType],
	                                parameters: Option[Parameters], bodyCode: Code,
	                                explicitOutputType: Option[ScalaType], annotations: Seq[Annotation],
	                                description: String, returnDescription: String, headerComments: Seq[String],
	                                isOverridden: Boolean, isImplicit: Boolean) =
		MethodDeclaration(visibility, name, genericTypes, parameters.getOrElse(this.parameters), bodyCode,
			explicitOutputType, annotations, description, returnDescription, headerComments, isOverridden, isImplicit,
			isLowMergePriority)
			
	
	// OTHER    -----------------------------
	
	def mapCode(f: Mutate[Code]) = copy(bodyCode = f(bodyCode))
}
