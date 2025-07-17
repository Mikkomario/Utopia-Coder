package 

/**
  * Used for accessing multiple llms from the DB at a time
  * @author Mikko Hilpinen
  * @since 02.06.2025, v0.1
  */
abstract class AccessLlms[A, +Repr <: TargetingManyLike[_, Repr, _]](wrapped: AccessManyColumns) 
	extends AccessEnumsByName[A, Repr, AccessLlm[A]]
{
	// ATTRIBUTES	--------------------
	
	/**
	  * Access to the values of accessible llms
	  */
	lazy val values = AccessLlmValues(wrapped)
	
	lazy val joinToAssignments = join(HiddenProblemsTables.llmAssignment)
	
	lazy val whereAssignments = FilterByLlmAssignment(joinToAssignments)
	
	
	// IMPLEMENTED	--------------------
	
	override def enumByNameModel = LlmDbModel
}

