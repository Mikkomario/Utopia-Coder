package utopia.coder.vault.test

import utopia.coder.model.data.NamingRules
import utopia.coder.vault.controller.writer.database.sql.SqlWriter

/**
 * Tests unique table initials -creation
 *
 * @author Mikko Hilpinen
 * @since 29/03/2024, v1.11
 */
object TableInitialsTest extends App
{
	implicit val naming: NamingRules = NamingRules.default
	println(SqlWriter.initialsFrom(Vector(
		"address", "address_name", "analyzed_case_study_link", "attachment", "attachment_message_link", "campaign",
		"case_study", "case_study_analysis", "case_study_analysis_event", "case_study_analysis_text_statement_placement",
		"case_study_contact_link", "case_study_extraction_event", "case_study_statement_placement", "categorization",
		"category", "city", "company", "company_description_statement_placement", "company_email_address_link",
		"company_employee_count", "company_funding", "company_funding_status", "company_group", "company_group_link",
		"company_identifier", "company_identifier_scraping_event", "company_industry_link", "company_keyword_link",
		"company_location_link", "company_phone_number", "company_phone_number_link", "company_retail_locations_count",
		"company_revenue_estimate", "company_seo_statement_placement", "company_sic",
		"company_social_media_account_link", "company_technology_link", "company_website_link", "contact",
		"contact_city_link", "contact_company_email_link", "contact_company_link", "contact_company_phone_link",
		"contact_department_link", "contact_email_address_link", "contact_email_verification",
		"contact_info_scraping_event", "contact_origin_reference", "contact_phone_number_link",
		"contact_role_description", "contact_scraping_js_rendering", "contact_seniority_link",
		"contact_social_media_link", "contact_title_link", "contact_title_link_detail", "country",
		"data_added_event", "data_source", "data_type", "delimiter", "department", "discovered_company_identifier",
		"domain", "email_address", "email_address_origin_reference", "email_service", "email_service_user",
		"error_record", "form", "form_field", "funding_type", "industry", "issue", "issue_alias", "issue_comment",
		"issue_notification", "issue_occurrence", "issue_resolution", "issue_variant", "link", "link_placement",
		"llm", "llm_assignment", "llm_query_record", "message", "message_recipient_link", "message_statement_link",
		"message_thread", "message_thread_subject_link", "page_type_analysis_result", "pending_reply_reference",
		"pending_thread_reference", "person_name", "person_name_statistics", "phone_country_code", "phone_number",
		"phone_number_origin_reference", "phone_number_scraping_event", "question", "request_path",
		"scraped_phone_number_link", "scraping_batch", "seniority", "setting", "stack_trace_element_record", "state",
		"statement", "street_address", "subject", "subject_statement_link", "technology", "vast_ai_machine_host_record",
		"vast_ai_machine_load_record", "vast_ai_machine_scraping_use", "vast_ai_machine_version",
		"website_access_failure", "website_redirect", "word", "word_placement", "work_title"
	)).toVector.map { case (name, abbr) => s"$abbr => $name" }.sorted.mkString("\n"))
}
