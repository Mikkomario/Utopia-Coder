package utopia.coder.vault.test

import utopia.coder.model.data.NamingRules
import utopia.coder.vault.controller.writer.database.SqlWriter

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
		"airline_code", "airport_code", "fuel_operation", "fuel_order", "refueling_order", "code_context", "aircraft",
		"curve", "installation_airport", "linear_path", "credit_card", "flight_identifier", "company", "fuel_product",
		"fuel_user", "air_carrier", "contract", "aircraft_transfer", "turnaround", "air_carrier_restriction",
		"arrival_flight", "air_carrier_aircraft_link", "air_carrier_code_link", "air_carrier_destination_code_link",
		"air_carrier_flight_origin_link", "aircraft_transfer_location_link", "aircraft_transfer_time", "arrival_transfer_link"
	)).toVector.map { case (name, abbr) => s"$abbr => $name" }.sorted.mkString("\n"))
}
