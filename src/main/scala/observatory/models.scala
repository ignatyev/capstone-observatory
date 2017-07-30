package observatory

case class Location(lat: Double, lon: Double)

case class Color(red: Int, green: Int, blue: Int)

case class Station(stnId: String, wbanId: String, lat: Double, lon: Double)

case class TemperatureRecord(stnId: String, wbanId: String, month: Int, day: Int, temperature: Double)
