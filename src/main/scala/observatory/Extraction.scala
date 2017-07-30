package observatory

import java.time.LocalDate

import org.apache.spark.sql.functions._
import org.apache.spark.sql.{Encoders, Row, SparkSession}

/**
  * 1st milestone: data extraction
  */
object Extraction {
  val spark: SparkSession =
    SparkSession
      .builder()
      .appName("Temperatures")
      .config("spark.master", "local")
      .getOrCreate()

  import spark.implicits._

  implicit val _ = Encoders.tuple(Encoders.kryo[LocalDate], Encoders.kryo[Location], Encoders.scalaDouble)

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Int, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Double)] = {
    val stations = spark.read.csv(stationsFile).toDF("stnId", "wbanId", "lat", "lon")
    val temperatures = spark.read.csv(temperaturesFile).toDF("stnId", "wbanId", "month", "day", "temperature")
    stations.join(temperatures,
      stations("stnId") === temperatures("stnId") and stations("wbanId") === temperatures("wbanId")
    )
      .filter($"lat" =!= "" and $"lon" =!= "")
      .map { row =>
        (LocalDate.of(year, row.getAs[String]("month").toInt, row.getAs[String]("day").toInt),
          Location(row.getAs[String]("lat").toDouble, row.getAs[String]("lon").toDouble),
          fahrenheitToCelsius(row.getAs[String]("temperature").toDouble))
      }
      .collect().toIterable
  }

  private def fahrenheitToCelsius(f: Double) = {
    (f - 32) * 5 / 9
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Double)]): Iterable[(Location, Double)] = {
    spark.createDataset(records.toSeq)
      .groupByKey(_._2) //location
      .agg(avg($"_3").as[Double]) //temperature
      .collect().toIterable
  }

}


