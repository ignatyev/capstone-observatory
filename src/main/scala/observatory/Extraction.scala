package observatory

import java.nio.file.Paths
import java.time.LocalDate

import org.apache.spark.sql.functions._
import org.apache.spark.sql.{Encoders, SparkSession}

import scala.language.postfixOps

/**
  * 1st milestone: data extraction
  */
object Extraction {
  import org.apache.log4j.{Level, Logger}
  Logger.getLogger("org.apache.spark").setLevel(Level.WARN)
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
    val stationsPath = Paths.get(getClass.getResource(stationsFile).toURI).toString
    val temperaturesPath = Paths.get(getClass.getResource(temperaturesFile).toURI).toString

    val stations = spark.read.csv(stationsPath).toDF("stnId", "wbanId", "lat", "lon")
    val temperatures = spark.read.csv(temperaturesPath).toDF("stnId", "wbanId", "month", "day", "temperature")
    stations
      .filter(($"lat" isNotNull) and ($"lon" isNotNull))
      .join(temperatures,
      stations("stnId") <=> temperatures("stnId") and stations("wbanId") <=> temperatures("wbanId")
    )
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


