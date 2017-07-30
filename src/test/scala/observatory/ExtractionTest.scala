package observatory

import java.time.LocalDate

import org.scalatest.{FunSuite, Matchers}

trait ExtractionTest extends FunSuite with Matchers {
  test("locateTemperatures") {
    Extraction.locateTemperatures(2000, "/stations.csv", "/2000.csv") should
      contain theSameElementsAs Iterable(
      (LocalDate.of(2000, 1, 1), Location(-21.23, -175.15), 0),
      (LocalDate.of(2000, 2, 2), Location(-21.23, -175.15), 5)
    )
  }

  test("locationYearlyAverageRecords") {
    val year = 2000
    Extraction.locationYearlyAverageRecords(
      Iterable(
        (LocalDate.of(year, 1, 1), Location(0, 0), 2),
        (LocalDate.of(year, 2, 1), Location(0, 0), 4),
        (LocalDate.of(year, 3, 1), Location(1, 1), 0),
        (LocalDate.of(year, 4, 1), Location(1, 1), 4)
      )
    ) should contain theSameElementsAs Iterable((Location(0, 0), 3), (Location(1, 1), 2))
  }

}