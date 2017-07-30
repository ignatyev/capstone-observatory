package observatory

import java.time.LocalDate

import org.scalactic.TolerantNumerics
import org.scalatest.{FunSuite, Matchers}

trait ExtractionTest extends FunSuite with Matchers {
  test("locateTemperatures") {
    Extraction.locateTemperatures(2015, "/stations.csv", "/2015.csv") should
      contain theSameElementsAs Seq(
      (LocalDate.of(2015, 8, 11), Location(37.35, -78.433), 27.3 +- 0.1),
      (LocalDate.of(2015, 12, 6), Location(37.358, -78.438), 0.0 +- 0.1),
      (LocalDate.of(2015, 1, 29), Location(37.358, -78.438), 2.0 +- 0.1)
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

  test("the same elements") {
    List(1, 2) should contain theSameElementsAs List(2, 1)
  }

  test("wtf") {
    implicit val doubleEquality = TolerantNumerics.tolerantDoubleEquality(0.01)

    Array(2.000000000000001.round, 0.0, 27.299999999999997) should
      contain theSameElementsAs List(27.3 +- 0.1, 0.0 +- 0.1, 2.0 +- 0.1)
  }
}