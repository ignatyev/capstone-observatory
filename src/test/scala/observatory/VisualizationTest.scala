package observatory


import observatory.Visualization.{distance, interpolateColor, predictTemperature, visualize}
import org.scalatest.prop.Checkers
import org.scalatest.{FunSuite, Matchers}

trait VisualizationTest extends FunSuite with Checkers with Matchers {
  test("interpolate color boundaries") {
    val colors = Seq((60d, Color(255, 255, 255)), (32d, Color(255, 0, 0)))
    interpolateColor(
      colors,
      60
    ) shouldBe Color(255, 255, 255)

    interpolateColor(
      colors,
      32
    ) shouldBe Color(255, 0, 0)

    interpolateColor(
      colors,
      30
    ) shouldBe Color(255, 0, 0)

    interpolateColor(
      colors,
      70
    ) shouldBe Color(255, 255, 255)
  }

  test("interpolate in between") {
    val colors = Seq((20d, Color(255, 200, 200)), (10d, Color(255, 0, 0)), (30d, Color(0, 0, 0)))
    interpolateColor(
      colors,
      15
    ) shouldBe Color(255, 100, 100)

    interpolateColor(
      colors,
      19
    ) shouldBe Color(255, 180, 180)

    interpolateColor(
      colors,
      11
    ) shouldBe Color(255, 20, 20)
  }

  test("interpolate colors negative") {
    interpolateColor(
      Seq((-20d, Color(255, 200, 200)), (-10d, Color(255, 0, 0)), (-30d, Color(0, 0, 0))),
      -11
    ) shouldBe Color(255, 20, 20)
  }

  test("distance SVO -> KUF") {
    distance(Location(55.596111, 37.2675), Location(53.501667, 50.155)) shouldBe 862d +- 1
  }

  ignore("predict t") {
    predictTemperature(Seq((Location(0, 0), 0), (Location(100, 100), 100)), Location(50, 50)) shouldBe 50d +- 2
    predictTemperature(Seq((Location(0, 0), 0), (Location(100, 100), 100)), Location(25, 25)) shouldBe 25d
  }

  ignore("visualize") {
    val colors = Seq((60d, Color(255, 255, 255)), (32d, Color(0, 255, 0)))
    visualize(Seq((Location(0, 0), 20), (Location(-50, -50), 40), (Location(100, 100), 150)), colors).output("img.png")
  }
}
