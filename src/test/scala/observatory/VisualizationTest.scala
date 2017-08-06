package observatory


import org.scalatest.prop.Checkers
import org.scalatest.{FunSuite, Matchers}

trait VisualizationTest extends FunSuite with Checkers with Matchers {
  test("interpolate color boundaries") {
    val colors = Seq((60d, Color(255, 255, 255)), (32d, Color(255, 0, 0)))
    Visualization.interpolateColor(
      colors,
      60
    ) shouldBe Color(255, 255, 255)

    Visualization.interpolateColor(
      colors,
      32
    ) shouldBe Color(255, 0, 0)

    Visualization.interpolateColor(
      colors,
      30
    ) shouldBe Color(255, 0, 0)

    Visualization.interpolateColor(
      colors,
      70
    ) shouldBe Color(255, 255, 255)
  }

  test("interpolate in between") {
    val colors = Seq((20d, Color(255, 200, 200)), (10d, Color(255, 0, 0)), (30d, Color(0, 0, 0)))
    Visualization.interpolateColor(
      colors,
      15
    ) shouldBe Color(255, 100, 100)

    Visualization.interpolateColor(
      colors,
      19
    ) shouldBe Color(255, 180, 180)

    Visualization.interpolateColor(
      colors,
      11
    ) shouldBe Color(255, 20, 20)
  }

  test("interpolate colors negative") {
    Visualization.interpolateColor(
      Seq((-20d, Color(255, 200, 200)), (-10d, Color(255, 0, 0)), (-30d, Color(0, 0, 0))),
      -11
    ) shouldBe Color(255, 20, 20)
  }
}
