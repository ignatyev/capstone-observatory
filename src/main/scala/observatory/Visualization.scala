package observatory

import java.lang.Math._

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location     Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Double)], location: Location): Double =
    temperatures.par.find { case (loc, _) => distance(location, loc) < 1 }.map(_._2).getOrElse {
      temperatures.par.map { case (loc, temp) => temp / squared(distance(location, loc)) }.sum /
        temperatures.par.map { case (loc, _) => 1 / squared(distance(location, loc)) }.sum
    }

  private val EARTH_RADIUS_KM = 6371

  private def squared(d: Double) = d * d

  private[observatory] def distance(p: Location, q: Location): Double = {
    val (fi1, lambda1) = (p.lat, p.lon)
    val (fi2, lambda2) = (q.lat, q.lon)
    abs(
      acos(sin(toRadians(fi1)) * sin(toRadians(fi2)) +
        cos(toRadians(fi1)) * cos(toRadians(fi2)) * cos(abs(toRadians(lambda1 - lambda2)))) * EARTH_RADIUS_KM
    )
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value  The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Double, Color)], value: Double): Color = {
    val sorted = points.toSeq.sortBy(_._1)
    val lowerPoint = sorted.filter(_._1 <= value).lastOption
    val upperPoint = sorted.find(_._1 >= value)

    /*lowerPoint.map(l => upperPoint.map(u => linerp(value, l, u)).getOrElse(l._2))
      .getOrElse(upperPoint.get._2)*/

    lowerPoint match {
      case None => sorted.head._2
      case Some(lower) =>
        upperPoint match {
          case None => sorted.last._2
          case Some(upper) => linerp(value, lower, upper)
        }
    }
  }

  private def linerp(value: Double, lower: (Double, Color), upper: (Double, Color)): Color = {
    if (lower == upper) lower._2
    else {
      val (x0, y0) = lower
      val (x1, y1) = upper
      val red = (y0.red * (x1 - value) + y1.red * (value - x0)) / (x1 - x0)
      val green = (y0.green * (x1 - value) + y1.green * (value - x0)) / (x1 - x0)
      val blue = (y0.blue * (x1 - value) + y1.blue * (value - x0)) / (x1 - x0)
      Color(red.round.toInt, green.round.toInt, blue.round.toInt)
    }
  }

  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image = {
    val colorMap = for {
      lat <- 90 until -90 by -1
      lon <- -180 until 180
    } yield interpolateColor(colors, predictTemperature(temperatures, Location(lat, lon)))
    Image(360, 180, colorMap.map(color => Pixel(color.red, color.green, color.blue, 255)).toArray)
  }

}

