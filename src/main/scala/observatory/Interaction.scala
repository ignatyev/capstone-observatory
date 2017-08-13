package observatory

import java.lang.Math._

import com.sksamuel.scrimage.{Image, Pixel}
import observatory.Visualization.{interpolateColor, predictTemperature}

/**
  * 3rd milestone: interactive visualization
  */
object Interaction {

  /**
    * @param zoom Zoom level
    * @param x X coordinate
    * @param y Y coordinate
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(zoom: Int, x: Int, y: Int): Location = {
    val n = pow(2, zoom)
    val lonDeg = x.toDouble / n * 360 - 180
    val latRad = atan(sinh(PI * (1 - 2 * y.toDouble / n)))
    val latDeg = latRad * 180 / PI
    Location(latDeg, lonDeg)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param zoom Zoom level
    * @param x X coordinate
    * @param y Y coordinate
    * @return A 256×256 image showing the contents of the tile defined by `x`, `y` and `zooms`
    */
  def tile(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)], zoom: Int, x: Int, y: Int): Image = {
    val colorMap = for {
      x <- 0 until 256
      y <- 0 until 256
    } yield interpolateColor(colors, predictTemperature(temperatures, tileLocation(256, x, y)))
    Image(256, 256, colorMap.map(color => Pixel(color.red, color.green, color.blue, 127)).toArray)
    //val location = tileLocation(zoom, x, y)
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
    yearlyData: Iterable[(Int, Data)],
    generateImage: (Int, Int, Int, Int, Data) => Unit
  ): Unit = {
    for {
      zoom <- 0 to 3
      x <- 0 to zoom
      y <- 0 to zoom
      (year, data) <- yearlyData
    } generateImage(year, zoom, x, y, data)
  }

}
