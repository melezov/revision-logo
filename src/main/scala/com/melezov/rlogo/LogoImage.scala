package com.melezov.rlogo

import com.melezov.rlogo.LogoImage._

import java.awt._
import java.awt.geom.{Ellipse2D, Line2D}
import java.awt.image.BufferedImage

object LogoImage {
  val Radii = Seq(
    88, 182, 322, 277, 393, 474, 559, 700, 802, 916, 971, 995
  )

  val Thetas = Seq(
     46,  71,  93, 125, 191, 197, 257, 272, 274, 347, 351, 373, 437, 460, 511,
    518, 552, 622, 663, 682, 748, 765, 800, 830, 833, 869, 916, 952, 994, 999,
  )

  val Snakes = Seq[Array[Byte]](
    Array(0, 0),
    Array(1, 8, 2, 12, 1, 12),
    Array(4, 1, 3, 4, 4, 6, 5, 13, 4, 14, 3, 16, 4, 17, 5, 18, 4, 21, 3, 25, 4, 26, 5, 27, 4, 27),
    Array(6, 0),
    Array(7, 0, 8, 2, 7, 3, 8, 5, 7, 7, 8, 9, 7, 15, 8, 19, 7, 20, 8, 22, 7, 23, 8, 29, 7, 29),
    Array(9, 10, 10, 11, 9, 24, 11, 28, 9, 28),
  )

  val Steps = 400
}

case class LogoImage(tilt: Double) {
  def makeImage(width: Int, height: Int): BufferedImage = {
    require(width > 0 && height > 0, "Cowardly refusing to generate an empty image")
    val image = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
    draw(image, width, height)
    image
  }

  def draw(image: BufferedImage, width: Int, height: Int): Unit = {
    val size = math.min(width, height)
    val actualSize = size * 0.95
    val radius = actualSize / 2
    val g = image.createGraphics()

    g.translate((width - actualSize) / 2, (height - actualSize) / 2)
    draw(g, radius, radius)
    g.dispose()
  }

  private[this] def draw(g: Graphics2D, width: Double, height: Double): Unit = {
    g.setColor(Color.red)
    for (ring <- Radii) {
      for (step <- 0 until Steps) {
        val w = tilt + step * (2 * Math.PI / Steps)
        val x = width + Math.cos(w) * width * ring
        val y = height + Math.sin(w) * height * ring

        if (step % 100 < 2) {
          g.draw(new Ellipse2D.Double(x, y, 3, 3))
        }
      }
    }

    for (snake <- Snakes) {
      var sr = Radii(snake.head) / 1000.0
      var sw = Thetas(snake.last) / 1000.0 - 1

      for (i <- 0 until snake.length - 1) {
        val ri = (i + 1) & ~1
        val wi = (i & ~1) + 1

        val er = Radii(snake(ri)) / 1000.0
        val ew = Thetas(snake(wi)) / 1000.0

        for (step <- 0 until Steps) {
          g.setColor(new Color(math.min(step * 500 / Steps, 255), math.min(step * 255 / Steps, 255), 255 - math.min(step * 255 / Steps, 255)))
          val w = tilt + (2 * Math.PI) * (sw + step * (ew - sw) / Steps)
          val r = sr + step * (er - sr) / Steps
          val x = width + Math.cos(w) * width * r
          val y = height + Math.sin(w) * height * r
          g.draw(new Line2D.Double(x + 1, y + 1, x - 1, y - 1))
          g.draw(new Line2D.Double(x + 1, y - 1, x - 1, y + 1))
        }

        sr = er
        sw = ew
      }
    }
  }
}
