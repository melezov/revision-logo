package com.melezov.rlogo

import com.melezov.rlogo.Buffer._

import java.awt.image.BufferedImage
object Buffer {
  val w = 320
  val h = 200

  val Radii: Seq[Int] = Seq(
    88, 182, 322, 277, 393, 474, 559, 700, 802, 916, 971, 995
  )

  val Thetas: Seq[Int] = Seq(
    46, 71, 93, 125, 191, 197, 257, 272, 274, 347, 351, 373, 437, 460, 511,
    518, 552, 622, 663, 682, 748, 765, 800, 830, 833, 869, 916, 952, 994, 999,
  )

  val Snakes: Seq[Array[Byte]] = Seq(
    Array(0, 0),
    Array(1, 8, 2, 12, 1, 12),
    Array(4, 1, 3, 4, 4, 6, 5, 13, 4, 14, 3, 16, 4, 17, 5, 18, 4, 21, 3, 25, 4, 26, 5, 27, 4, 27),
    Array(6, 0),
    Array(7, 0, 8, 2, 7, 3, 8, 5, 7, 7, 8, 9, 7, 15, 8, 19, 7, 20, 8, 22, 7, 23, 8, 29, 7, 29),
    Array(9, 10, 10, 11, 9, 24, 11, 28, 9, 28),
  )
}

class Buffer(scale: Int, tilt: Double) {
  require(scale > 0)

  val used = new Array[Int](w * h)
  val bytes = new Array[Int](w * h)

  val wh = Math.min(w, h) / 2
  val wd = (w - h) / 2
  val hd = 0

  var calcs = 0

  println(tilt)

  for (si <- Snakes.indices) {
    val snake = Snakes(si)
    var sr = Radii(snake.head) / (1000.0)
    var sw = Thetas(snake.last) / 1000.0 - 1

    for (i <- 0 until snake.length - 1) {
      val ri = (i + 1) & ~1
      val wi = (i & ~1) + 1

      val er = Radii(snake(ri)) / (1000.0)
//      val er = sr + (erx - sr) / tilt

      val ew = Thetas(snake(wi)) / 1000.0

      def calc(step: Int, Steps: Int): (Int, Int) = {
        calcs += 1
        val r = sr + step * (er - sr) / Steps
        val w = tilt + (2 * Math.PI) * (sw + step * (ew - sw) / Steps)
        val x = wh + wd + Math.cos(w) * wh * r
        val y = wh + hd + Math.sin(w) * wh * r
        (x.toInt, y.toInt)
      }

      val (bx, by) = calc(0, 5)
      val (ex, ey) = calc(1, 5)
      val Steps = (Math.sqrt((bx - ex) * (bx - ex) + (by - ey) * (by - ey)) * 20).toInt

      for (step <- 0 until Steps) {
        val (x, y) = calc(step, Steps)
        setU(x, y, 1 + (si & 1))
      }

      sr = er
      sw = ew
    }
  }

  for (y <- 0 until h; x <- 0 until w) {
    val u = used(y * w + x)
    if (u > 0) {
      val col = u match {
        case 1 => 0x0000ff
        case _ => 0x00ffff
      }
      if (u == 1) {
        set(x, y, col)
      } else {
        set(x, y, col)
      }
    }
  }

  def get(x: Int, y: Int): Int = {
    if (x >= 0 && x < w && y >= 0 && y < h) {
      bytes(y * w + x)
    } else {
      0
    }
  }

  def set(x: Int, y: Int, px: Int): Unit = {
    if (x >= 0 && x < w && y >= 0 && y < h) {
      bytes(y * w + x) = px
    }
  }

  def getU(x: Int, y: Int): Int = {
    if (x >= 0 && x < w && y >= 0 && y < h) {
      used(y * w + x)
    } else {
      0
    }
  }

  def setU(x: Int, y: Int, value: Int): Unit = {
    if (x >= 0 && x < w && y >= 0 && y < h) {
      used(y * w + x) = value
    }
  }

  def draw(): BufferedImage = {
    val img = new BufferedImage(w * scale, h * scale, BufferedImage.TYPE_INT_RGB)

    for {
      y <- 0 until h
      x <- 0 until w
      px = get(x, y)
      dy <- 0 until scale
      dx <- 0 until scale
    } {
      img.setRGB(x * scale + dx, y * scale + dy, px)
    }

    img
  }
}
