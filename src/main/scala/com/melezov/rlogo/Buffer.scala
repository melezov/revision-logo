package com.melezov.rlogo

import com.melezov.rlogo.Buffer._

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

object Approx extends App {
  EscHook.init()
  sys.exit(0)
}

import java.awt.image.BufferedImage

object Buffer {

  sealed abstract class Color(val index: Byte) extends Serializable with Product
  object Color {
    case object Blank extends Color(0)
    case object Wall extends Color(6)
    case object Border extends Color(12)
    case object Top extends Color(14)
  }

  val w = 320
  val h = 200

  val wh = Math.min(w, h) / 2
  val wd = (w - h) / 2
  val hd = 0

  val Snakes: Seq[Int] = Seq(
     2, 0x16, 0x0b,
     6, 0x2e, 0x41, 0x52, 0x67, 0x2e, 0x67,
    26, 0x64, 0x11, 0x46, 0x2d, 0x64, 0x3d, 0x79, 0x6d, 0x64, 0x79, 0x46, 0x82, 0x64,
        0x93, 0x79, 0x9d, 0x64, 0xb5, 0x46, 0xcd, 0x64, 0xd8, 0x79, 0xe1, 0x64, 0xe1,
     2, 0x8e, 0x0b,
    26, 0xb2, 0x0b, 0xcc, 0x16, 0xb2, 0x1e, 0xcc, 0x2f, 0xb2, 0x40, 0xcc, 0x52, 0xb2,
        0x7a, 0xcc, 0xa1, 0xb2, 0xb1, 0xcc, 0xbd, 0xb2, 0xc4, 0xcc, 0xed, 0xb2, 0xed,
    10, 0xe9, 0x53, 0xf7, 0x58, 0xe9, 0xc5, 0xfd, 0xeb, 0xe9, 0xeb,
     0,
  )

  def rg(i: Int) = Snakes(i) * (wh / 254.364)

  def fg(i: Int) = Snakes(i) * (2 * Math.PI / 237.049)

  def test(fi: Double, anim: Double): Seq[(Int, Int)] = {
    val points = new ArrayBuffer[(Int, Int)]

    var o = 0
    while (o != -1) {
      var len = Snakes(o)
      if (len == 0) {
        o = -1
      } else {
        o += 1
        var sr = rg(o)
        var sw = fg(o + len - 1) - 2 * Math.PI

        var i = 0
        while (i < len - 1) {
          val ri = (i + 1) & ~1
          val wi = (i & ~1) + 1

          val erx = rg(o + ri)
          val er = if (i == len - 2) {
            erx
          } else {
            sr + (erx - sr) * anim
          }

          val ew = fg(o + wi)

          def calc(step: Int, Steps: Int): (Int, Int) = {
            val r = sr + step * (er - sr) / Steps
            val w = sw + step * (ew - sw) / Steps + fi
            val x = wh + wd + Math.cos(w) * r
            val y = wh + hd + Math.sin(w) * r
            (x.toInt, y.toInt)
          }

          var (bx, by) = calc(0, 5)
          var (ex, ey) = calc(1, 5)
          bx -= ex
          by -= ey

          val outOfMyAssStepConstant = 10
          val Steps = (Math.sqrt(bx * bx + by * by + 1) * outOfMyAssStepConstant).toInt

          for (step <- 0 until Steps) {
            val (x, y) = calc(step, Steps)
            points += ((x, y))
          }

          sr = er
          sw = ew
          i += 1
        }

        o += len
      }
    }

    points.toSeq
  }
}

class Buffer(scale: Int, fi: Double, anim: Double) {
  require(scale > 0)

  private[this] val buffer: Array[Color] = Array.fill(w * h)(Color.Blank)

  for ((x, y) <- test(fi, anim)) set(x, y, Color.Border)

  def ff(sx: Int, sy: Int, ifCol: Color, putCol: Color): Unit = {
    val stack = new Array[Int](100000)
    var sp = 0

    def push(x: Int, y: Int): Unit = {
      stack(sp) = y * w + x
      sp += 1
    }
    push(sx, sy)

    @tailrec def ffin(): Unit = {
      if (sp == 0) return;

      sp -= 1
      val xy = stack(sp)
      val (x, y) = (xy % w, xy / w)

      if (get(x, y) == ifCol) {
        set(x, y, putCol)
        if (y > 0) push(x, y - 1)
        if (x > 0) push(x - 1, y)
        if (x < w - 1) push(x + 1, y)
        if (y < h - 1) push(x, y + 1)
      }

      ffin()
    }

    ffin()
  }

  ff(160, 14, Color.Blank, Color.Top)
  ff(160, 48, Color.Blank, Color.Top)
  ff(160, 86, Color.Blank, Color.Top)

  for {
    y <- 0 until h
    x <- 0 until w
  } {
    val px = get(x, y)
    if (px != Color.Blank && px != Color.Wall) {
      for (wall <- 1 to 4) {
        if (get(x + wall, y + wall) == Color.Blank) {
          set(x + wall, y + wall, Color.Wall)
        }
      }
    }
  }

  def get(x: Int, y: Int): Color = {
    if (x >= 0 && x < w && y >= 0 && y < h) {
      buffer(y * w + x)
    } else {
      Color.Blank
    }
  }

  def set(x: Int, y: Int, col: Color): Unit = {
    if (x >= 0 && x < w && y >= 0 && y < h) {
      buffer(y * w + x) = col
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
      val col = VgaPalette(px.index)
      img.setRGB(x * scale + dx, y * scale + dy, col)
    }

    img
  }
}
