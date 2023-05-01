package com.melezov.rlogo

import java.awt.{Graphics2D, RenderingHints}
import java.awt.image.BufferedImage
import javax.imageio.ImageIO

object Template {
  val image = {
    // from: https://img.demoparty.net/party/none/revision/revision-2023-logo-274.jpeg
    val is = getClass.getClassLoader.getResourceAsStream("revision-2023-logo-274.jpeg")
    val original = ImageIO.read(is)

    // shift by -6px horizontally to center the image
    val buffer = new BufferedImage(original.getWidth(), original.getHeight(), BufferedImage.TYPE_INT_RGB);
    val g = buffer.getGraphics
    g.drawImage(original, 6, 0, null);
    g.dispose()
    buffer
  }

  def makeImage(width: Int, height: Int): BufferedImage = {
    require(width > 0 && height > 0, "Cowardly refusing to generate an empty image")
    val buffer = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    val g = buffer.getGraphics.asInstanceOf[Graphics2D]

    val min = math.min(width, height)
    val x = math.max(0, width - min) / 2
    val y = math.max(0, height - min) / 2

    g.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR)
    g.drawImage(image, x, y, min, min, null)
    g.dispose()
    buffer
  }
}
