package com.melezov.rlogo

import lc.kra.system.keyboard.GlobalKeyboardHook
import lc.kra.system.keyboard.event.{GlobalKeyEvent, GlobalKeyListener}

import java.awt._
import java.awt.event._
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.{ExecutorService, Executors}
import javax.swing.{JFrame, JPanel, SwingUtilities, WindowConstants}
import scala.collection.mutable

object Main extends App {
  class Canvas extends JFrame("Revision Logo Test") {
    val executor: ExecutorService = Executors.newSingleThreadExecutor()

    val windowActive = new AtomicBoolean(true)
    locally {
      val listener = new WindowAdapter() {
        override def windowClosing(e: WindowEvent): Unit = executor.shutdown()
        override def windowLostFocus(e: WindowEvent): Unit = windowActive.set(false)
        override def windowGainedFocus(e: WindowEvent): Unit = windowActive.set(true)
      }
      addWindowListener(listener)
      addWindowFocusListener(listener)
      setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    }

    private val timings = mutable.TreeMap.empty[String, String].withDefaultValue("")

    private val canvas: Canvas = this
    canvas.setMinimumSize(new Dimension(32, 32))
    canvas.setSize(new Dimension(512, 512))

    private val pane = new JPanel() {
      var mX = Option.empty[Int]
      var mY = Option.empty[Int]

      var mouseTilt = 0.0

      locally {
        val listener = new MouseMotionListener with MouseListener {
          override def mouseDragged(e: MouseEvent): Unit = {
            mouseMoved(e)
            mouseTilt = (e.getX + e.getY) / 100.0
          }

          override def mouseMoved(e: MouseEvent): Unit = {
            mX = Some(e.getX)
            mY = Some(e.getY)
            Canvas.this.repaint()
          }

          override def mouseClicked(e: MouseEvent): Unit = ()
          override def mousePressed(e: MouseEvent): Unit = ()
          override def mouseReleased(e: MouseEvent): Unit = {
            mX = None
            mY = None
            mouseTilt = 0
            Canvas.this.repaint()
          }

          override def mouseEntered(e: MouseEvent): Unit = ()
          override def mouseExited(e: MouseEvent): Unit = ()
        }
        addMouseListener(listener)
        addMouseMotionListener(listener)
      }

      override protected def paintComponent(g: Graphics): Unit = {
        super.paintComponent(g)

        val w = getWidth
        val h = getHeight

        if (w > 0 && h > 0) {
          val hueShift = h / 1000.0 - 0.5
          val logo = LogoImage(mouseTilt)

          val frameColor = if (windowActive.get) 0xffb8cfe5 else 0xffeeeeee
          val frameIcon =  logo.makeImage(16, 16)
          val taskbarIcon = logo.makeImage(32, 32)
          canvas.setIconImages(java.util.Arrays.asList(frameIcon, taskbarIcon))

          val imgTemplate = {
            val startAt = System.currentTimeMillis()
            val img = Template.makeImage(w, h)
            val endAt = System.currentTimeMillis()
            timings("0-template") = s"${w}x$h template logo took ${endAt - startAt} ms"
            img
          }

          val imgGenerated = {
            val startAt = System.currentTimeMillis()
            val img = logo.makeImage(w, h)
            val endAt = System.currentTimeMillis()
            timings("1-generated") = s"${w}x$h gen logo took ${endAt - startAt} ms"
            img
          }

          g.drawImage(imgTemplate, 0, 0, null)
          g.drawImage(imgGenerated, 0, 0, null)

          g.setFont(g.getFont.deriveFont(Font.BOLD))
          g.setColor(Color.pink)

          for (x <- mX; y <- mY) {
            var mfi = math.atan2(y - h / 2.0, x - w / 2.0) / (2 * Math.PI)
            if (mfi < 0) mfi += 1
//            mfi += mouseTilt
            g.drawString(f"Marker: $mfi%.3f", 20, 10)
          }

          g.drawString(f"Tilt: $mouseTilt%.3f", 20, 30)
          g.drawString(f"Hue shift: $hueShift%.3f", 20, 50)

          g.setColor(Color.blue)
          for (x <- mX; y <- mY) {
            g.drawLine(w / 2, h / 2, x, y)
          }

          g.setColor(Color.green.darker.darker)
          timings.zipWithIndex.foreach { case ((key, timing), index) =>
            g.drawString(timing, 20, 80 + index * 20)
          }
        }
      }
    }

    add(pane)
  }

  val hook = new GlobalKeyboardHook(true)
  hook.addKeyListener(new GlobalKeyListener {
    override def keyPressed(event: GlobalKeyEvent): Unit = {
      if (event.getVirtualKeyCode() == GlobalKeyEvent.VK_ESCAPE) {
        println("Exiting (ESC)")
        sys.exit(0)
      }
    }

    override def keyReleased(event: GlobalKeyEvent): Unit = ()
  });

  JFrame.setDefaultLookAndFeelDecorated(true)
  SwingUtilities.invokeAndWait(() => {
    new Canvas().setVisible(true)
  })
}
