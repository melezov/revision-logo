package com.melezov.rlogo

import lc.kra.system.keyboard.GlobalKeyboardHook
import lc.kra.system.keyboard.event.{GlobalKeyEvent, GlobalKeyListener}

import java.awt._
import java.awt.event._
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.{ExecutorService, Executors}
import javax.swing.{JFrame, JPanel, SwingUtilities, WindowConstants}
import scala.collection.mutable

object Main320 extends App {
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

        val ws = w / Buffer.w
        val hs = h / Buffer.h

        if (ws > 0 && hs > 0) {
          val scale = Math.min(ws, hs)
          val b = new Buffer(scale, mouseTilt)
          val img = b.draw()
          g.drawImage(img, 0, 0, null)
        }

        g.dispose()
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
