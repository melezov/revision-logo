package com.melezov.rlogo

import lc.kra.system.keyboard.GlobalKeyboardHook
import lc.kra.system.keyboard.event.{GlobalKeyEvent, GlobalKeyListener}

object EscHook {
  def init(): Unit = {
    val hook = new GlobalKeyboardHook(true)
    hook.addKeyListener (new GlobalKeyListener {
      override def keyPressed(event: GlobalKeyEvent): Unit = {
        if (event.getVirtualKeyCode == GlobalKeyEvent.VK_ESCAPE) {
          println("Exiting (ESC)")
          sys.exit(0)
        }
      }

      override def keyReleased (event: GlobalKeyEvent): Unit = ()
    })
  }
}
