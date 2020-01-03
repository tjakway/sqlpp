package com.jakway.sqlpp.util

import java.io.Writer

class MultiWriter(val forwardTo: Seq[Writer])
  extends Writer {
  override def write(chars: Array[Char], i: Int, i1: Int): Unit = {
    forwardTo.foreach(_.write(chars, i, i1))
  }

  override def flush(): Unit = {
    forwardTo.foreach(_.flush())
  }

  override def close(): Unit = {
    forwardTo.foreach(_.close())
  }
}
