package com.jakway.sqlpp.config

import com.jakway.sqlpp.error.SqlppError

class PrioritizedPref[A](val orderedGetters: Seq[() => Either[SqlppError, A]]) {
  def apply(): Either[SqlppError, A] = {
    ???
  }
}
