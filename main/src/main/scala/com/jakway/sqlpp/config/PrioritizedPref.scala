package com.jakway.sqlpp.config

import com.jakway.sqlpp.config.PrioritizedPref.{NoPrefFoundError, OrderedGetters}
import com.jakway.sqlpp.config.error.ConfigError
import com.jakway.sqlpp.error.SqlppError

class PrioritizedPref[A](val orderedGetters: OrderedGetters[A],
                         val default: Option[A] = None) {
  def apply(): Either[SqlppError, A] = {
    val empty: (Seq[SqlppError], Option[A]) = (Seq.empty, None)
    val res = orderedGetters.foldLeft(empty) {
      case ((errors, None), thisGetter) => {
        thisGetter() match {
          case Right(x) => (errors, Some(x))
          case Left(e) => (errors :+ e, None)
        }
      }
      case ((errors, Some(x)), _) => (errors, Some(x))
    }

    res match {
      case (_, Some(x)) => Right(x)
      case (errors, None) => {
        default match {
            //use the default if no getters returned values
          case Some(x) => Right(x)
            //if there is no default it's an error
          case None => Left(new NoPrefFoundError(errors))
        }
      }
    }
  }
}

object PrioritizedPref {
  type OrderedGetters[A] = Seq[() => Either[SqlppError, A]]

  class NoPrefFoundError(val errors: Seq[SqlppError])
    extends ConfigError(errors)

  class EmptyOptionError(override val msg: String)
    extends ConfigError(msg)
}