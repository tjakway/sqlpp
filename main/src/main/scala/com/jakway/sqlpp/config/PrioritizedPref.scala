package com.jakway.sqlpp.config

import com.jakway.sqlpp.config.PrioritizedPref.{NoPrefFoundError, OrderedGetters}
import com.jakway.sqlpp.config.env.{SqlppSystemEnvReader, SqlppSystemPropertyReader}
import com.jakway.sqlpp.config.error.ConfigError
import com.jakway.sqlpp.error.SqlppError

/**
 *
 * @param orderedGetters prioritized list of getters
 *                       index 0 = highest priority
 * @param default
 * @tparam A
 */
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

  /**
   * get an argument using the following list of priorities
   *  (lowest number = highest priority):
   *
   *    1: optional CLI arg
   *    2: java property (passed using -DFOO=bar)
   *    3. system environment variable
   *    4. optional default
   *
   * it's an error if no value is found
   *
   * @param varName
   * @param default
   * @param parseArg
   * @param configOption
   * @tparam A
   * @return
   */
  def standardEnvOrder[A](varName: String,
                          default: Option[A],
                          parseArg: String => Either[SqlppError, A],
                          configOption: Option[String]):
    Either[SqlppError, A] = {

    val orderedGetters: OrderedGetters[String] = {
      configOption.map(res => (() => Right(res))).toSeq ++
        Seq(
          () => SqlppSystemPropertyReader.getPropertyF(varName),
          () => SqlppSystemEnvReader.getPropertyF(varName))
    }

    val res = new PrioritizedPref[String](orderedGetters, None).apply()

    //we ***don't*** directly map PrioritizedPref.apply()
    //to parseArg because we want to fail early if an env variable
    //was found but failed to parse
    //rather than continuing to try and find a parseable arg
    //lower on the priority list

    val parsedValue = res match {
        //parse arg after it's found
      case Right(x) => parseArg(x)

        //if we haven't found any args,
      case Left(e) => default match {
          //return the default if found or
        case Some(defFound) => Right(defFound)
          //return original error
        case None => Left(e)
      }
    }

    parsedValue
  }
}