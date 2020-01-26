package com.jakway.sqlpp.config.unchecked

import java.io.File
import java.util.Formatter

import com.jakway.sqlpp.config.error.ConfigError
import com.jakway.sqlpp.config.unchecked.CreateProfileDirOption.Errors.CreateProfileDirOptionError
import com.jakway.sqlpp.error.{CheckFile, SqlppError}
import org.slf4j.{Logger, LoggerFactory}

sealed abstract class CreateProfileDirOption {
  def valueName: String
  def choices: Option[Set[String]] = None
}

object CreateProfileDirOption {
  val logger: Logger = LoggerFactory.getLogger(getClass)

  /****************************************************************************/
  case object NoCreateProfileDir extends CreateProfileDirOption {
    override def valueName: String = "Don't create profile dir"
    override def choices: Option[Set[String]] = Some(Set("none"))
  }

  case object CreateDefaultProfileDir extends CreateProfileDirOption {
    override def valueName: String = "Create default profile dir"
    override def choices: Option[Set[String]] = Some(Set("default"))
  }

  class CreateUserPassedProfileDir(toCreate: File)
    extends CreateProfileDirOption {
    override def valueName: String = "Create profile dir at user-defined path"
    override def choices: Option[Set[String]] = Some(Set("<path>"))
  }

  object CreateUserPassedProfileDir {
    def apply(toCreate: File):
      Either[SqlppError, CreateUserPassedProfileDir] = {

      for {
        _ <- CheckFile.checkDoesNotExist(toCreate)
      } yield {
        new CreateUserPassedProfileDir(toCreate)
      }
    }
  }

  /****************************************************************************/

  object Errors {
    class CreateProfileDirOptionError(override val msg: String)
      extends ConfigError(msg)
  }

  /****************************************************************************/

  private def allChoices: Seq[(String, Option[Set[String]])] = {
    val xs: Seq[CreateProfileDirOption] = Seq(
      NoCreateProfileDir,
      CreateDefaultProfileDir,
      new CreateUserPassedProfileDir(new File(""))
    )

    xs.map(x => (x.valueName, x.choices))
  }


  object Print {
    def newFmt(): Formatter = {
      val sb = new StringBuffer()
      new Formatter(sb)
    }

    private val defaultPrintChoicesSeparator: String = ":"
    def printChoices(
      choices: Seq[(String, Option[Set[String]])] = allChoices,
      separator: String = defaultPrintChoicesSeparator): String = {
      def printChoice(x: Option[Set[String]]): String = x match {
        case Some(xs) => {
          val closingBracket =
            if(xs.size > 0) {
              " }"
            } else {
              "}"
            }

          "{ " + xs.reduce(_ + ", " + _) + closingBracket
        }

        case None => ""
      }

      val fmt = newFmt()

      if(choices.isEmpty) {
        logger.warn("printChoices called with empty argument")
        ""
      } else {
        val minWidth = choices.map(_._1).min

        val formatStr: String = {
          def singleArg(leftJustify: Boolean) = {
            val justifyStr =
              if(leftJustify) {
                "-"
              } else {
                ""
              }

            "%" + justifyStr + s"$minWidth.$minWidth" + "s"
          }

          singleArg(true) +
            " " + separator + " " +
            singleArg(false) +
            "\n"
        }

        choices.foreach {
          case (left, right) => {
            fmt.format(formatStr, left, printChoice(right))
          }
        }

        fmt.toString
      }
    }

    def errMsg(whenParsing: String, toParse: String): String = {
      val fmt = newFmt()
      fmt.format("%s not recognized when parsing %s.\n",
        toParse, whenParsing)
      fmt.format("%s", printChoices())

      fmt.toString
    }
  }

  private def reformatErrorMessage(underlyingError: SqlppError,
                                   optionName: String): SqlppError = {
    val fmt = Print.newFmt()
    fmt.format(s"Error handling %s caused by %s\n",
      optionName, underlyingError.print)
    fmt.format("%s arguments:\n", optionName)
    fmt.format("%s", Print.printChoices())

    new CreateProfileDirOptionError(fmt.toString)
  }

  def parse(str: String, optionName: String):
    Either[SqlppError, CreateProfileDirOption] = {

    def matches(choices: Option[Set[String]]): Boolean = {
      choices match {
        case Some(xs) => {
          xs.contains(str)
        }
        case None => false
      }
    }

    str match {
      case _ if matches(NoCreateProfileDir.choices) =>
        Right(NoCreateProfileDir)

      case _ if matches(CreateDefaultProfileDir.choices) =>
        Right(CreateDefaultProfileDir)

      case _ => {
        CreateUserPassedProfileDir(new File(str)) match {
          case Right(x) => Right(x)
          case Left(e) =>
            //wrap and return error
            Left(reformatErrorMessage(e, optionName))
        }
      }
    }
  }
}
