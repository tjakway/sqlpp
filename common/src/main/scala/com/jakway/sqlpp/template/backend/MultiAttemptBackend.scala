package com.jakway.sqlpp.template.backend

import java.io.{File, InputStream}

import com.jakway.sqlpp.error.{CheckFile, SqlppError}
import com.jakway.sqlpp.template.ValueSource
import com.jakway.sqlpp.template.backend.MultiAttemptBackend.MultiAttemptBackendError
import com.jakway.sqlpp.template.backend.PropertiesBackend.PropertiesBackendError
import com.jakway.sqlpp.util.TryToEither
import org.slf4j.{Logger, LoggerFactory}

import scala.util.Try

class MultiAttemptBackend(override val names: Set[String],
                          val files: Seq[File],
                          val resources: Seq[String],
                          val logAttempts: Boolean =
                            MultiAttemptBackend.defaultLogAttempts)
  extends PropertiesBackend(names) {
  val logger: Logger = LoggerFactory.getLogger(getClass)

  def tryLoad[A](zero: (Seq[SqlppError], Option[PropertiesBackend]),
                 in: Seq[A],
                 loadF: A => Either[SqlppError, PropertiesBackend]):
    (Seq[SqlppError], Option[PropertiesBackend]) = {

    in.foldLeft(zero) {
      //skip if we already found results
      case (res@(_, Some(_)), _) => res

      case ((accErrors, None), thisInput) => {
        loadF(thisInput) match {
          case Right(success) => (accErrors, Some(success))
          case Left(error) => (accErrors :+ error, None)
        }
      }
    }
  }

  private def loadFile(f: File): Either[SqlppError, PropertiesBackend] = {
    val fileChecks = Seq(
      CheckFile.checkExists,
      CheckFile.checkIsFile,
      CheckFile.setReadable(true))

    CheckFile.composeAll(fileChecks)(f).flatMap { _ =>
      val toCheck = new PropertiesFileBackend(names, f)

      toCheck.getValueSource
             .map(_ => toCheck)
    }
  }

  private def loadResource(resourceName: String):
    Either[SqlppError, PropertiesBackend] = {

    def f = Try {
      val toCheck = new PropertiesResourceBackend(names, resourceName)

      toCheck.getValueSource
             .map(_ => toCheck)
    }

    TryToEither(new MultiAttemptBackendError(_))(f) match {
      //flatten the error
      case Right(x) => x
      case Left(y) => Left(y)
    }
  }

  //TODO: may want to make this a lazy val so it isn't
  //rerun in every call to getValueSource or getPropertiesInputStream
  def getPropertiesBackend: Either[SqlppError, PropertiesBackend] = {
    val zero: (Seq[SqlppError], Option[PropertiesBackend]) = (Seq.empty, None)

    tryLoad(
      tryLoad(zero, files, loadFile),
      resources,
      loadResource) match {

      case (errors, Some(res)) => {
        if(logAttempts) {
          logger.debug(s"Encountered these errors " +
            s"before successfully finding backend with names " +
            s"$names: " + SqlppError.formatErrors(errors))
        }

        Right(res)
      }

      case (errors, None) => {
        Left(new MultiAttemptBackendError(
          s"Error loading backend " + toString +
            s" caused by: " + SqlppError.formatErrors(errors)))

      }
    }
  }

  override def getValueSource(): Either[SqlppError, ValueSource] = {
    getPropertiesBackend.flatMap(_.getValueSource)
  }

  override def getPropertiesInputStream: Either[SqlppError, InputStream] = {
    getPropertiesBackend.flatMap(_.getPropertiesInputStream)
  }

  override def toString: String = {
    getClass.getName + s"($names, $files, $resources)"
  }
}

object MultiAttemptBackend {
  class MultiAttemptBackendError(override val msg: String)
    extends PropertiesBackendError(msg) {
    def this(throwable: Throwable) {
      this(SqlppError.formatThrowableCause(throwable))
    }
  }

  val defaultLogAttempts: Boolean = true
}
