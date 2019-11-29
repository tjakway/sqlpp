package com.jakway.sqlpp.build

import java.io.{File, FileFilter}

import com.jakway.sqlpp.error.{CheckFile, SqlppError}
import com.jakway.sqlpp.template.ResourceLoaderConfig.StandardResourceLoaders.LoaderType
import com.jakway.sqlpp.template.TemplateEngine.ExtraTemplateOptions
import com.jakway.sqlpp.template.{GeneralVelocityOptions, PropertySource, ResourceLoaderConfig, TemplateEngine, ValueSource}
import com.jakway.sqlpp.util.MergeMaps.HandleDuplicatesF
import com.jakway.sqlpp.util.MergeProperties
import org.slf4j.{Logger, LoggerFactory}

import scala.util.Success

object Main {
  val logger: Logger = LoggerFactory.getLogger(getClass)

  def main(args: Array[String]): Unit = {
    run() match {
      case Right(_) => logger.info("Done.")
      case Left(e) => {
        logger.error(e.toString)
        System.exit(1)
      }
    }
  }

  val outputTemplate: String = "mk_targets/target_template.xml.vtl"
  val sourceDir: File = new File("build_templates/src/main/resources/mk_targets")
  val outputDir: File = new File("main/src/main/resources/")

  val defaultsFilename: String = "defaults.xml"

  def run(): Either[SqlppError, Unit] = {
    for {
      _ <- checkSourceDir(sourceDir)
      _ <- checkOutputDir(outputDir)

      vsFiles = getValueSources(sourceDir)
      vsMap <- valueSourcesToMap(vsFiles)

      templateEngine <- getTemplateEngine
    } yield {
      templateEngine.multiApplyFiles(outputTemplate, vsMap)
    }
  }

  class MergeDefaultsError(override val msg: String)
    extends SqlppError(msg)

  private def mergeDefaults(propertySources: Seq[File]):
    Either[SqlppError, Seq[PropertySource]] = {

    def handleDuplicates: MergeProperties.HandleDuplicatesF[SqlppError] = {
      key => left => right =>
        Left(new MergeDefaultsError(s"Unexpected duplicate entries for $key: " +
          s"($left, $right)"))
    }


    val eVSMap: Either[SqlppError, Map[File, PropertySource]] =
    propertySources.foldLeft(
      Right(Map.empty): Either[SqlppError, Map[File, PropertySource]]) {
      case (eAcc, thisSourceFile) => eAcc.flatMap { acc =>
        PropertySource.fromXML(thisSourceFile)
          .map(acc.updated(thisSourceFile, _))
      }
    }

    eVSMap.flatMap { vsMap =>
      val empty: (Option[PropertySource], Map[File, PropertySource]) =
        (None, Map.empty)

      val (defaultsOption, rest) = vsMap.foldLeft(empty) {
        case ((None, acc), (thisSourceFile, prop)) => {
          if(thisSourceFile.getName == defaultsFilename) {
            (Some(prop), acc)
          } else {
            (None, acc.updated(thisSourceFile, prop))
          }
        }
        case ((x@Some(_), acc), (thisSourceFile, prop)) => {
          (x, acc.updated(thisSourceFile, prop))
        }
      }

      defaultsOption match {
        case Some(defaults) => {
          val res = {
            rest.foldLeft(
              Right(Seq.empty): Either[SqlppError, Seq[PropertySource]]) {
              case (eAcc, (thisSourceFile, thisSource)) => eAcc.flatMap { acc =>
                MergeProperties.merge()

                thisSource.prop
              }
            }
          }
        }
        case None => Left(new MergeDefaultsError(
          s"Expected to find a template named $defaultsFilename " +
            s"containing default properties"))
      }

    }
  }

  private def getValueSources(srcDir: File): Seq[File] = {
    val filter: FileFilter = new FileFilter {
      override def accept(file: File): Boolean = {
        file.isFile && file.getAbsolutePath.endsWith(".xml")
      }
    }

    srcDir.listFiles(filter)
  }

  private def getAssociatedOutput(input: File): File = {
    new File(outputDir, input.getName)
  }

  private def valueSourcesToMap(srcs: Seq[File]):
    Either[SqlppError, Map[ValueSource, File]] = {

    val empty: Either[SqlppError, Map[ValueSource, File]] = Right(Map.empty)
    srcs.foldLeft(empty) {
      case (eAcc, thisSource) => eAcc.flatMap { acc =>
        val output = getAssociatedOutput(thisSource)

        PropertySource.fromXML(thisSource).map { vs =>
          acc.updated(vs, output)
        }
      }
    }
  }

  private def checkSourceDir(src: File): Either[SqlppError, Unit] = {
    for {
      _ <- CheckFile.checkExists(src)
      _ <- CheckFile.checkIsDirectory(src)
      _ <- CheckFile.checkReadable(src)
    } yield {}
  }

  private def checkOutputDir(out: File): Either[SqlppError, Unit] = {
    for {
      _ <- CheckFile.checkExists(out)
      _ <- CheckFile.checkIsDirectory(out)
      _ <- CheckFile.checkWriteable(out)
    } yield {}
  }

  private val resourceLoaders: Set[LoaderType] = Set {
    ResourceLoaderConfig.StandardResourceLoaders.ClassLoader
  }

  private def getTemplateEngine: Either[SqlppError, TemplateEngine] = {
    TemplateEngine.apply(
      GeneralVelocityOptions.defaultEncoding)(resourceLoaders
      )(ExtraTemplateOptions(Seq(), Seq())
      )(GeneralVelocityOptions())
  }
}
