package com.jakway.sqlpp.build

import java.io.{File, FileFilter}

import com.jakway.sqlpp.error.{CheckFile, SqlppError}
import com.jakway.sqlpp.template.ResourceLoaderConfig.StandardResourceLoaders.LoaderType
import com.jakway.sqlpp.template.TemplateEngine.ExtraTemplateOptions
import com.jakway.sqlpp.template.{GeneralVelocityOptions, PropertySource, ResourceLoaderConfig, TemplateEngine, ValueSource}
import com.jakway.sqlpp.util.MergeMaps.HandleDuplicatesF
import com.jakway.sqlpp.util.{MergeProperties, WithFormatter}
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

  val outputTemplate: String = WithFormatter("mk_targets%ntarget_template.xml.vtl")
  val sourceDir: File = new File(WithFormatter(
    "build_templates%nsrc%nmain%nresources%nmk_targets"))
  val outputDir: File = new File(WithFormatter("main%nsrc%nmain%nresources%ngen"))

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

  object MergeDefaults {
    val logger: Logger = LoggerFactory.getLogger(getClass)

    class MergeDefaultsError(override val msg: String)
      extends SqlppError(msg)

    object MergeDefaultsError {
      def apply(throwable: Throwable): MergeDefaultsError =
        new MergeDefaultsError(SqlppError.formatThrowable(throwable))
    }


    private def preferRightSide: MergeProperties.HandleDuplicatesF[SqlppError] = {
      key => left => right =>
        logger.debug(s"Duplicate properties for $key:" +
          s" ($left, $right), preferring right side")

        Right((key, right))
    }


    def mergeDefaults(propertySources: Seq[File]):
      Either[SqlppError, Seq[PropertySource]] = {



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

        //split the list of properties into defaults and others
        //
        //alternatively we could just find the defaults while
        //leaving them in the list while making sure to properly
        //merge duplicates that are equal
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
                  val eMergedProperties = MergeProperties.merge(
                    defaults.prop, thisSource.prop,
                    preferRightSide,
                    MergeDefaultsError.apply)

                  eMergedProperties.map { mergedProperties =>
                    acc :+ PropertySource(mergedProperties)
                  }
                }
              }
            }
            //add defaults back to the list
            res.map(acc => acc :+ defaults)
          }
          case None => Left(new MergeDefaultsError(
            s"Expected to find a template named $defaultsFilename " +
              s"containing default properties"))
        }

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
