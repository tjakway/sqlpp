package com.jakway.sqlpp.build

import java.io.{File, FileFilter}

import com.jakway.sqlpp.error.{CheckFile, SqlppError}
import com.jakway.sqlpp.template.ResourceLoaderConfig.StandardResourceLoaders.LoaderType
import com.jakway.sqlpp.template.TemplateEngine.ExtraTemplateOptions
import com.jakway.sqlpp.template._
import com.jakway.sqlpp.util.{MergeProperties, WithFormatter}
import org.slf4j.{Logger, LoggerFactory}

object Paths {
  import WithFormatter.WithPathSeparator
  val outputTemplate: String = WithPathSeparator("mk_targets%starget_template.xml.vtl")
  val sourceDir: File = new File(WithPathSeparator(
    "build_templates%ssrc%smain%sresources%smk_targets"))
  val outputDir: File = new File(WithPathSeparator("main%ssrc%smain%sresources%sgen"))

  val defaultsFilename: String = "defaults.xml"
}

object Main {
  import Paths._

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


  def run(): Either[SqlppError, Unit] = {
    for {
      _ <- checkSourceDir(sourceDir)
      _ <- checkOutputDir(outputDir)

      vsFiles = getValueSources(sourceDir)
      sources <- MergeDefaults.mergeDefaults(vsFiles)

      vsMap = sources.zip(vsFiles).map {
        case (source, sourceFile) => (source: ValueSource, getAssociatedOutput(sourceFile))
      }.toMap

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

    private def noDuplicates: MergeProperties.HandleDuplicatesF[SqlppError] = {
      key => left => right =>
        Left(new MergeDefaultsError(
          s"Unexpected duplicate properties for $key:" +
          s" ($left, $right)"))
    }

    def mergeDefaults(vsMap: Map[ValueSource, File]):
      Either[SqlppError, Map[ValueSource, File]] = {

      val empty: (Map[PropertySource, File], Map[ValueSource, File]) =
        (Map.empty, Map.empty)

      val (propertySources, rest) = vsMap.foldLeft(empty) {
        case ((accPropertySources, accRest), (key, value)) => {
          key match {
            case p@PropertySource(_) =>
              (accPropertySources.updated(p, value), accRest)
            case _ => (accPropertySources, accRest.updated(key, value))
          }
        }
      }

      val propertySourceSeq = propertySources.toSeq
      val propertySourceKeys = propertySourceSeq.map(_._1)
      val propertySourceValues = propertySourceSeq.map(_._2)

      for {
        mergedDefaults <-
          mergeDefaults()


      }

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
        val empty: (Option[(PropertySource, Int)], Map[File, PropertySource], Int) =
          (None, Map.empty, 0)

        //split the list of properties into defaults and others
        //
        //alternatively we could just find the defaults while
        //leaving them in the list while making sure to properly
        //merge duplicates that are equal
        val (defaultsOption, rest, _) = vsMap.foldLeft(empty) {
          case ((None, acc, pos), (thisSourceFile, prop)) => {
            if(thisSourceFile.getName == defaultsFilename) {
              (Some((prop, pos)), acc, pos + 1)
            } else {
              (None, acc.updated(thisSourceFile, prop), pos + 1)
            }
          }
          case ((x@Some(_), acc, pos), (thisSourceFile, prop)) => {
            (x, acc.updated(thisSourceFile, prop), pos + 1)
          }
        }

        defaultsOption match {
          case Some((defaults, defaultsPos)) => {
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

            //add defaults back to the list at the position it was found
            res.map { acc =>
              val (left, right) = acc.splitAt(defaultsPos)
              left ++ Seq(defaults) ++ right
            }
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
