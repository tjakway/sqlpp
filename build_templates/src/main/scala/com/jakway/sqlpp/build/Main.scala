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
}

object Main {
  import Paths._

  val logger: Logger = LoggerFactory.getLogger(getClass)

  /**
   * currently we don't use the string repository,
   * but we need to provide a name anyway
   */
  val stringRepositoryName: String = "build_templates_repo"

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
    val mergeDefaults = new MergeDefaults()
    for {
      _ <- checkSourceDir(sourceDir)
      _ <- checkOutputDir(outputDir)

      vsFiles = getValueSources(sourceDir)
      sources <- mergeDefaults.mergeDefaults(vsFiles)

      vsMap = sources.zip(vsFiles).map {
        case (source, sourceFile) => (source: ValueSource, getAssociatedOutput(sourceFile))
      }.toMap

      templateEngine <- getTemplateEngine
    } yield {
      templateEngine.multiApplyFiles(outputTemplate, vsMap)
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
      _ <- CheckFile.checkWritable(out)
    } yield {}
  }

  private val resourceLoaders: Set[LoaderType] = Set {
    ResourceLoaderConfig.StandardResourceLoaders.ClassLoader
  }

  private def getTemplateEngine: Either[SqlppError, TemplateEngine] = {
    TemplateEngine.apply(
      GeneralVelocityOptions.defaultEncoding)(resourceLoaders
      )(ExtraTemplateOptions(stringRepositoryName, Seq(), Seq())
      )(GeneralVelocityOptions())
  }
}
