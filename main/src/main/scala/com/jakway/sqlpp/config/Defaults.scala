package com.jakway.sqlpp.config

import java.io.{File, InputStream}
import java.nio.charset.{Charset, StandardCharsets}

import com.jakway.sqlpp.config.backend.BuiltinBackends
import com.jakway.sqlpp.config.checked.{Config, TemplateStringInfo}
import com.jakway.sqlpp.config.output.OutputTarget
import com.jakway.sqlpp.template.ResourceLoaderConfig.StandardResourceLoaders
import com.jakway.sqlpp.template.ResourceLoaderConfig.StandardResourceLoaders.LoaderType
import com.jakway.sqlpp.template.TemplateEngine.ExtraTemplateOptions
import com.jakway.sqlpp.template.backend.Backend

object Defaults {
  val defaultEncoding: Charset = StandardCharsets.UTF_8
  val allowOverwrite: Boolean = false
  lazy val extraTemplateOptions: ExtraTemplateOptions =
    ExtraTemplateOptions(
      TemplateStringInfo.defaultStringResourceRepositoryName,
      //add the current directory to list of places to search
      Seq(new File(".")),
      Seq())

  val noSourceImpliesStdin: Boolean = true

  object VerbosityLevel {
    import com.jakway.sqlpp.config.{VerbosityLevel => VL}
    val default: VL = VL.Standard
  }

  object DataDir {
    val createProfileDirOption: CreateProfileDirOption =
      CreateProfileDirOption.CreateDefaultProfileDir
    val dataDirVarName: String = "SQLPP_DIR"
    val xdgConfigSubdirName: String = "sqlpp"
    val homeSubdirName: String = ".sqlpp"
    val deleteOnFailure: Boolean = false

    object Options {
      val defaultRejectNonXmlFiles: Boolean = true
    }
  }

  object TemplateStringInfo {
    val defaultTemplateSourceKey: String = "__SQLPP_TEMPLATE_SOURCE_KEY"
    val defaultStringResourceRepositoryName: String = "__SQLPP_STRING_RESOURCE_REPOSITORY"

    val default: TemplateStringInfo = checked.TemplateStringInfo()
  }

  object Config {
    val defaultResourceLoaderTypes: Set[LoaderType] =
      Set(
        StandardResourceLoaders.StringLoader,
        StandardResourceLoaders.ClassLoader,
        StandardResourceLoaders.FileLoader
      )

    def default(source: InputStream): Config = {
      checked.Config(
        source,
        Map.empty,
        Defaults.defaultEncoding.displayName(),
        Defaults.defaultEncoding.displayName(),
        defaultResourceLoaderTypes,
        extraTemplateOptions,
        TemplateStringInfo.default,
        DataDir.createProfileDirOption,
        DataDir.deleteOnFailure,
        //upcast them to backends
        BuiltinBackends.all.map(x => x: Backend)
      )
    }
  }

  object UncheckedConfig {
    val defaultNoCreateProfileDir: Boolean = false
  }
}
