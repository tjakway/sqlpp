package com.jakway.sqlpp.config

import java.io.File

import com.jakway.sqlpp.Backend
import com.jakway.sqlpp.config.error.OutputTargetErrors
import com.jakway.sqlpp.error.{CheckFile, SqlppError}





case class OutputTarget(backend: Backend,
                        dest: File)
