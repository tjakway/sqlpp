package com.jakway.sqlpp.config.test.framework

import org.scalatest.{BeforeAndAfter, Suite}

trait WithTempDirBeforeAndAfter
  extends WithTempDir
    with BeforeAndAfter { this: Suite =>

  before {
    mkTempDir()
  }

  after {
    rmTempDir()
  }
}
