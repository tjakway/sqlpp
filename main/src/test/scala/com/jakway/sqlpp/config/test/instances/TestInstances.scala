package com.jakway.sqlpp.config.test.instances

import com.jakway.sqlpp.config.test.TestResources
import com.jakway.sqlpp.config.test.template.TemplateEngineTest


class SimpleXMLTest
  extends TemplateEngineTest(
    TestResources.Tests.simple, TestResources.Tests.simple)

class EmptyXMLTest
  extends TemplateEngineTest(
    TestResources.Tests.empty, TestResources.Tests.empty)

class AllVariablesXMLTest
  extends TemplateEngineTest(
    TestResources.Tests.allVariables, TestResources.Tests.allVariables)
