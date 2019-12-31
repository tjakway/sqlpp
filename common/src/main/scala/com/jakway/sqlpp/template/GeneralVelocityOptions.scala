package com.jakway.sqlpp.template

import java.nio.charset.StandardCharsets

import com.jakway.sqlpp.template.TemplateEngine.PropertyMap

object GeneralVelocityOptions {
  def apply(inputEncoding: String = defaultEncoding): PropertyMap = {
    import org.apache.velocity.runtime.{RuntimeConstants => VelocityConstants}
    Map(
      VelocityConstants.INPUT_ENCODING -> inputEncoding,

      //strict settings to catch bugs
      VelocityConstants.RUNTIME_REFERENCES_STRICT -> "true",
      VelocityConstants.STRICT_MATH -> "true",
      VelocityConstants.VM_ARGUMENTS_STRICT -> "true",

      VelocityConstants.SKIP_INVALID_ITERATOR -> "false",

      VelocityConstants.RESOURCE_MANAGER_LOGWHENFOUND -> "true",
    )
  }

  val defaultEncoding: String = StandardCharsets.UTF_8.displayName()
}

