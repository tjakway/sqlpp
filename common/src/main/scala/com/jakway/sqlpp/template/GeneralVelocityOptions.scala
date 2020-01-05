package com.jakway.sqlpp.template

import java.nio.charset.StandardCharsets

import com.jakway.sqlpp.template.TemplateEngine.PropertyMap

object GeneralVelocityOptions {
  def apply(inputEncoding: String = defaultEncoding,
            strict: Boolean = true,
            verbose: Boolean = true): PropertyMap = {
    import org.apache.velocity.runtime.{RuntimeConstants => VelocityConstants}

    val basicOptions: Seq[(String, String)] = Seq(
      VelocityConstants.INPUT_ENCODING -> inputEncoding
    )

    val whenStrict = Seq(
      //strict settings to catch bugs
      VelocityConstants.RUNTIME_REFERENCES_STRICT -> "true",
      VelocityConstants.STRICT_MATH -> "true",
      VelocityConstants.VM_ARGUMENTS_STRICT -> "true",

      VelocityConstants.SKIP_INVALID_ITERATOR -> "false"
    )

    val whenVerbose = Seq(
      VelocityConstants.RESOURCE_MANAGER_LOGWHENFOUND -> "true"
    )

    //apply changes, using basicOptions as a starting point
    mapBranch(whenVerbose)(verbose,
      mapBranch(whenStrict)(strict, basicOptions.toMap))
  }

  /**
   * update map with different sets of values depending on a boolean
   * @param ifTrue
   * @param ifFalse
   * @param branchCondition
   * @param start
   * @tparam A
   * @tparam B
   * @return
   */
  private def mapBranch[A, B](ifTrue: Seq[(A, B)],
                              ifFalse: Seq[(A, B)] = Seq())
                             (branchCondition: Boolean,
                              start: Map[A, B]): Map[A, B] = {

    def updateMap(updateWith: Seq[(A, B)]): Map[A, B] = {
      updateWith.foldLeft(start) {
        case (acc, (key, value)) =>
          acc.updated(key, value)
      }
    }

    if(branchCondition) {
      updateMap(ifTrue)
    } else {
      updateMap(ifFalse)
    }
  }


  val defaultEncoding: String = StandardCharsets.UTF_8.displayName()
}

