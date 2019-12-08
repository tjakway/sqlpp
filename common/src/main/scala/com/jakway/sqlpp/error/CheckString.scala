package com.jakway.sqlpp.error

object CheckString {
  def isEmpty: String => Boolean = { s =>
    s.trim.isEmpty || s.forall(_.isWhitespace)
  }

  type StringCheckF = (String => SqlppError) =>
                        String =>
                        Either[SqlppError, Unit]

  def checkNonEmpty: StringCheckF = { onError => str =>
    if(isEmpty(str)) {
      Left(onError("Expected nonempty string"))
    } else {
      Right({})
    }
  }
}
