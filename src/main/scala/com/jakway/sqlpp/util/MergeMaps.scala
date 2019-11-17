package com.jakway.sqlpp.util

object MergeMaps {
  type HandleDuplicatesF[K, V, ErrorType] = K => V => V => Either[ErrorType, (K, V)]

  /**
   *
   * @param handleDuplicates will be called with Key => (LeftValue, RightValue)
   * @param left
   * @param right
   * @tparam K
   * @tparam V
   * @tparam ErrorType
   * @return
   */
  def mergeMaps[K, V, ErrorType](
                      handleDuplicates: HandleDuplicatesF[K, V, ErrorType])
                     (left: Map[K, V],
                      right: Map[K, V]): Either[ErrorType, Map[K, V]] = {

    //fold each entry from the right map into the left map,
    //calling handleDuplicates as appropriate
    val start: Either[ErrorType, Map[K, V]] = Right(left)
    right.foldLeft(start) {
      case (eAcc, (thisKey, thisValue)) => eAcc.flatMap { acc =>
        acc.get(thisKey) match {
          case None => Right(acc.updated(thisKey, thisValue))

            //if the entry already exists,
            //call handleDuplicates and enter the result into the map
            //(if successful)
          case Some(existingValue) =>
            handleDuplicates(thisKey)(existingValue)(thisValue)
              .map(mergedValue => acc.updated(thisKey, mergedValue))
        }
      }
    }
  }
}
