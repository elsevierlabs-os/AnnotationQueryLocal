package com.elsevier.aql.query

import scala.collection.mutable.ListBuffer
import com.elsevier.aql.annotations.AQAnnotation

/**
 * Helper object to simulate joins between two Arrays of AQAnnotations.
 */
object BinaryOp {

   /**
    * @param arr1 - Array of AQAnnotations 
    * @param arr2 - Array of AQAnnotations
    * @param condition - Condition we want to apply
    * @return Array[AQAnnotation]
    */
   def apply(arr1: Array[AQAnnotation], arr2: Array[AQAnnotation], condition: (AQAnnotation, AQAnnotation) => Boolean): Array[AQAnnotation] = {
      
      val results: ListBuffer[AQAnnotation] = new ListBuffer[AQAnnotation]()
      var continue = true
      var index = 0
      val it1 = arr1.toIterator
      
      while (it1.hasNext) {
        val a1 = it1.next()
        continue = true
        val it2 = arr2.toIterator 
        while (it2.hasNext && continue) {
          val a2 = it2.next()
          if (condition(a1, a2)) {
            results += a1
            continue = false
          } 
        }
      }
      
      results.toArray
  }

}