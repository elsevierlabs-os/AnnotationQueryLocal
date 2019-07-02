package com.elsevier.aql.query

import com.elsevier.aql.annotations.AQAnnotation

/**
 * Provide the ability to combine (union) Arrays of AQAnnotations. The input is 2 Arrays of AQAnnotations. The output is the union of these annotations.
 */
object Or {
  
  /*
   * left - Array of AQAnnotations
   * right - Array of AQAnnotations
   * limit - Number of AQAnnotations to return.
   */
  def apply(left: Array[AQAnnotation], right: Array[AQAnnotation], limit: Integer=0): Array[AQAnnotation] = {

    val s1 = left.toStream
    val s2 = right.toStream

    def mergeStreams(s1: Stream[AQAnnotation], s2: Stream[AQAnnotation]): Stream[AQAnnotation] = {
      if (s1.isEmpty) s2
      else if (s2.isEmpty) s1
      else if ((s1.head.docId < s2.head.docId) && (s1.head.startOffset < s2.head.startOffset) && (s1.head.endOffset < s2.head.endOffset)) s1.head #:: mergeStreams(s1.tail, s2)
      else s2.head #:: mergeStreams(s1, s2.tail)
    }

    if (limit > 0) {
      mergeStreams(s1, s2).toArray.sortBy { x => (x.startOffset, x.endOffset) }.slice(0, limit)
    } else {
      mergeStreams(s1, s2).toArray.sortBy { x => (x.startOffset, x.endOffset) }
    }
    
  }
  
}