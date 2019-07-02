package com.elsevier.aql.query

import scala.collection.mutable.ListBuffer
import com.elsevier.aql.annotations.AQAnnotation

/**
 * Provide the ability to find annotations that are before another annotation. The input is 2 Arrays of AQAnnotations. We will call them A and B. 
 * The purpose is to find those annotations in A that are before B. 
 * What that means is the end offset for an annotation from A must be before (or equal to) the start offset from an annotation in B. 
 * We ultimately return the annotations that meet this criteria. 
 * Unlike the Before function, we adjust the returned annotation a bit. 
 * For example, we set the annotType to "seq" and we use the A startOffset and the B endOffset. 
 * A distance operator can also be optionally specified. This would require an A annotation (endOffset) to occur n characters (or less) before the B annotation (startOffset).
 */
object Sequence {
 
  /*
   * left - Array of AQAnnotations, the ones we will return if they are before AQAnnotations from 'right'.
   * right - Array of AQAnnotations, the ones we are looking to see if are after AQAnnotations from 'left'.
   * dist  - Number of characters  where endOffset from 'left' must occur before startOffset from 'right'. Default is Int.MaxValue.
   * limit - Number of AQAnnotations to return.
  */
  def apply(left: Array[AQAnnotation], right: Array[AQAnnotation], dist: Integer=Int.MaxValue, limit: Integer=0): Array[AQAnnotation] = {
    
    val results: ListBuffer[AQAnnotation] = new ListBuffer[AQAnnotation]()
    var curAnn: AQAnnotation = null
    var index = 0
    val it1 = left.toIterator
      
    while (it1.hasNext) {
      val a1 = it1.next()
      val it2 = right.toIterator 
      while (it2.hasNext ) {
        val a2 = it2.next()
        if (a2.startOffset >= a1.endOffset && a2.startOffset - a1.endOffset < dist && !((a1.annotSet == a2.annotSet) && (a1.annotType == a2.annotType) && (a1.startOffset == a2.startOffset) && (a1.endOffset == a2.endOffset))) {
          results +=  AQAnnotation(a1.docId,a1.annotSet,"seq",a1.startOffset,a2.endOffset,a1.annotId,None)
        } 
      }
    }
    
    if (limit > 0) {
      results.toArray.distinct.slice(0,limit)
    } else {
      results.toArray.distinct
    }

  } 
  
}