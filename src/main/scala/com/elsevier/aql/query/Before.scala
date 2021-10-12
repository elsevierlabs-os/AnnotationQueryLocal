package com.elsevier.aql.query

import com.elsevier.aql.annotations.AQAnnotation

/**
 * Provide the ability to find annotations that are before another annotation. The input is 2 Arrays of AQAnnotations. We will call them A and B. 
 * The purpose is to find those annotations in A that are before B. 
 * What that means is the end offset for an annotation from A must be before (or equal to) the start offset from an annotation in B. 
 * We ultimately return the A annotations that meet this criteria. 
 * A distance operator can also be optionally specified. 
 * This would require an A annotation (endOffset) to occur n characters (or less) before the B annotation (startOffset). 
 * There is also the option of negating the query (think Not Before) so that we return only A where it is not before B. 
 */
object Before {

  /**
   * @param left Array of AQAnnotations, the ones we will return if they are before AQAnnotations from 'right'.
   * @param right Array of AQAnnotations, the ones we are looking to see if are after AQAnnotations from 'left'.
   * @param dist  Number of characters  where endOffset from 'left' must occur before startOffset from 'right'. Default is Int.MaxValue.
   * @param limit Number of AQAnnotations to return.
   * @param not Whether to negate the entire query (think NOT before).  Default is false.
   * @return Array[AQAnnotation]
  */
  def apply(left: Array[AQAnnotation], right: Array[AQAnnotation], dist: Integer=Int.MaxValue, limit: Integer=0, not: Boolean=false): Array[AQAnnotation] = {
 
    var rs = BinaryOp(left, right, 
                      (l, r) => r.startOffset >= l.endOffset &&
                                r.startOffset - l.endOffset < dist && 
                                !((l.annotSet == r.annotSet) && (l.annotType == r.annotType) && (l.startOffset == r.startOffset) && (l.endOffset == r.endOffset)))
                                
    if (not) {
      rs = left.diff(rs)
    } 
    
    if (limit > 0) {
      rs.slice(0,limit)
    } else {
      rs
    }      
    
  }
  
}