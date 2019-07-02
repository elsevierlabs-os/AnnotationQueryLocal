package com.elsevier.aql.query

import com.elsevier.aql.annotations.AQAnnotation

/**
 * Provide the ability to find annotations that are after another annotation. The input is 2 Arrays of AQAnnotations. We will call them A and B. 
 * The purpose is to find those annotations in A that are after B. 
 * What that means is the start offset for an annotation from A must be after (or equal to) the end offset from an annotation in B. 
 * We ultimately return the A annotations that meet this criteria. 
 * A distance operator can also be optionally specified. 
 * This would require an A annotation (startOffset) to occur n characters (or less) after the B annotation (endOffset). 
 * There is also the option of negating the query (think Not After) so that we return only A where it is not after B.
 */
object After {

  /*
   * left - Array of AQAnnotations, the ones we will return if they are after AQAnnotations from 'right'.
   * right - Array of AQAnnotations, the ones we are looking to see if are before AQAnnotations from 'left'.
   * dist  - Number of characters  where startOffset from 'left' must occur after endOffset from 'right'. Default is Int.MaxValue.
   * limit - Number of AQAnnotations to return.
   * not - Whether to negate the entire query (think NOT after).  Default is false.
  */
  
  def apply(left: Array[AQAnnotation], right: Array[AQAnnotation], dist: Integer=Int.MaxValue, limit: Integer=0, not: Boolean=false): Array[AQAnnotation] = {

    var rs = BinaryOp(left, right, 
                      (l, r) => l.startOffset >= r.endOffset &&
                                l.startOffset - r.endOffset < dist && 
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