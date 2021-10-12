package com.elsevier.aql.query

import com.elsevier.aql.annotations.AQAnnotation

/**
 * Provide the ability to find annotations that are contained by another annotation. The input is 2 Arrays of AQAnnotations. We will call them A and B. 
 * The purpose is to find those annotations in A that are contained in B. 
 * What that means is the start/end offset for an annotation from A must be contained by the start/end offset from an annotation in B. 
 * The start/end offsets are inclusive.  We ultimately return the contained annotations (A) that meet this criteria. 
 * There is also the option of negating the query (think Not Contains) so that we return only A where it is not contained in B.
 */
object ContainedIn {

  /**
   * @param left Array of AQAnnotations, the ones we will return if they are contained in AQAnnotations from 'right'.
   * @param right Array of AQAnnotations, the ones we are looking to see if they contain AQAnnotations from 'left'.
   * @param limit Number of AQAnnotations to return.
   * @param not Whether to negate the entire query (think NOT contained in).  Default is false.
   * @return Array[AQAnnotation]
  */
  def apply(left: Array[AQAnnotation], right: Array[AQAnnotation], limit: Integer=0, not: Boolean=false): Array[AQAnnotation] = {

    var rs = BinaryOp(left, right, 
                      (l, r) => l.startOffset >= r.startOffset &&
                                l.endOffset <= r.endOffset && 
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