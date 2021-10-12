package com.elsevier.aql.query

import com.elsevier.aql.annotations.AQAnnotation

/**
 * Provide the ability to find annotations that contain another annotation. The input is 2 Arrays of AQAnnotations. We will call them A and B. 
 * The purpose is to find those annotations in A that contain B. What that means is the start/end offset for an annotation from A must contain the start/end offset from an annotation in B. 
 * The start/end offsets are inclusive.  We ultimately return the container annotations (A) that meet this criteria. 
 * We also deduplicate the A annotations as there could be many annotations from B that could be contained by an annotation in A but it only makes sense to return the unique container annotations. 
 * There is also the option of negating the query (think Not Contains) so that we return only A where it does not contain B.
 */
object Contains { 
  
  /**
   * @param left Array of AQAnnotations, the ones we will return if they contain AQAnnotations from 'right'.
   * @param right Array of AQAnnotations, the ones we are looking to see if they occur in the AQAnnotations from 'left'.
   * @param limit Number of AQAnnotations to return.
   * @param not Whether to negate the entire query (think NOT contains).  Default is false.
   * @return Array[AQAnnotation]
  */
  def apply(left: Array[AQAnnotation], right: Array[AQAnnotation], limit: Integer=0, not: Boolean=false): Array[AQAnnotation] = {
 
    var rs = BinaryOp(left, right,    
                      (l, r) => l.startOffset <= r.startOffset &&
                                l.endOffset >= r.endOffset && 
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