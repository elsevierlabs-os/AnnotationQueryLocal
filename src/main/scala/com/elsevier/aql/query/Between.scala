package com.elsevier.aql.query

import com.elsevier.aql.annotations.AQAnnotation

/**
 * Provide the ability to find annotations that are before one annotation and after another. The input is 3 Arrays of AQAnnotations. We will call them A, B and C. 
 * The purpose is to find those annotations in A that are before B and after C. 
 * What that means is the end offset for an annotation from A must be before (or equal to) the start offset from an annotation in B and the start offset for A be after (or equal to) the end offset from C. 
 * We ultimately return the A annotations that meet this criteria. 
 * A distance operator can also be optionally specified. 
 * This would require an A annotation (endOffset) to occur n characters (or less) before the B annotation (startOffset) and would require the A annotation (startOffset) to occur n characters (or less) after the C annotation (endOffset) . 
 * There is also the option of negating the query (think Not Between) so that we return only A where it is not before B nor after C.
 */
object Between {

  /*
   * middle - Array of AQAnnotations, the ones we will return if they are between AQAnnotations from 'left' and AQAnnotations from 'right.
   * left - Array of AQAnnotations, the ones we are looking to see if they are before AQAnnotations from 'middle'.
   * right - Array of AQAnnotations, the ones we are looking to see if they are after AQAnnotations from 'middle'.
   * dist  - Number of characters  where startOffset from 'middle' must occur after endOffset of 'left' or endOffset from 'middle' must occur before startOffset of 'right'
   * limit - Number of AQAnnotations to return.
   * not - Whether to negate the entire query (think NOT between).  Default is false.
  */
  def apply(middle: Array[AQAnnotation], left: Array[AQAnnotation], right: Array[AQAnnotation], dist: Integer=Int.MaxValue, limit: Integer=0, not: Boolean=false): Array[AQAnnotation] = {

    val intermediate = BinaryOp(middle, right, 
                                (l, r) => r.startOffset >= l.endOffset &&
                                          r.startOffset - l.endOffset < dist && 
                                          !((l.annotSet == r.annotSet) && (l.annotType == r.annotType) && (l.startOffset == r.startOffset) && (l.endOffset == r.endOffset)))
                                          
    var rs = BinaryOp(intermediate,left,
                      (l,r) => l.startOffset >= r.endOffset &&
                               l.startOffset - r.endOffset < dist &&
                               !((l.annotSet == r.annotSet) && (l.annotType == r.annotType) && (l.startOffset == r.startOffset) && (l.endOffset == r.endOffset)))   
                                          
    if (not) {                                      
      rs = middle.diff(rs)
    } 
    
    if (limit > 0) {
      rs.slice(0,limit)
    } else {
      rs
    }    
    
  }
  
}

