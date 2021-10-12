package com.elsevier.aql.query

import com.elsevier.aql.annotations.AQAnnotation

/**
 * Provide the ability to find annotations that are in the same document. The input is 2 Arrays of AQAnnotations. We will call them A and B. 
 * The purpose is to find those annotations in A and B that are in the same document.
 */
object And {
  
  /**
   * @param left Array of AQAnnotations
   * @param right Array of AQAnnotations.
   * @param limit Number of AQAnnotations to return.
   * @param not Think and NOT (only return annotations from A that are not in B).  Default is false.
   * @param leftOnly - Return only the left or the left and right.  The default is to only return the left.
   * @return Array[AQAnnotation]
  */
  def apply(left: Array[AQAnnotation], right: Array[AQAnnotation], limit: Integer=0, not: Boolean=false, leftOnly: Boolean=true): Array[AQAnnotation] = {

    var rs = Array.empty[AQAnnotation]
    
    if (leftOnly) {
      rs = left
      if (not) {
        rs = left.diff(rs)
      } 
    } else {
      // Essentially a Cartesian join on docId removing duplicates
      rs = (left ++ right).distinct.sortBy { x => (x.startOffset, x.endOffset) }
    }
    
    if (limit > 0) {
      rs.slice(0,limit)
    } else {
      rs
    }   
    
  } 
    
}