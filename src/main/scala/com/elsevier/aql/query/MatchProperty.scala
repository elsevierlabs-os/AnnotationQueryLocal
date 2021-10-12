package com.elsevier.aql.query

import com.elsevier.aql.annotations.AQAnnotation

/**
 * Provide the ability to find annotations (looking at their property) that are in the same document. The input is 2 Arrays of AQAnnotations. We will call them A and B. 
 * The purpose is to find those annotations in A that are in the same document as B and also match values on the specified property.
 */
object MatchProperty {

  /**
   * @param left Array of AQAnnotations, the ones we will return if they match AQAnnotations from 'right'.
   * @param right Array of AQAnnotations the ones we are looking to see if they match AQAnnotations from 'left'.
   * @param name Name of the property to match.
   * @param limit Number of AQAnnotations to return.
   * @param not Whether to negate the entire query (think NOT contains).  Default is false.
   * @return Array[AQAnnotation]
  */
  def apply(left: Array[AQAnnotation], right: Array[AQAnnotation], name: String, not: Boolean=false, limit: Integer=0): Array[AQAnnotation] = {
    
    var rs = BinaryOp(left, right, (l, r) => l.properties.getOrElse(Map.empty).getOrElse(name, "") == r.properties.getOrElse(Map.empty).getOrElse(name, "") &&
                                             l.properties.getOrElse(Map.empty).get(name) != None &&
                                             r.properties.getOrElse(Map.empty).get(name) != None)
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