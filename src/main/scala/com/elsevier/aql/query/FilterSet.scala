package com.elsevier.aql.query

import com.elsevier.aql.annotations.AQAnnotation

/**
 * Provide the ability to filter an Array of AQAnnotations based on the value in the annotSet field. 
 * You can provide a single value or list values to filter on. 
 * Returns the filtered Array of AQAnnotation.
 */
object FilterSet {
 
  /**
   * @param arr Array of AQAnnotations that will be filtered by the specified annotation set.
   * @param annotSet String to filter against the annotSet field in the array of AQAnnotations.
   * @param annotSetArr Array of Strings to filter against the annotSet field in the array of AQAnnotations. An OR will be applied to the Strings.  Only used if annotSet was not specified.
   * @param annotSetCompare Comparison operator to use for the annotSet field in the array of AQAnnotations.  Default is '='.  Possible values are '=' and '!='.
   * @param limit Number of AQAnnotations to return.
   * @param not Whether to negate the entire query.  Default is false.
   * @return Array[AQAnnotation]
  */
  def apply(arr: Array[AQAnnotation], annotSet: String="", annotSetArr: Array[String]=Array.empty[String], annotSetCompare: String="=", limit: Integer=0, not: Boolean=false): Array[AQAnnotation] = {
    
    var rs = arr.filter(x => if (annotSet != "") { if (annotSetCompare == "=") x.annotSet == annotSet else x.annotSet != annotSet } else true)
                .filter(x => if (!annotSetArr.isEmpty) { if (annotSetCompare == "=") annotSetArr contains x.annotSet else !(annotSetArr contains x.annotSet) } else true) 

    if (not) {
      rs = arr.diff(rs)
    } 

    if (limit > 0) {
      rs.slice(0,limit)
    } else {
      rs
    }
        
  }
  
}