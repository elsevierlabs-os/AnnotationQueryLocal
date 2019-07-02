package com.elsevier.aql.query

import com.elsevier.aql.annotations.AQAnnotation

/**
 * Provide the ability to filter an Array of AQAnnotations based on the value in the annotSet field. 
 * You can provide a single value or list values to filter on. 
 * Returns the filtered Array of AQAnnotation.
 */
object FilterSet {
 
  /*
   * arr - Array of AQAnnotations that will be filtered by the specified annotation set.
   * annotSet - String to filter against the annotSet field in the array of AQAnnotations.
   * annotSetArr - Array of Strings to filter against the annotSet field in the array of AQAnnotations. An OR will be applied to the Strings.  Only used if annotSet was not specified.
   * annotSetCompare - Comparison operator to use for the annotSet field in the array of AQAnnotations.  Default is '='.  Possible values are '=' and '!='.
   * limit - Number of AQAnnotations to return.
   * not - Whether to negate the entire query.  Default is false.
  */
  def apply(arr: Array[AQAnnotation], annotSet: String="", annotSetArr: Array[String]=Array.empty[String], annotSetCompare: String="=", limit: Integer=0, not: Boolean=false): Array[AQAnnotation] = {
    
    val lcAnnotSet = if (annotSet != "")  annotSet.toLowerCase else null
    val lcAnnotSetArr = if (!annotSetArr.isEmpty) annotSetArr.map(_.toLowerCase) else null
    var rs = arr.filter(x => if (lcAnnotSet != null) { if (annotSetCompare == "=") x.annotSet == lcAnnotSet else x.annotSet != lcAnnotSet } else true)
                .filter(x => if (lcAnnotSetArr != null) { if (annotSetCompare == "=") lcAnnotSetArr contains x.annotSet else !(lcAnnotSetArr contains x.annotSet) } else true) 

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