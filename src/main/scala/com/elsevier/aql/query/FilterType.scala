package com.elsevier.aql.query

import com.elsevier.aql.annotations.AQAnnotation

/**
 * Provide the ability to filter an Array of AQAnnotations based on the value in the annotType field. 
 * You can provide a single value or list values to filter on. 
 * Returns the filtered Array of AQAnnotation.
 */
object FilterType {

  /*
   * arr - Array of AQAnnotations that will be filtered by the specified annotation type.
   * annotType - String to filter against the annotType field in the array of AQAnnotations.
   * annotTypeArr - Array of Strings to filter against the annotType field in the array of AQAnnotations. An OR will be applied to the Strings.  Only used if annotType was not specified.
   * annotTypeCompare - Comparison operator to use for the annotType field in the array of AQAnnotations.  Default is '='.  Possible values are '=' and '!='.
   * limit - Number of AQAnnotations to return.
   * not - Whether to negate the entire query.  Default is false.
  */
  def apply(arr: Array[AQAnnotation], annotType: String="", annotTypeArr: Array[String]=Array.empty[String], annotTypeCompare: String="=", limit: Integer=0, not: Boolean=false): Array[AQAnnotation] = {
    
    var rs = arr.filter(x => if (annotType != "") { if (annotTypeCompare == "=") x.annotType == annotType else x.annotType != annotType } else true)
                .filter(x => if (!annotTypeArr.isEmpty) { if (annotTypeCompare == "=") annotTypeArr contains x.annotType else !(annotTypeArr contains x.annotType) } else true) 

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