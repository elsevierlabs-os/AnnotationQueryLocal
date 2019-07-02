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
    
    val lcAnnotType = if (annotType != "")  annotType.toLowerCase else null
    val lcAnnotTypeArr = if (!annotTypeArr.isEmpty) annotTypeArr.map(_.toLowerCase) else null
    var rs = arr.filter(x => if (lcAnnotType != null) { if (annotTypeCompare == "=") x.annotType == lcAnnotType else x.annotType != lcAnnotType } else true)
                .filter(x => if (lcAnnotTypeArr != null) { if (annotTypeCompare == "=") lcAnnotTypeArr contains x.annotType else !(lcAnnotTypeArr contains x.annotType) } else true) 

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