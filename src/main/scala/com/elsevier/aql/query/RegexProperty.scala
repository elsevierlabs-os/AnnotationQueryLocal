package com.elsevier.aql.query

import com.elsevier.aql.annotations.AQAnnotation

/**
 * Provide the ability to filter an Array of AQAnnotations based on the regex applied to the specified property value in the map.  
 * Returns the filtered Array of AQAnnotation.
 */
object RegexProperty {

  /*
   * arr -  Array of AQAnnotations that will be filtered by the specified property name and regex expression.
   * name - Name of the property to filter.
   * regex - Regex expression to use for the filter.
   * limit - Number of AQAnnotations to return.
   * not - Whether to negate the entire query. Default is false.
  */
  def apply(arr: Array[AQAnnotation], name: String, regex: String,  limit: Integer=0, not: Boolean=false): Array[AQAnnotation] = {
    
    var rs = arr.filter(x =>  if (regex == "") 
                                x.properties.getOrElse(Map.empty).get(name) != None
                              else
                                x.properties.getOrElse(Map.empty).get(name) != None &&
                                regex.r.pattern.matcher(x.properties.getOrElse(Map.empty).getOrElse(name,"")).matches())
    
    if (not) {
      rs = arr.diff(rs)
              .filter(x => x.properties.getOrElse(Map.empty).get(name) != None)
    } 
    
    if (limit > 0) {
      rs.slice(0,limit)
    } else {
      rs
    }    
    
  }
  
}