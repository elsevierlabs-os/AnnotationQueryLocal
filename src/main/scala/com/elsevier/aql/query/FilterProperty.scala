package com.elsevier.aql.query

import com.elsevier.aql.annotations.AQAnnotation

/**
 * Provide the ability to filter an Array of AQAnnotations based on the value matching the specified property value in the map.  
 * Returns the filtered Array of AQAnnotation.
 * A single value or an array of values can be used for the filter comparison.
 * Note:  If the AQAnnotation doesn't contain the specified property the AQAnnotation will not be returned (regardless of the comparison operator)
 */
object FilterProperty {

  /**
   * @param arr Array of AQAnnotations that will be filtered by the specified property name and value.
   * @param name Name of the property to filter.
   * @param value Value of the named property to filter.
   * @param valueArr The array of values of the named property  to filter. An OR will be applied to the Strings. Only used if value was not specified.
   * @param valueCompare Comparison operator to use for the property filter. Default is '='. Possible values are '=' and '!=' when valueArr specified. Possible values are '=','!=','<','<=','>', and '>=' otherwise.
   * @param limit Number of AQAnnotations to return.
   * @param not Whether to negate the entire query. Default is false.
   * @return Array[AQAnnotation]
  */
  def apply(arr: Array[AQAnnotation], name: String, value: String="", valueArr: Array[String]=Array.empty[String], valueCompare: String="=", limit: Integer=0, not: Boolean=false): Array[AQAnnotation] = {
    
    var rs = arr.filter(x => if (value != "") { 
                                if (x.properties.getOrElse(Map.empty).get(name) == None) false
                                else if (valueCompare == "=") x.properties.getOrElse(Map.empty).get(name).get == value
                                else if (valueCompare == "!=") x.properties.getOrElse(Map.empty).get(name).get != value
                                else if (valueCompare == "<") x.properties.getOrElse(Map.empty).get(name).get < value
                                else if (valueCompare == "<=") x.properties.getOrElse(Map.empty).get(name).get <= value
                                else if (valueCompare == ">") x.properties.getOrElse(Map.empty).get(name).get > value
                                else if (valueCompare == ">=") x.properties.getOrElse(Map.empty).get(name).get >= value  
                                else false
                              } else if (!valueArr.isEmpty) { 
                                if (x.properties.getOrElse(Map.empty).get(name) == None) false
                                else if (valueCompare == "=") valueArr contains x.properties.getOrElse(Map.empty).get(name).get 
                                else !(valueArr contains x.properties.getOrElse(Map.empty).get(name).get) 
                              } else {
                                false
                              })
                   
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