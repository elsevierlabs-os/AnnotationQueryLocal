package com.elsevier.aql.utilities

import java.net.URLEncoder
import scala.collection.mutable.ListBuffer
import com.elsevier.aql.annotations.AQAnnotation
 
/**
 * This function converts a Array[AQAnnotation] to a Array[caret delimited annotations].  
 */
object GetCaretAnnotations {
  
  /*
   * aqAnnotations - Array of AQAnnotations to convert to Array of caret delimited annotations
   * props - Array of property names  to make name-value pairs in the other column of caret delimited annotations.
   * encodeProps - Array of property names  to url encode the value when making name-value pairs in the other column of caret delimited annotations.
  */

  def apply(aqAnnotations:Array[AQAnnotation],  props:Array[String]=Array.empty[String], encodeProps:Array[String]=Array.empty[String]): Array[String] = {

    val WILDCARD = "*"
    
    aqAnnotations.map(aqAnnotation => {
    
      var otherBuf:ListBuffer[String] = new ListBuffer[String]()
    
      for((key:String,value:String) <- aqAnnotation.properties.getOrElse(Map.empty)) {
      
        if (props.contains(key) || props.contains(WILDCARD)) {
          if (encodeProps.contains(key) || encodeProps.contains(WILDCARD)) {
            otherBuf += (key + "=" + URLEncoder.encode(value,"UTF-8"))
          } else {
            otherBuf += (key + "=" + value)
          }
        } 
      }
    
      if (otherBuf.size > 0) {
        Array(aqAnnotation.annotId,
              aqAnnotation.annotSet,
              aqAnnotation.annotType,
              aqAnnotation.startOffset,
              aqAnnotation.endOffset,
              otherBuf.mkString("&")).mkString("^")
      } else {
        Array(aqAnnotation.annotId,
              aqAnnotation.annotSet,
              aqAnnotation.annotType,
              aqAnnotation.startOffset,
              aqAnnotation.endOffset).mkString("^")
      }
    }) 
  }
  
}