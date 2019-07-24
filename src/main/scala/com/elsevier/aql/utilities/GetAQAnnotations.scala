package com.elsevier.aql.utilities

import java.net.URLDecoder
import scala.collection.mutable.ListBuffer
import scala.io.BufferedSource
  
import com.elsevier.aql.annotations.AQAnnotation
  
  
/**
 * This function converts a Array[caret delimited annotations] to a Array[AQAnnotation].  
 * A bare-bones AQAnnotation (with no properties) can be generated by only specifying the strArr and docId.
 * If properties (name-value pairs from the caret delimited annotation other column) are desired, you have the option of specifying an Array of names (from these name-value pairs).  
 * Additionally, you have the option of specifying if the values for these properties should be lower-cased and/or url decoded.
 */
object GetAQAnnotations extends  { 

  val logger = org.apache.logging.log4j.LogManager.getLogger("GetAQAnnotations")
  
  /**
   * strArr - Array of Strings (caret delimited annotations) 
   * docId - name of file in the directory (will also be used for docId in the AQAnnotation).
   * props - Array of property names  (from the name-value pairs in the other column in caret delimited annotation) that you would like populated in the AQAnnotation Map of properties.
   * lcProps - Array of property names where the value should be lower cased when populating the AQAnnotation Map of properties.
   * decodeProps - Array of property names where the value should be url decoded when populating the AQAnnotation Map of properties.
   */
  def apply(strArr: Array[String], docId: String, props:Array[String]=Array.empty[String], lcProps:Array[String]=Array.empty[String], decodeProps:Array[String]=Array.empty[String]): Array[AQAnnotation] = {
    
    // Generated AQAnnotations
    annots(docId, strArr, props, lcProps, decodeProps)

  }

  
  /**
   * key - value to use for the docId in AQAnnotations
   * str - Array of caret delimited annotations
   * props - Array of property names  (from the name-value pairs in the other column in caret delimited annotation) that you would like populated in the AQAnnotation Map of properties.
   * lcProps - Array of property names where the value should be lower cased when populating the AQAnnotation Map of properties.
   * decodeProps - Array of property names where the value should be url decoded when populating the AQAnnotation Map of properties.
   */
  private def annots(key: String, strArr: Array[String], props:Array[String], lcProps:Array[String], decodeProps:Array[String]): Array[AQAnnotation] = {

    val ORIG = "orig"
    val ORIG_ANNOT_ID = "origAnnotID"
    val PARENT_ID = "parentId"
    val ATTR = "attr"
    val OM_NON_ATTRIBUTE_PROPERTIES = Array(ORIG, ORIG_ANNOT_ID, PARENT_ID)
    val OM_ANNOT_SET = "om"
    val WILDCARD = "*"
  
    // Buffer to hold the generated AQAnnotations
    val lb: ListBuffer[AQAnnotation] = new ListBuffer[AQAnnotation]()
   
    // Get the document of caret delimited annotations.  
    try {
      
      // Loop through each record
      for (strRec <- strArr) {
     
        val strRecArr: Array[String] = strRec.split(("\\^"))
      
        // Process the name-value attribute (if there are any)
        if (strRecArr.length > 5) {
        
          var attrBuf:ListBuffer[String] = new ListBuffer[String]()
          var propsMap = Map[String, String]()
          val otherToks = strRecArr(5).split("&")
        
          for(otherTok <- otherToks) {
                
            // Get the key-value pair
            val toks = otherTok.split("=")
            if (toks.size == 2) {
              val key = toks(0)
              var value = toks(1)
                
                  if (props.contains(key) || props.contains(WILDCARD)) {
                  
                    // Check if it should be url decoded
                    if (decodeProps.contains(key) || decodeProps.contains(WILDCARD)) {
                      value = URLDecoder.decode(value,"UTF-8")
                    }                  
                  
                    // Check if it should be lower cased
                    if (lcProps.contains(key) || lcProps.contains(WILDCARD)) {
                      value = value.toLowerCase
                    }
                  
                    if (props.contains(WILDCARD) && strRecArr(1).toLowerCase == OM_ANNOT_SET && !(OM_NON_ATTRIBUTE_PROPERTIES.contains(key))) {
                      attrBuf += otherTok
                    } else {
                      // Add the entry to the map
                      propsMap += (key -> value)
                    }
               
                  } else if ((props contains ATTR) && strRecArr(1).toLowerCase == OM_ANNOT_SET) {   
                
                    if (!(OM_NON_ATTRIBUTE_PROPERTIES contains key)) {
                      attrBuf += otherTok
                    }
                  
                  }
        
            }
          }
          
          // Make sure there was not an attr property in the other column before overriding it
          if (attrBuf.size > 0 && !(propsMap.contains(ATTR))) {
            propsMap += (ATTR -> attrBuf.mkString("&"))
          }
        
          // Currently don't try and populate text
          val aqAnnotation : AQAnnotation = AQAnnotation(key,
                                                         strRecArr(1),
                                                         strRecArr(2),
                                                         strRecArr(3).toLong,
                                                         strRecArr(4).toLong,
                                                         strRecArr(0).toLong,
                                                         if (propsMap.size > 0) Some(propsMap) else None)
           lb += aqAnnotation

        }
        
      }
      
    } catch {
      case e: Exception => println("Unable to find document: " + key)
    }
    
    lb.toArray
    
  } 
    
}