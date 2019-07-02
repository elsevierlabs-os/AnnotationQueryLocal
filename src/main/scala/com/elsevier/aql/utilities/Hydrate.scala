package com.elsevier.aql.utilities

import java.net.URLDecoder
import scala.collection.mutable.ListBuffer
import scala.io.BufferedSource

import com.elsevier.aql.annotations.AQAnnotation
  

/**
 * This function will retrieve the text for each AQAnnotation in the passed Array[AQAnnotation], populate the text property with this value in the AQAnnotation, and return a Array[AQAnnotation] with the text property populated.  
 * Keep in mind that for 'text/word' annotations the orig column will already be populated with the 'original' text value so this may not be needed.  
 * However, if you are working with sentence annotations (and other similar annotations) this could prove to be very helpful. 
 */
object Hydrate {

  val logger = org.apache.logging.log4j.LogManager.getLogger("Hydrate")

  /**
   * Local Filesystem implementation
   * arr - The Array of Annotations that we want to populate the text column with the text for this annotation
   * str - The string (for the text) for the document in the Array of Annotations.
   * excludes - Whether we want to include the 'excludes' text.  True means exclude the excluded text.
  */
  def apply(arr: Array[AQAnnotation], str: String, excludes: Boolean=true): Array[AQAnnotation] = {
    
    val TEXT = "text"
    val EXCLUDES = "excludes"   
    
    var results:Array[AQAnnotation] = Array.empty[AQAnnotation]
                  
    arr.map(ann => {
      
                  var text = ""
            
                  // Was an empty str passed or is the 'text' property already set?
                  if (str == "" || (ann.properties.getOrElse(Map.empty).getOrElse(TEXT,"") != "")) {
                    // Just return the original annotation 
                    ann
                  } else {
                
                    // Check the excludes flag
                    if (excludes && ann.properties.getOrElse(Map.empty).getOrElse(EXCLUDES,"") != "") {
                      // Check for excludes (startOffset,endOffset)
                      var exList:ListBuffer[(Integer,Integer)] = ListBuffer[(Integer,Integer)]()
                      // (annotId,annotSet,annotType,startOffset,endOffset)
                      var lb = new ListBuffer[(Long,String,String,Long,Long)]
                      for (excludesEntry <- ann.properties.getOrElse(Map.empty).getOrElse(EXCLUDES,"").split("\\|")) {
                        var excToks = excludesEntry.split(",")
                        lb += ((excToks(0).toLong,excToks(1),excToks(2),excToks(3).toLong,excToks(4).toLong))
                      }
                      val excludes = lb.distinct.toArray
                      excludes.foreach(entry => {
                        // (startOffset,endOffset)
                        exList += ((entry._4.toInt,entry._5.toInt))
                      })
                      exList.sortBy(x => (x._1,x._2))
                      // Process the excludes 
                      var curOffset: Integer = ann.startOffset.toInt
                      for (exclude <- exList) {
                        if (exclude._1 <= curOffset) {
                          curOffset = exclude._2
                        } else {
                          text = text + str.substring(curOffset,exclude._1)
                          curOffset = exclude._2
                        }
                      }
                      if (curOffset < ann.endOffset) {
                        text = text + str.substring(curOffset, ann.endOffset.toInt)
                      }
                    } else {
                      // Excludes flag was not set or there were no excludes present
                      text = str.substring(ann.startOffset.toInt, ann.endOffset.toInt)
                    }
                    var props = ann.properties.getOrElse(Map.empty)
                    props += (TEXT -> text)
                    new AQAnnotation(ann.docId,                       // Map to AQAnnotation
                                     ann.annotSet.toLowerCase,
                                     ann.annotType.toLowerCase,
                                     ann.startOffset,
                                     ann.endOffset,
                                     ann.annotId,
                                     Some(props))             
                  }

           })

  }

}