package com.elsevier.aql.utilities

import scala.collection.mutable.MutableList
import scala.collection.mutable.ListBuffer
import java.net.URLDecoder
import scala.io.BufferedSource

import com.elsevier.aql.annotations.AQAnnotation
import com.elsevier.aql.query.BinaryOp


/**
 * This function will retrieve the xml for each AQAnnotation in the passed Array[AQAnnotation], populate the xml property with this value in the AQAnnotation, and return a Array[AQAnnotation] with the xml property populated.  
 */
object HydrateXML {
  
  val logger = org.apache.logging.log4j.LogManager.getLogger("HydrateXML")
  
  /**
   * @param arr The Array of Annotations that we want to populate the xml property with the xml for this annotation 
   * @param str The string (for the text) for the document in the Array of Annotations.
   * @param om Array[AQAnnotation] of the Original Markup annotations.  The annotations should be for the same documents contained in the arr.
   * @param displayAttrs Whether we want to display attributes.  True means to display the attributes.
   * @return Array[AQAnnotation]
  */

  def apply(arr: Array[AQAnnotation], str: String, om: Array[AQAnnotation], displayAttrs:Boolean=true): Array[AQAnnotation] = {
  
    val ATTR = "attr"
    val XML = "xml"
  
    arr.map(ann => {
      // Was an empty str passed or is the 'xml' property already set?
      if (str == "" || (ann.properties.getOrElse(Map.empty).getOrElse(XML,"") != "")) {
        // Just return the original annotation 
        ann
      } else {  
        val origText = str.substring(ann.startOffset.toInt, ann.endOffset.toInt)
        // Get any om annotations within the start/end and 
        val omToks =om.filter(rec => rec.docId == ann.docId && rec.startOffset >= ann.startOffset && rec.endOffset <= ann.endOffset)
                      .flatMap(rec => {
                        //(startOffset,endOffset,parentId,annotId,text)
                        val entries:MutableList[(Long,Long,Long,Long,String)] = MutableList[(Long,Long,Long,Long,String)]()
                        
                        // Get the xml tokens
                        var attrs = ""
                        if (displayAttrs && rec.properties.getOrElse(Map.empty).getOrElse(ATTR,"") != "") {
                          for (attrEntry <- rec.properties.getOrElse(Map.empty).getOrElse(ATTR,"").split("&")) {
                            val attrNameValue = attrEntry.split("=")
                            attrs = attrs.concat(" " + attrNameValue(0) + "=\"" + URLDecoder.decode(attrNameValue(1),"utf-8") + "\"")
                          }
                        }
                        
                        // Check if begin/end are the same and add only one entry
                        if (rec.startOffset == rec.endOffset) {
                          entries += ((rec.startOffset, rec.startOffset,rec.properties.getOrElse(Map.empty).getOrElse("parentId","0").toLong,rec.annotId, "<" + rec.annotType +  attrs + "/>"))

                        // Add begin/end tag for the entry
                        } else {
                          entries += ((rec.startOffset, rec.startOffset, rec.properties.getOrElse(Map.empty).getOrElse("parentId","0").toLong, rec.annotId, "<" + rec.annotType + attrs + ">"))
                          // use a negative value for the parentId on the end tag
                          entries += ((rec.endOffset, rec.endOffset, -rec.properties.getOrElse(Map.empty).getOrElse("parentId","0").toLong, rec.annotId, "</" + rec.annotType + ">"))
                        }
                        entries.toIterator
                      })
                      .sortBy(x => (x._1,x._2,x._3,x._4))  // Sort by startOffset, endOffset, parentId, annotId
                         
        
       // Start stepping through the tags.  Add them to the buffer and substring from text.
        var modText = if (omToks.size == 0) {
                        origText
                      } else {
                        var txt = ""
                        var curOffset = ann.startOffset
                        omToks.foreach(entry => {
                          // check if offset is less than current offset
                          if (entry._1 <= curOffset) {
                            txt = txt.concat(entry._5)
                          } else {
                            txt = txt.concat(origText.substring((curOffset - ann.startOffset).toInt,(entry._1 - ann.startOffset).toInt))
                            txt = txt.concat(entry._5)
                            curOffset = entry._2
                          }
                        })
                        if (curOffset < ann.endOffset) {
                          txt = txt.concat(origText.substring((curOffset - ann.startOffset).toInt))
                        }
                        txt
                      }                      
     
        var props = ann.properties.getOrElse(Map.empty)
        props += (XML -> modText)     
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

