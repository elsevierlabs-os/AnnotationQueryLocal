package com.elsevier.aql.utilities

import scala.collection.mutable.MutableList
import scala.collection.mutable.ListBuffer
import java.net.URLDecoder
import scala.io.BufferedSource

import com.elsevier.aql.annotations.AQAnnotation
import com.elsevier.aql.query.BinaryOp


/**
 * Output the string of text identified by the AQannotation and highlight in 'red' the text that was ignored (excluded).  
 * Also add the XML tags (in 'orange') that would have occurred in this string.  
 * Note, there are no guarantees that the XML will be well-formed.
 */
object HydrateXML {
  
  val logger = org.apache.logging.log4j.LogManager.getLogger("HydrateXML")
  
  /*
   * results - Array[AQAnnotation] that you would like to display.  
   * textPath - Path for the str files.  The annotations in results must be for documents contained in these str files.
   * om - Array[AQAnnotation] of the Original Markup annotations.  The annotations should be for the same documents contained in the results.
   * nrows - Number of results to display
   * offset - Number of characters before/after each annotation in results to display
   * highlightAnnotations - Array[AQAnnotation] that you would like to highlight in the results
  */

  def apply(results: Array[AQAnnotation], textPath: String, om: Array[AQAnnotation], displayAttrs:Boolean=true): Array[AQAnnotation] = {
  
    val ATTR = "attr"
    val XML = "xml"
    val offset = 0
    var str= ""
    var lastDoc= ""
  
    // Get the Original Markup tokens for the number of results to display
    val xmlTokens = BinaryOp(om,results,
                             (l, r) => l.startOffset >= r.startOffset &&
                                       l.endOffset <= r.endOffset)
                    .sortBy{x => (x.docId, x.startOffset,x.annotType)}
      
    results.sortBy {x => (x.docId,x.startOffset)}.map(ann => {
        var currDoc = ann.docId
        if (!currDoc.equals(lastDoc)) {
          var source: Option[BufferedSource] = None               
          str = try {
                      source = Some(scala.io.Source.fromFile(textPath + currDoc,"utf-8"))
                      source.get.mkString 
                    } catch {
                      case e: Exception => 
                        logger.error("Unable to find document: " + ann.docId)
                        ""               
                    } finally {
                      if (source != None) {
                        source.get.close()
                      } 
                    }
                    lastDoc = currDoc
        }

        // Get the text for annotation (account for the offset)
        if (str == "" || (ann.properties.getOrElse(Map.empty).getOrElse(XML,"") != "")) {
          // Just return the original annotation 
          ann
        } else {

        val origText = str.substring(ann.startOffset.toInt, ann.endOffset.toInt)
      
        // Get any om annotations and excludes within the start/end and 
        val omToks = xmlTokens.filter(rec => rec.docId == ann.docId && rec.startOffset >= ann.startOffset && rec.endOffset <= ann.endOffset)
                      .flatMap(rec => {
                        //(startOffset,endOffset,type,text)
                        val entries:MutableList[(Long,Long,String,String)] = MutableList[(Long,Long,String,String)]()
                        
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
                          entries += ((rec.startOffset, rec.startOffset, "om", "<" + rec.annotType +  attrs + ">"))

                        // Add begin/end tag for the entry
                        } else {
                          entries += ((rec.startOffset, rec.startOffset, "om", "<" + rec.annotType + attrs + ">"))
                          entries += ((rec.endOffset, rec.endOffset, "om", "</" + rec.annotType + ">"))
                        }
                        entries.toIterator
                      })
      
      
        // Sort by startOffset, endOffset, type
        val allToks = omToks.sortBy(x => (x._1,x._2,x._3))
      
        // Start stepping through the tags.  Add them to the buffer and substring from text.
        var modText = if (allToks.size == 0) {
                        origText
                      } else {
                        var txt = ""
                        var curOffset = ann.startOffset
                        allToks.foreach(entry => {
                          // check if offset is less than current offset
                          if (entry._1 <= curOffset) {
                            if (entry._3 == "om") {
                              txt = txt.concat(entry._4)
                            } 
                          } else {
                            txt = txt.concat(origText.substring((curOffset - ann.startOffset).toInt,(entry._1 - ann.startOffset).toInt))
                            if (entry._3 == "om") {
                              txt = txt.concat(entry._4)
                            } 
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