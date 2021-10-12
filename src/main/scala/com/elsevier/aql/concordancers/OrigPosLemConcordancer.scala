package com.elsevier.aql.concordancers

import java.net.URLDecoder
import scala.io.BufferedSource

import com.elsevier.aql.annotations.AQAnnotation
  
/**
 * Output the string of text identified by the AQAnnotation (typically a sentence annotation). 
 * Below the sentence (in successive rows) output the original terms, parts of speech, and lemma terms for the text identified by the AQAnnotation.
 */
object OrigPosLemConcordancer {
  
  val logger = org.apache.logging.log4j.LogManager.getLogger("OrigPosLemConcordancer")
  
  /**
   * @param sentences Sentence annotations that you would like to display.  
   * @param annots The Array of AQAnnotations that will contain the the AQAnnotations (orig, lemma, pos) for the above sentences
   * @param str The string (for the text) for the document in the Array of Annotations.
   * @param wordType The annotType that identifies the AQAnnotation in the above annotations.
   * @param nrows Number of sentences to display
   * @return String of html
  */

  def apply(sentences: Array[AQAnnotation], annots:Array[AQAnnotation],  str: String, wordType:String="word", nrows:Integer=10): String = {

    val ORIG = "orig"
    val POS = "pos"
    val LEMMA = "lemma"
  
    val sentenceAnnots = sentences.sortBy{ x => (x.docId,x.startOffset)}.slice(0,nrows)
    var tmpStr = ""
  
    // Get the TextAnnotations (for the specified annotType) for each sentence
    for(sentence <- sentenceAnnots) {
    val textAnnots = annots.filter( x => x.docId == sentence.docId &&
                                         x.annotType == wordType &&
                                         x.startOffset >= sentence.startOffset &&
                                         x.endOffset <= sentence.endOffset)
                           .sortBy{ x => x.startOffset}

    // Get the raw text for the sentence annotation
    val text =  if (str == null || str == "") {
                  ""
                } else {  
                  try {
                    str.substring(sentence.startOffset.toInt, sentence.endOffset.toInt)
                  } catch {
                    case e: Exception =>  {
                      logger.error(e)
                      ""
                    }
                  }
                }

    tmpStr += "<table border='1' style='font-family: monospace;table-layout: fixed;'><tr>"
    tmpStr += ("<td>" + sentence.docId + "</td>")
    tmpStr += ("<td>" + sentence.startOffset + "</td>")
    tmpStr += ("<td>" + sentence.endOffset + "</td>")
    tmpStr += ("<td colspan='" + textAnnots.size + "'>" + text + "</td>")
    tmpStr += "</tr>"
  
    // Get original row
    tmpStr += "<tr>"
    tmpStr += ("<td>orig</td>")
    tmpStr += ("<td bgcolor='grey'/>")
    tmpStr += ("<td bgcolor='grey'/>")
    for (annot <- textAnnots) {
      tmpStr += ("<td>" + URLDecoder.decode(annot.properties.getOrElse(Map.empty).getOrElse(ORIG,"UNKNOWN"),"utf-8") + "</td>")
    }
    tmpStr += "</tr>"

    // Get pos row
    tmpStr += "<tr>"
    tmpStr += ("<td>pos</td>")
    tmpStr += ("<td bgcolor='grey'/>")
    tmpStr += ("<td bgcolor='grey'/>")
    for (annot <- textAnnots) {
      tmpStr += ("<td>" + URLDecoder.decode(annot.properties.getOrElse(Map.empty).getOrElse(POS,"UNKNOWN"),"utf-8") + "</td>")
    }
    tmpStr += "</tr>"

    // Get lemma row
    tmpStr += "<tr>"
    tmpStr += ("<td>lemma</td>")
    tmpStr += ("<td bgcolor='grey'/>")
    tmpStr += ("<td bgcolor='grey'/>")
    for (annot <- textAnnots) {
      tmpStr += ("<td>" + URLDecoder.decode(annot.properties.getOrElse(Map.empty).getOrElse(LEMMA,"UNKNOWN"),"utf-8") + "</td>")
    }
    tmpStr += "</tr></table><p/><p/><p/>"
    }
   "<html><body>" + tmpStr + "</body></html>"
    
  }
 
}