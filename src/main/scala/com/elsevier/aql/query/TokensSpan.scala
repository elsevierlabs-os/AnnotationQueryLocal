package com.elsevier.aql.query

import scala.collection.mutable.ListBuffer
import com.elsevier.aql.annotations.AQAnnotation

/**
 * Provides the ability to create a string from a list of tokens that are contained in a span. The specified tokenProperty is used to extract the values from the tokens when creating the string. 
 * For SCNLP, this tokenProperty could be values like 'orig', 'lemma', or 'pos'. The spans would typically be a SCNLP 'sentence' or could even be things like an OM 'ce:para'.  
 * Returns a Array[AQAnnotation] spans with 3 new properties all prefixed with the specified tokenProperty value followed by (ToksStr, ToksSpos, ToksEpos) The ToksStr property will be the 
 * concatenated string of token property values contained in the span. The ToksSPos and ToksEpos are properties that will help us determine the start/end offset for each of the individual tokens in the ToksStr. 
 * These helper properties are needed for the function RegexTokensSpan so we can generate accurate accurate start/end offsets based on the str file.
 */
object TokensSpan {

  /*
   * tokens - Array of AQAnnotations (which we will use to concatenate for the string)
   * spans - Array of AQAnnotations (identifies the start/end for the tokens to be used for the concatenated string)
   * tokenProperty - The property field in the tokens to use for extracting the value for the concatenated string
  */
  def apply(tokens: Array[AQAnnotation], spans: Array[AQAnnotation], tokenProperty: String): Array[AQAnnotation] = {
    
    var lb = new ListBuffer[AQAnnotation]()
    ContainedInList(tokens,spans).foreach(rec => {
      val span = rec._1
      val tokenList = rec._2
      var newProps = Map[String,String]()
      var oldProps = span.properties.getOrElse(Map.empty)
      for ((k,v) <- oldProps) {
        newProps += (k -> v)
      }
      val toksStr = ListBuffer[String]()
      val toksSpos = ListBuffer[String]()
      val toksEpos = ListBuffer[String]()
      var offset = 0      
      for (token <- tokenList) {
        val tokStr = token.properties.getOrElse(Map.empty).getOrElse(tokenProperty,"")
        toksStr += (tokStr)
        toksSpos += (offset.toString + "|" +token.startOffset.toString)
        offset += (tokStr.length)
        toksEpos += (offset.toString + "|" + token.endOffset.toString)
        offset += 1
      }
      newProps += (tokenProperty + "ToksStr" -> toksStr.mkString(" "))
      newProps += (tokenProperty + "ToksSpos" -> toksSpos.mkString(" "))
      newProps += (tokenProperty + "ToksEpos" -> toksEpos.mkString(" "))
      val annot = new AQAnnotation(span.docId,                       // Map to AQAnnotation
                                   span.annotSet,
                                   span.annotType,
                                   span.startOffset,
                                   span.endOffset,
                                   span.annotId,
                                   Some(newProps))
            lb += (annot)
    })
    
    return lb.toArray

  }
  
}