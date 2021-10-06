package com.elsevier.aql.query

import scala.collection.mutable.ListBuffer
import java.util.regex.Pattern
import java.util.regex.Matcher
import com.elsevier.aql.annotations.AQAnnotation

/**
 * Provides the ability to apply a regular expression to the concatenated string generated by TokensSpan. For the strings matching the regex,
 * an Array[AQAnnotations] will be returned.  The AQAnnotation will correspond to the offsets within the concatenated string containing the match.
 */
object RegexTokensSpan {

  /*
   * tokensSpan - Array of AQAnnotations (the annotations returned from the TokensSpan function)
   * prop - the property name (orig, lemma, pos) that was used to generate the string for the span in TokensSpan
   * regex - the regular expression to apply to the span
   * annotSet - the value to assign to annotSet for the returned matched annotations (default will be the annotSet from the tokensSpan)
   * annotType - the value to assign to annotType for the returned matched annotations (default will be the annotType from the tokensSpan)
   * annotProps - the additional properties to append to the properties map for the returned matched annotations 
  */
    def apply(tokensSpan: Array[AQAnnotation], prop:String, regex: String, annotSet: String = "", annotType: String = "", annotProps: Map[String,String] = Map[String, String]()): Array[AQAnnotation] = {
      val pattern = Pattern.compile(regex)
      var annotId = 0
      var lb = ListBuffer[AQAnnotation]()
      tokensSpan.foreach(rec => {
        val span = rec.properties.getOrElse(Map.empty).getOrElse(prop+"ToksStr","")
        val matcher = pattern.matcher(span)
        while(matcher.find()) {
          annotId += 1
          var newAnnotSet = annotSet
          var newAnnotType = annotType
          if (annotSet == "") newAnnotSet = rec.annotSet
          if (annotType == "") newAnnotType = rec.annotType
          var props = Map[String,String]()
          val oldProps = rec.properties.getOrElse(Map.empty)
          for ((key,value) <- annotProps) {
            props += (key -> value)
          }
          // start
          var startPos = -1L
          var startPosLB = ListBuffer[Int]()
          for (start <- oldProps.getOrElse(prop+"ToksSpos","").split(" ")) {
            val startToks = start.split("\\|")
            if (startToks(0).toInt == matcher.start()) {
              startPos = startToks(1).toLong
            }
            if (startToks(0).toInt < matcher.start()) {
              startPosLB += (startToks(1).toInt)
            }
          }
          if (startPos == -1) {
            startPos = startPosLB.max
          }
          // end
          var endPos = -1L
          var endPosLB = ListBuffer[Int]()
          for (end <- oldProps.getOrElse(prop+"ToksEpos","").split(" ")) {
            val endToks = end.split("\\|")
            if (endToks(0).toInt == matcher.end()) {
              endPos = endToks(1).toLong
            }
            if (endToks(0).toInt > matcher.end()) {
              endPosLB += (endToks(1).toInt)
            }              
          }
          if (endPos == -1) {
            endPos = endPosLB.min
          }
          props += (prop+"Match" -> span.substring(matcher.start(),matcher.end()))    
          // get the excludes from the span (but only include those contained in within the match)
          for ((k,v) <- oldProps) {
            if (k == "excludes") {
              val excludesLB = ListBuffer[String]()
              for (exclude <- v.split("\\|")) {
                val arr = exclude.split(",")
                val excludeStart = arr(3).toLong
                val excludeEnd = arr(4).toLong
                if (excludeStart >= startPos && excludeEnd <= endPos) {
                  excludesLB += exclude
                }
              }
              if (!excludesLB.isEmpty) 
                props += ("excludes" -> excludesLB.mkString("|"))
            }
          }
          val annot = new AQAnnotation(rec.docId,                       // Map to AQAnnotation
                                       newAnnotSet,
                                       newAnnotType,
                                       startPos,
                                       endPos,
                                       annotId,
                                       Some(props))
            
          lb += (annot)
        }    
      })
      
      return lb.toArray

  }
  
}