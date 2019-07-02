package com.elsevier.aql.query

import scala.collection.mutable.ListBuffer
import com.elsevier.aql.annotations.AQAnnotation

/**
 * Provide the ability to find the following sibling annotations for every annotation in the anchor Array[AQAnnotations]. 
 * The following sibling annotations can optionally be required to be contained in a container Array[AQAnnotations]. 
 * The return type of this function is different from other functions. 
 * Instead of returning a Array[AQAnnotation] this function returns a Array[(AQAnnotation,Array[AQAnnotation])].
 */
object Following {

  /*
   * annot - Array of AQAnnotations, the ones we will be using to look for following sibling annotations. 
   * anchor - Array of AQAnnotations  starting point for using to look for following sibling annotations (use the endOffset and docId).
   * container - Array of AQAnnotations to use when requiring the following sibling annotations to be contained in a specific annotation.
   * cnt - Number of preceding sibling AQAnnotations to return.
  */
    def apply(annot: Array[AQAnnotation], anchor: Array[AQAnnotation], container: Array[AQAnnotation]=Array.empty[AQAnnotation], cnt: Integer=3) : Array[(AQAnnotation,Array[AQAnnotation])] = {
    
    var lb = new ListBuffer[(AQAnnotation,Array[AQAnnotation])]()
    
    for(ann <- anchor) {
        val res = BinaryOp(annot,Array(ann), (l, r) => l.startOffset >= r.endOffset)
        lb += ((ann,res.sortBy { x => x.startOffset }.slice(0,cnt)))
    }
    
    if (!container.isEmpty) {
      
      val containedResults = new ListBuffer[(AQAnnotation,Array[AQAnnotation])]()
      for (entry <- lb) {
        
        // Check if the entry already exists
        var found: Boolean = false
        if (containedResults.size > 0) {
          for (containedResult <- containedResults if found == false) {
            if (entry._1.docId == containedResult._1.docId &&
                entry._1.annotSet == containedResult._1.annotSet &&
                entry._1.annotType == containedResult._1.annotType &&
                entry._1.annotId == containedResult._1.annotId &&
                entry._1.startOffset == containedResult._1.startOffset &&
                entry._1.endOffset == containedResult._1.endOffset &&
                entry._1.properties.getOrElse(Map.empty) == containedResult._1.properties.getOrElse(Map.empty)
                ) found = true
          }
        }
        if (!found) {
          val containedIn = BinaryOp(container,Array(entry._1), (l,r) => l.startOffset <= r.startOffset &&
                                                                         l.endOffset >=  r.endOffset)
                                                                       
          var res2 = BinaryOp(entry._2, containedIn, (l,r) => l.startOffset >= r.startOffset &&
                                                              l.endOffset <= r.endOffset)
          containedResults += ((entry._1,res2))
        }
      }
      
      containedResults.toArray
      
    } else {
      
      lb.toArray
    }
    
  }

}