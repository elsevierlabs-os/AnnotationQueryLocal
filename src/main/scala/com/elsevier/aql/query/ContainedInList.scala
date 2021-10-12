package com.elsevier.aql.query

import scala.collection.mutable.ListBuffer
import com.elsevier.aql.annotations.AQAnnotation

 /**
  * Provide the ability to find annotations that are contained by another annotation.  The input is 2 Datasets of AQAnnotations.  We will call them A and B.  
  * The purpose is to find those annotations in A that are contained in B.  What that means is the start/end offset for an annotation from A  must be contained by the start/end offset from an annotation in  B.  
  * We of course have to also match on the document id.  
  * We ultimately return an array with 2 fields where the first field is an annotation from B and the second field is an array of entries from A
  * that are contained in the first entry.   
  */
object ContainedInList {
  
  /**
   * @param left - Array of AQAnnotations, the ones we will return (as a list) if they are contained in AQAnnotations from 'right'.
   * @param right - Array of AQAnnotations, the ones we are looking to see if they contain AQAnnotations from 'left'.
   * @return Array[AQAnnotation,Array[AQAnnotation]]
   */
  def apply(left: Array[AQAnnotation], right: Array[AQAnnotation]): Array[(AQAnnotation,Array[AQAnnotation])] = {
    
    var lb = new ListBuffer[(AQAnnotation,Array[AQAnnotation])]()
    
    for(ann <- right) {
        val res = BinaryOp(left, Array(ann), 
                          (l, r) => l.startOffset >= r.startOffset &&
                                    l.endOffset <= r.endOffset && 
                                    !((l.annotSet == r.annotSet) && (l.annotType == r.annotType) && (l.startOffset == r.startOffset) && (l.endOffset == r.endOffset)))
        
        if (res.size > 0) {                            
          lb += ((ann,res.sortBy { x => x.startOffset }))
        }
    }
                                
    lb.toArray
     
  }
  
}