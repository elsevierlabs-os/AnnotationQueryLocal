package com.elsevier.aql.annotations

/** AQAnnotation.
 *  @param docId Document Id (PII)
 *  @param annotSet Annotation set (such as scnlp, ge)
 *  @param annotType Annotation type (such as text, sentence)
 *  @param startOffset Starting offset for the annotation (based on the text file for the document)
 *  @param endOffset Ending offset for the annotation (based on the text file for the document)
 *  @param annotId Annotation Id
 *  @param properties Map of key-value properties
*/
case class AQAnnotation(docId: String,                                   
                         annotSet: String,                               
                         annotType: String,                              
                         startOffset: Long,                              
                         endOffset: Long,                               
                         annotId: Long,                                  
                         properties: Option[Map[String,String]] = None)  