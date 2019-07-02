package com.elsevier.aql.annotations

/** AQAnnotation **/
case class AQAnnotation(docId: String,                                   // Document Id (PII)
                         annotSet: String,                               // Annotation set (such as scnlp, ge)
                         annotType: String,                              // Annotation type (such as text, sentence)
                         startOffset: Long,                              // Starting offset for the annotation (based on the text file for the document)
                         endOffset: Long,                                // Ending offset for the annotation (based on the text file for the document)
                         annotId: Long,                                  // Annotation Id
                         properties: Option[Map[String,String]] = None)  // Properties