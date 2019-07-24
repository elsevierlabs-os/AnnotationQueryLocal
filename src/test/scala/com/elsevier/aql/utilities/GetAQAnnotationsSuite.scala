package com.elsevier.aql.utilities

import com.elsevier.aql.annotations.AQAnnotation

class GetAQAnnotationsSuite extends AQBase {
   
  test("Check missing annotation file") {
    val aqAnnots: Array[AQAnnotation] = GetAQAnnotations(getCaretFileAnnots("src/test/resources/genia/junk"),"junk",Array("orig","lemma","pos","excludes"),Array("lemma","pos"),Array("orig","lemma"))
    assert(aqAnnots.size == 0)
  }

  test("Check count of annotations") {
    val aqAnnots: Array[AQAnnotation] = GetAQAnnotations(getCaretFileAnnots("src/test/resources/genia/S0022314X13001777"),"S0022314X13001777",Array("orig","lemma","pos","excludes"),Array("lemma","pos"),Array("orig","lemma"))
    assert(aqAnnots.size == 4066)
  }

  test("Check an annotation") {
    val aqAnnots: Array[AQAnnotation] = GetAQAnnotations(getCaretFileAnnots("src/test/resources/genia/S0022314X13001777"),"S0022314X13001777",Array("orig","lemma","pos","excludes"),Array("lemma","pos"),Array("orig","lemma")).sortBy { x => (x.startOffset, x.endOffset, x.annotType) }
    assert(aqAnnots(0) == AQAnnotation("S0022314X13001777","ge","word",18546,18551,3,Some(Map("orig" -> "Sylow", "lemma" -> "sylow", "pos" -> "jj"))))
  }
  
    test("Check property wildcard") {
    val aqAnnots: Array[AQAnnotation] = GetAQAnnotations(getCaretFileAnnots("src/test/resources/genia/S0022314X13001777"),"S0022314X13001777", Array("*")).sortBy { x => (x.startOffset, x.endOffset, x.annotType) }
    assert(aqAnnots.take(1)(0) == AQAnnotation("S0022314X13001777","ge","word",18546,18551,3,Some(Map("lemma" -> "sylow", "pos" -> "JJ", "tokidx" -> "1", "orig" -> "Sylow", "parentId" -> "4054", "origAnnotID" -> "4055"))))
  }

  test("Check lower case wildcard") {
    val aqAnnots: Array[AQAnnotation] = GetAQAnnotations(getCaretFileAnnots("src/test/resources/genia/S0022314X13001777"),"S0022314X13001777", Array("*"),Array("*")).sortBy { x => (x.startOffset, x.endOffset, x.annotType) }
    assert(aqAnnots.take(1)(0) == AQAnnotation("S0022314X13001777","ge","word",18546,18551,3,Some(Map("lemma" -> "sylow", "pos" -> "jj", "tokidx" -> "1", "orig" -> "sylow", "parentId" -> "4054", "origAnnotID" -> "4055"))))
  }

  test("Check url decode wildcard") {
    val aqAnnots: Array[AQAnnotation] = GetAQAnnotations(getCaretFileAnnots("src/test/resources/genia/S0022314X13001777"),"S0022314X13001777", Array("*"),Array.empty,Array("*")).sortBy { x => (x.startOffset, x.endOffset, x.annotType) }
    assert(aqAnnots.take(1)(0) == AQAnnotation("S0022314X13001777","ge","word",18546,18551,3,Some(Map("lemma" -> "sylow", "pos" -> "JJ", "tokidx" -> "1", "orig" -> "Sylow", "parentId" -> "4054", "origAnnotID" -> "4055"))))
  }
    
}