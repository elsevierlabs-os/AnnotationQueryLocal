package com.elsevier.aql.utilities

import com.elsevier.aql.annotations.AQAnnotation

class GetCaretAnnotationsSuite extends AQBase {
  
  test("Check count of annotations") {
    val aqAnnots: Array[AQAnnotation] = GetAQAnnotations(getCaretFileAnnots("src/test/resources/genia/S0022314X13001777"),"S0022314X13001777",Array("orig","lemma","pos","excludes"),Array("lemma","pos"),Array("orig","lemma"))
    val caretAnnots: Array[String] = GetCaretAnnotations(aqAnnots,Array("orig","lemma","pos"),Array("orig","lemma"))
    assert(caretAnnots.size == 4066)
  }

  test("Check an annotation") {
    val aqAnnots: Array[AQAnnotation] = GetAQAnnotations(getCaretFileAnnots("src/test/resources/genia/S0022314X13001777"),"S0022314X13001777",Array("orig","lemma","pos","excludes"),Array("lemma","pos"),Array("orig","lemma")).sortBy { x => (x.startOffset, x.endOffset, x.annotType) }
    val caretAnnots: Array[String] = GetCaretAnnotations(aqAnnots,Array("orig","lemma","pos"),Array("orig","lemma"))
    assert(caretAnnots(3) == "4^ge^word^18552^18560^orig=p-groups&lemma=p-group&pos=nns")
  }

  test("Check property wildcard") {
    val aqAnnots: Array[AQAnnotation] = GetAQAnnotations(getCaretFileAnnots("src/test/resources/genia/S0022314X13001777"),"S0022314X13001777", Array("orig", "lemma", "pos", "excludes"), Array("lemma", "pos"), Array("orig", "lemma")).sortBy { x => (x.startOffset, x.endOffset, x.annotType) }
    val caretAnnots: Array[String] = GetCaretAnnotations(aqAnnots, Array("*"))
    assert(caretAnnots(3) == "4^ge^word^18552^18560^orig=p-groups&lemma=p-group&pos=nns")
  }
  
  test("Check encode wildcard") {
    val aqAnnots: Array[AQAnnotation] = GetAQAnnotations(getCaretFileAnnots("src/test/resources/genia/S0022314X13001777"),"S0022314X13001777", Array("orig", "lemma", "pos", "excludes"), Array("lemma", "pos"), Array("orig", "lemma")).sortBy { x => (x.startOffset, x.endOffset, x.annotType) }
    val caretAnnots: Array[String] = GetCaretAnnotations(aqAnnots, Array("*"),Array("*"))
    assert(caretAnnots(3) == "4^ge^word^18552^18560^orig=p-groups&lemma=p-group&pos=nns")
  }
}