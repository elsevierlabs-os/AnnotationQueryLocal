package com.elsevier.aql.utilities

import com.elsevier.aql.annotations.AQAnnotation
import com.elsevier.aql.query.FilterType

class HydrateSuite extends AQBase {
   
    test("Check missing annotation file") {
      val aqAnnots: Array[AQAnnotation] = GetAQAnnotations(getCaretFileAnnots("src/test/resources/genia/S0022314X13001777"),"S0022314X13001777",Array("orig","lemma","pos","excludes"),Array("lemma","pos"),Array("orig","lemma"))
      val sentenceAnnots = FilterType(aqAnnots,"sentence")
      val hydrateAnnots = Hydrate(sentenceAnnots,getStrFile("src/test/resources/junk/"))
      assert(hydrateAnnots(0) == AQAnnotation("S0022314X13001777","ge","sentence",18546,18607,1,None))
    }      
           
    test("Check sentence - 2") {
      val aqAnnots: Array[AQAnnotation] = GetAQAnnotations(getCaretFileAnnots("src/test/resources/genia/S0022314X13001777"),"S0022314X13001777",Array("orig","lemma","pos","excludes"),Array("lemma","pos"),Array("orig","lemma"))
      val sentenceAnnots = FilterType(aqAnnots,"sentence")
      val hydrateAnnots = Hydrate(sentenceAnnots,getStrFile("src/test/resources/str/S0022314X13001777"))
      assert(hydrateAnnots(0) == AQAnnotation("S0022314X13001777","ge","sentence",18546,18607,1,Some(Map("text" -> "Sylow p-groups of polynomial permutations on the integers mod"))))
    }
      
    test("Check sentence with excludes - 2") {
      val aqAnnots: Array[AQAnnotation] = GetAQAnnotations(getCaretFileAnnots("src/test/resources/genia/S0022314X13001777"),"S0022314X13001777",Array("orig","lemma","pos","excludes"),Array("lemma","pos"),Array("orig","lemma"))
      val sentenceAnnots = FilterType(aqAnnots,"sentence")
      val hydrateAnnots = Hydrate(sentenceAnnots,getStrFile("src/test/resources/str/S0022314X13001777"))
      assert(hydrateAnnots(8) == AQAnnotation("S0022314X13001777","ge","sentence",20490,20777,256,Some(Map("excludes" -> "2872,om,mml:math,20501,20510|2894,om,mml:math,20540,20546|2907,om,mml:math,20586,20590|2913,om,mml:math,20627,20630|2923,om,mml:math,20645,20651|2933,om,mml:math,20718,20721", "text" -> "A function  arising from a polynomial in  or, equivalently, from a polynomial in , is called a polynomial function on . We denote by  the monoid with respect to composition of polynomial functions on . By monoid, we mean semigroup with an identity element."))))
    }
    
    test("Check sentence without excludes - 2") {
      val aqAnnots: Array[AQAnnotation] = GetAQAnnotations(getCaretFileAnnots("src/test/resources/genia/S0022314X13001777"),"S0022314X13001777",Array("orig","lemma","pos","excludes"),Array("lemma","pos"),Array("orig","lemma"))
      val sentenceAnnots = FilterType(aqAnnots,"sentence")
      val hydrateAnnots = Hydrate(sentenceAnnots,getStrFile("src/test/resources/str/S0022314X13001777"),false)
      assert(hydrateAnnots(8) == AQAnnotation("S0022314X13001777","ge","sentence",20490,20777,256,Some(Map("excludes" -> "2872,om,mml:math,20501,20510|2894,om,mml:math,20540,20546|2907,om,mml:math,20586,20590|2913,om,mml:math,20627,20630|2923,om,mml:math,20645,20651|2933,om,mml:math,20718,20721", "text" -> "A function g:Zpn→Zpn arising from a polynomial in Zpn[x] or, equivalently, from a polynomial in Z[x], is called a polynomial function on Zpn. We denote by (Fn,∘) the monoid with respect to composition of polynomial functions on Zpn. By monoid, we mean semigroup with an identity element."))))
    }
}