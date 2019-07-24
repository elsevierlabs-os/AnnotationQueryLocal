package com.elsevier.aql.utilities

import com.elsevier.aql.annotations.AQAnnotation
import com.elsevier.aql.query.FilterType

class HydrateXMLSuite extends AQBase {
  
    test("Check sentence") {
      val aqAnnots: Array[AQAnnotation] = GetAQAnnotations(getCaretFileAnnots("src/test/resources/genia/S0022314X13001777"),"S0022314X13001777",Array("orig","lemma","pos","excludes"),Array("lemma","pos"),Array("orig","lemma"))
      val sentenceAnnots = FilterType(aqAnnots,"sentence")
      val omAnnots: Array[AQAnnotation] = GetAQAnnotations(getCaretFileAnnots("src/test/resources/om/S0022314X13001777"),"S0022314X13001777",Array("*"),decodeProps=Array("*"))
      val hydrateAnnots = HydrateXML(sentenceAnnots,getStrFile("src/test/resources/str/S0022314X13001777"),omAnnots)
      assert(hydrateAnnots(0) == AQAnnotation("S0022314X13001777","ge","sentence",18546,18607,1,Some(Map("xml" -> "Sylow <ce:italic>p</ce:italic>-groups of polynomial permutations on the integers mod"))))
    }
      
    test("Check sentence with excludes") {
      val aqAnnots: Array[AQAnnotation] = GetAQAnnotations(getCaretFileAnnots("src/test/resources/genia/S0022314X13001777"),"S0022314X13001777",Array("orig","lemma","pos","excludes"),Array("lemma","pos"),Array("orig","lemma"))
      val sentenceAnnots = FilterType(aqAnnots,"sentence")
      val omAnnots: Array[AQAnnotation] = GetAQAnnotations(getCaretFileAnnots("src/test/resources/om/S0022314X13001777"),"S0022314X13001777",Array("*"),decodeProps=Array("*"))
      val hydrateAnnots = HydrateXML(sentenceAnnots,getStrFile("src/test/resources/str/S0022314X13001777"),omAnnots)
      assert(hydrateAnnots(8) == AQAnnotation("S0022314X13001777","ge","sentence",20490,20777,256,Some(Map("excludes" -> "2872,om,mml:math,20501,20510|2894,om,mml:math,20540,20546|2907,om,mml:math,20586,20590|2913,om,mml:math,20627,20630|2923,om,mml:math,20645,20651|2933,om,mml:math,20718,20721", "xml" -> """A function <mml:math altimg="si14.gif" overflow="scroll"><mml:mi>g</mml:mi><mml:mo>:</mml:mo><mml:msub><mml:mrow><mml:mi mathvariant="double-struck">Z</mml:mi></mml:mrow><mml:mrow><mml:msup><mml:mrow><mml:mi>p</mml:mi></mml:mrow><mml:mrow><mml:mi>n</mml:mi></mml:mrow></mml:msup></mml:mrow></mml:msub><mml:mo>→</mml:mo><mml:msub><mml:mrow><mml:mi mathvariant="double-struck">Z</mml:mi></mml:mrow><mml:mrow><mml:msup><mml:mrow><mml:mi>p</mml:mi></mml:mrow><mml:mrow><mml:mi>n</mml:mi></mml:mrow></mml:msup></mml:mrow></mml:msub></mml:math> arising from a polynomial in <mml:math altimg="si15.gif" overflow="scroll"><mml:msub><mml:mrow><mml:mi mathvariant="double-struck">Z</mml:mi></mml:mrow><mml:mrow><mml:msup><mml:mrow><mml:mi>p</mml:mi></mml:mrow><mml:mrow><mml:mi>n</mml:mi></mml:mrow></mml:msup></mml:mrow></mml:msub><mml:mo stretchy="false">[</mml:mo><mml:mi>x</mml:mi><mml:mo stretchy="false">]</mml:mo></mml:math> or, equivalently, from a polynomial in <mml:math altimg="si16.gif" overflow="scroll"><mml:mi mathvariant="double-struck">Z</mml:mi><mml:mo stretchy="false">[</mml:mo><mml:mi>x</mml:mi><mml:mo stretchy="false">]</mml:mo></mml:math>, is called a <ce:italic>polynomial function</ce:italic> on <mml:math altimg="si6.gif" overflow="scroll"><mml:msub><mml:mrow><mml:mi mathvariant="double-struck">Z</mml:mi></mml:mrow><mml:mrow><mml:msup><mml:mrow><mml:mi>p</mml:mi></mml:mrow><mml:mrow><mml:mi>n</mml:mi></mml:mrow></mml:msup></mml:mrow></mml:msub></mml:math>. We denote by <mml:math altimg="si17.gif" overflow="scroll"><mml:mo stretchy="false">(</mml:mo><mml:msub><mml:mrow><mml:mi>F</mml:mi></mml:mrow><mml:mrow><mml:mi>n</mml:mi></mml:mrow></mml:msub><mml:mo>,</mml:mo><mml:mo>∘</mml:mo><mml:mo stretchy="false">)</mml:mo></mml:math> the monoid with respect to composition of polynomial functions on <mml:math altimg="si6.gif" overflow="scroll"><mml:msub><mml:mrow><mml:mi mathvariant="double-struck">Z</mml:mi></mml:mrow><mml:mrow><mml:msup><mml:mrow><mml:mi>p</mml:mi></mml:mrow><mml:mrow><mml:mi>n</mml:mi></mml:mrow></mml:msup></mml:mrow></mml:msub></mml:math>. By monoid, we mean semigroup with an identity element."""))))
    }
    
}