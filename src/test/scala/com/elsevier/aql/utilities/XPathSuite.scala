package com.elsevier.aql.utilities

import com.elsevier.aql.annotations.AQAnnotation

class XPathSuite extends AQBase {
    
    test("XPath - Test 1") {
      val omAnnots: Array[AQAnnotation] = GetAQAnnotations(getCaretFileAnnots("src/test/resources/om/S0022314X13001777"),"S0022314X13001777",Array("*"),decodeProps=Array("*"))
      val xpathAnnots = XPath(omAnnots,"src/test/resources/om/")
      /*
      var i=0
      for (xpathAnnot <- xpathAnnots) {
        println((i,xpathAnnot.properties.getOrElse(Map.empty).getOrElse("xpath","")))
        i+=1
      }
      */
      
      assert(xpathAnnots(8631).properties.getOrElse(Map.empty).getOrElse("xpath","") == "/xocs:doc[1]/xocs:serial-item[1]/ja:article[1]/ja:body[1]/ce:sections[1]/ce:section[5]/ce:para[3]/ce:enunciation[1]/ce:para[1]/mml:math[9]/mml:msub[1]/mml:mrow[1]/mml:mi[1]")
      assert(xpathAnnots(9700).properties.getOrElse(Map.empty).getOrElse("xpath","") == "/xocs:doc[1]/xocs:serial-item[1]/ja:article[1]/ja:tail[1]/ce:bibliography[1]/ce:bibliography-sec[1]/ce:bib-reference[17]/sb:reference[1]/sb:host[1]/sb:pages[1]/sb:first-page[1]")
    }

      
    test("XPath - Test 2") {
      val omAnnots: Array[AQAnnotation] = GetAQAnnotations(getCaretFileAnnots("src/test/resources/om/S0022314X13001777"),"S0022314X13001777",Array("*"),decodeProps=Array("*"))
      val xpathAnnots = XPath(omAnnots,"src/test/resources/junk/")
      assert(xpathAnnots(1593).properties.getOrElse(Map.empty).getOrElse("xpath","") == "ERR")
      assert(xpathAnnots(2811).properties.getOrElse(Map.empty).getOrElse("xpath","") == "ERR")
    }
}