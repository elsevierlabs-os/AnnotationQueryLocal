package com.elsevier.aql.query

import org.scalatest.FunSuite

import com.elsevier.aql.annotations.AQAnnotation
import com.elsevier.aql.utilities.GetAQAnnotations

import scala.io.BufferedSource

class QuerySuite extends FunSuite {
  
   def getCaretFileAnnots(filename: String) :Array[String] = {
      val logger = org.apache.logging.log4j.LogManager.getLogger("QuerySuite")
      var source: Option[BufferedSource] = None
      var str = ""
      // Get the document of caret delimited annotations.  
      try {
        source = Some(scala.io.Source.fromFile(filename,"utf-8"))
        str = source.get.mkString
      } catch {
        case e: Exception => logger.error("Unable to find document: " + filename)
      } finally {
        if (source != None) {
          source.get.close()
        } 
      }
      str.split("\n")
  } 
   
  // Original Markup Annotations
  val omAnnots: Array[AQAnnotation] = GetAQAnnotations(getCaretFileAnnots("src/test/resources/om/S0022314X13001777"),"S0022314X13001777",Array("orig"),Array("orig"),Array("orig"))
  
  // Genia Annotations
  val geniaAnnots: Array[AQAnnotation] = GetAQAnnotations(getCaretFileAnnots("src/test/resources/genia/S0022314X13001777"),"S0022314X13001777",Array("orig","lemma","pos"),Array("lemma","pos"),Array("orig","lemma"))
  
  // Combined Annotations
  val annots = (omAnnots ++ geniaAnnots).sortBy { x => (x.startOffset, x.endOffset) }
  
  // Test FilterProperty
  
  test("FilterProperty(annots,'orig','and')") {
    assert(FilterProperty(annots,"orig","and").size == 108)
  }
  
  test("FilterProperty(annots,'orig','and',limit=5)") {
    assert(FilterProperty(annots,"orig","and",limit=5).size == 5)
  }  
  
  test("FilterProperty(annots,'orig','and',not=true)") {
    assert(FilterProperty(annots,"orig","and",not=true).size == 3907)
  }    
  
  test("FilterProperty(annots,'orig','and',valueCompare='!=')") {
    assert(FilterProperty(annots,"orig","and",valueCompare="!=").size == 3907)
  }
  
  test("FilterProperty(annots,'orig',valueArr=Array('and','to'))") {
    assert(FilterProperty(annots,"orig",valueArr=Array("and","to")).size == 153)
  }  
  
  // Test RegexProperty
  
  test("RegexProperty(annots,'orig','.*and.*')") {
    assert(RegexProperty(annots,"orig",".*and.*").size == 135)
  }
  
  test("RegexProperty(annots,'orig','.*and.*',not=true)") {
    assert(RegexProperty(annots,"orig",".*and.*",not=true).size == 3880)
  }
  
  // Test FilterSet 
  
  test("FilterSet(annots,'ge')") {
    assert(FilterSet(annots,"ge").size == 4066)
  }   
  
  test("FilterSet(annots,'om')") {
    assert(FilterSet(annots,"om").size == 9754)
  }    
  
  test("FilterSet(annots,Array('ge','om'))") {
    assert(FilterSet(annots,annotSetArr=Array("ge","om")).size == 13820)
  } 
  
  // Test FilterType 
  
  test("FilterType(annots,'sentence')") {
    assert(FilterType(annots,"sentence").size == 128)
  }    

  test("FilterType(annots,'ce:para')") {
    assert(FilterType(annots,"ce:para").size == 142)
  } 
  
  test("FilterType(annots,Array('sentence','ce:para'))") {
    assert(FilterType(annots,annotTypeArr=Array("sentence","ce:para")).size == 270)
  }
  
  // Test Contains

  test("Contains(FilterType(annots,'ce:para'),FilterType(annots,'sentence'))") {
    assert(Contains(FilterType(annots,"ce:para"),FilterType(annots,"sentence")).size == 138)
  }  
  
  test("Contains(FilterType(annots,'ce:para'),FilterType(annots,'sentence'),not=true)") {
    assert(Contains(FilterType(annots,"ce:para"),FilterType(annots,"sentence"),not=true).size == 4)
  } 
  
  // Test ContainedIn
  
  test("ContainedIn(FilterType(annots,'sentence'),FilterType(annots,'ce:para'))") {
    assert(ContainedIn(FilterType(annots,"sentence"),FilterType(annots,"ce:para")).size == 126)
  }   
  
  test("ContainedIn(FilterType(annots,'sentence'),FilterType(annots,'ce:para'),not=true)") {
    assert(ContainedIn(FilterType(annots,"sentence"),FilterType(annots,"ce:para"),not=true).size == 2)
  }  
    
  // Test Before
  
  test("Before(FilterProperty(annots,'orig','polynomial'),FilterProperty(annots,'orig','function'))") {
    assert(Before(FilterProperty(annots,"orig","polynomial"),FilterProperty(annots,"orig","function")).size == 47)
  }     
  
  test("Before(FilterProperty(annots,'orig','polynomial'),FilterProperty(annots,'orig','function'),dist=50)") {
    assert(Before(FilterProperty(annots,"orig","polynomial"),FilterProperty(annots,"orig","function"),dist=50).size == 11)
  }  
  
  test("Before(FilterProperty(annots,'orig','polynomial'),FilterProperty(annots,'orig','function'),dist=50,not=true)") {
    assert(Before(FilterProperty(annots,"orig","polynomial"),FilterProperty(annots,"orig","function"),dist=50,not=true).size == 36)
  }  
  
  // Test After
  
  test("After(FilterProperty(annots,'orig','function'),FilterProperty(annots,'orig','polynomial'))") {
    assert(After(FilterProperty(annots,"orig","function"),FilterProperty(annots,"orig","polynomial")).size == 27)
  }   

  test("After(FilterProperty(annots,'orig','function'),FilterProperty(annots,'orig','polynomial'),dist=50)") {
    assert(After(FilterProperty(annots,"orig","function"),FilterProperty(annots,"orig","polynomial"),dist=50).size == 9)
  }  
  
  test("After(FilterProperty(annots,'orig','function'),FilterProperty(annots,'orig','polynomial'),dist=50,not=true)") {
    assert(After(FilterProperty(annots,"orig","function"),FilterProperty(annots,"orig","polynomial"),dist=50,not=true).size == 18)
  }   
  
  // Test Between
  
  test("Between(FilterProperty(annots,'orig','permutations'),FilterProperty(annots,'orig','polynomial'),FilterProperty(annots,'orig','integers'))") {
    assert(Between(FilterProperty(annots,"orig","permutations"),FilterProperty(annots,"orig","polynomial"),FilterProperty(annots,"orig","integers")).size == 2)
  }   
  
  test("Between(FilterProperty(annots,'orig','permutations'),FilterProperty(annots,'orig','polynomial'),FilterProperty(annots,'orig','integers'),dist=50)") {
    assert(Between(FilterProperty(annots,"orig","permutations"),FilterProperty(annots,"orig","polynomial"),FilterProperty(annots,"orig","integers"),dist=50).size == 2)
  } 
  
  test("Between(FilterProperty(annots,'orig','permutations'),FilterProperty(annots,'orig','polynomial'),FilterProperty(annots,'orig','integers'),dist=50,not=true)") {
    assert(Between(FilterProperty(annots,"orig","permutations"),FilterProperty(annots,"orig","polynomial"),FilterProperty(annots,"orig","integers"),dist=50,not=true).size == 16)
  }   
  
  // Test Sequence

  test("Sequence(FilterProperty(annots,'orig','to'),FilterProperty(annots,'orig','be'),dist=5)") {
    assert(Sequence(FilterProperty(annots,"orig","to"),FilterProperty(annots,"orig","be"),dist=5).size == 5)
  }   
    
  // Test Or

  test("Or(omAnnots,geniaAnnots)") {
    assert(Or(omAnnots,geniaAnnots).size == 13820)
  }  
    
  // Test And  

  test("And(omAnnots,omAnnots)") {
    assert(And(omAnnots,omAnnots).size == 9754)
  }   
  
  test("And(omAnnots,omAnnots,not=true)") {
    assert(And(omAnnots,omAnnots,not=true).size == 0)
  }   
  
  test("And(omAnnots,geniaAnnots)") {
    assert(And(omAnnots,geniaAnnots).size == 9754)
  }     

  test("And(omAnnots,geniaAnnots,not=true)") {
    assert(And(omAnnots,geniaAnnots,not=true).size == 0)
  }     
  
  test("And(omAnnots,omAnnots,leftOnly=false)") {
    assert(And(omAnnots,omAnnots,leftOnly=false).size == 9754)
  }   
  
  test("And(omAnnots,geniaAnnots,leftOnly=false)") {
    assert(And(omAnnots,geniaAnnots,leftOnly=false).size == 13820)
  }
  
  // Test MatchProperty 

  test("MatchProperty(omAnnots,FilterType(omAnnots,'xocs:doi','orig')") {
    assert(MatchProperty(omAnnots,FilterType(omAnnots,"xocs:doi"),"orig").size == 1)
  }    
    
  // Test Preceding

  test("Preceding(FilterType(annots,'sentence'),Contains(FilterType(annots,'sentence'),FilterProperty(annots,'orig','function')))") { 
    val result = Preceding(FilterType(annots,"sentence"),Contains(FilterType(annots,"sentence"),FilterProperty(annots,"orig","function")))(0)
    assert(result._2.size == 2)
    assert(result._1 == AQAnnotation("S0022314X13001777","ge","sentence",19649,19739,59,None))
    assert(result._2(0) == AQAnnotation("S0022314X13001777","ge","sentence",19280,19471,14,None))
    assert(result._2(1) == AQAnnotation("S0022314X13001777","ge","sentence",18546,18607,1,None))
  }
  
  test("Preceding(FilterType(annots,'sentence'),Contains(FilterType(annots,'sentence'),FilterProperty(annots,'orig','function')),FilterType(annots,'ce:para'))") {
    val result = Preceding(FilterType(annots, "sentence"), Contains(FilterType(annots, "sentence"), FilterProperty(annots, "orig", "function")),FilterType(annots,"ce:para"))
                 .sortBy{ x => (x._1.startOffset, x._1.endOffset) }
    assert(result(0)._2.size == 0)
    assert(result(0)._1 == AQAnnotation("S0022314X13001777","ge","sentence",19649,19739,59,None))
  }
  
  test("Preceding(FilterType(annots,'none'),Contains(FilterType(annots,'sentence'),FilterProperty(annots,'orig','function')),FilterType(annots,'ce:para'))") {
    val result = Preceding(FilterType(annots, "none"), Contains(FilterType(annots, "sentence"), FilterProperty(annots, "orig", "function")),FilterType(annots,"ce:para"))
                 .sortBy{ x => (x._1.startOffset, x._1.endOffset) }
    assert(result.size == 21)
    assert(result(0)._1 == AQAnnotation("S0022314X13001777","ge","sentence",19649,19739,59,None))
    assert(result(0)._2.isEmpty == true)
  }
  
  // Test Following
  
  test("Following(FilterType(annots,'sentence'),Contains(FilterType(annots,'sentence'),FilterProperty(annots,'orig','function')))") { 
    val result = Following(FilterType(annots,"sentence"),Contains(FilterType(annots,"sentence"),FilterProperty(annots,"orig","function")),cnt=20)
    assert(result(0)._2.size == 20)
    assert(result(0)._1 == AQAnnotation("S0022314X13001777","ge","sentence",19649,19739,59,None))
    assert(result(0)._2(0) == AQAnnotation("S0022314X13001777","ge","sentence",19740,19911,80,None))
    assert(result(0)._2(1) == AQAnnotation("S0022314X13001777","ge","sentence",19912,20133,119,None))
    assert(result(0)._2(2) == AQAnnotation("S0022314X13001777","ge","sentence",20134,20311,167,None))
  }

  test("Following(FilterType(annots,'sentence'),Contains(FilterType(annots,'sentence'),FilterProperty(annots,'orig','function')),FilterType(annots,'ce:para'),20)") {
    val result = Following(FilterType(annots, "sentence"), Contains(FilterType(annots, "sentence"), FilterProperty(annots, "orig", "function")),FilterType(annots,"ce:para"),20)
                 .sortBy{ x => (x._1.startOffset, x._1.endOffset) }
    assert(result(0)._2.size == 3)
    assert(result(0)._1 == AQAnnotation("S0022314X13001777","ge","sentence",19649,19739,59,None))
    assert(result(0)._2(0) == AQAnnotation("S0022314X13001777","ge","sentence",19740,19911,80,None))
    assert(result(0)._2(1) == AQAnnotation("S0022314X13001777","ge","sentence",19912,20133,119,None))
    assert(result(0)._2(2) == AQAnnotation("S0022314X13001777","ge","sentence",20134,20311,167,None))
  }   
  
  test("Following(FilterType(annots,'none'),Contains(FilterType(annots,'sentence'),FilterProperty(annots,'orig','function')),FilterType(annots,'ce:para'),20)") {
    val result = Following(FilterType(annots, "none"), Contains(FilterType(annots, "sentence"), FilterProperty(annots, "orig", "function")),FilterType(annots,"ce:para"),20)
                 .sortBy{ x => (x._1.startOffset, x._1.endOffset) }
    assert(result.size == 21)
    assert(result(0)._1 == AQAnnotation("S0022314X13001777","ge","sentence",19649,19739,59,None))
    assert(result(0)._2.isEmpty == true)
  }
 
}