package com.elsevier.aql.utilities

import scala.collection.mutable.MutableList
import scala.collection.mutable.ListBuffer
import java.net.URLDecoder
import scala.io.BufferedSource

import com.elsevier.aql.annotations.AQAnnotation
import com.elsevier.aql.query.BinaryOp
import com.elsevier.aql.query.FilterType
import com.elsevier.aql.query.FilterProperty

/**
 * This function will calculate the xpath expression for each Original Markup (OM) AQAnnotation in the passed OM Array[AQAnnotation], populate the xpath property with this value in the AQAnnotation, and return a Array[AQAnnotation] with the xpath property populated.  
 */
object XPath {
  
def apply(arr: Array[AQAnnotation], omBase: String): Array[AQAnnotation] = {

    //  Get the Caret annotations for the specified OM file
    def getCaretFileAnnots(omBase: String, filename: String) :Array[String] = {
      import java.nio.file.{Files, Paths}
      import java.util.zip.GZIPInputStream
      import java.io._
      import org.apache.commons.io.IOUtils
    
      var byteArray: Option[Array[Byte]] = None
      var result = ""
      
      try {
        byteArray = Some(Files.readAllBytes(Paths.get(omBase + filename)))
      } catch {
        case e: Exception =>  System.out.println("Problems reading file.")
      }

      if (byteArray != None) {
        val magic: Int = byteArray.get(0) & 0xff | (byteArray.get(1) << 8) & 0xff00
        if (magic == GZIPInputStream.GZIP_MAGIC) {
          // File is gzipped
          try {
            val gis = new GZIPInputStream(new ByteArrayInputStream(byteArray.get))
            val bufferedReader = new BufferedReader(new InputStreamReader(gis, "UTF-8"))
            result =IOUtils.toString(bufferedReader)
          } catch {
            case e: Exception =>  System.out.println("Problems decompressing file." + e.getMessage) 
          }
        } else {
          // File is not gzipped
          val bufferedReader = new BufferedReader(new InputStreamReader(new ByteArrayInputStream(byteArray.get),"UTF-8"))
          result = IOUtils.toString(bufferedReader)
        }
      }
      result.split("\n")
    } 
    
    
  
    // Get the om annotations with the same annotType (element name) and  parentId.  This indicates they are siblings as they have the same parent.
    def getSibling(target:AQAnnotation, omAnnots: Array[AQAnnotation]): String = {
      var xpath = target.annotType
      var idx = 1
      val name = target.annotType
      val parentId = target.properties.getOrElse(Map.empty).getOrElse("parentId","")
      val siblings = FilterProperty(FilterType(omAnnots,name),"parentId",parentId)
      var found = false
      for (sibling <- siblings if !found) {
        if (sibling.startOffset == target.startOffset && sibling.endOffset == target.endOffset) {
          found = true
        } else {
          idx += 1
        }
      }
      return xpath + "[" + idx + "]"
    }
   
    // Calculate the xpath expression for the annotation
    def getXPath(target: AQAnnotation, omAnnots: Array[AQAnnotation]): String = {
      if (omAnnots.size == 0) return "ERR"
      val xPathBuff : ListBuffer[String] = new ListBuffer[String]()
      val targetOrigAnnotID = target.properties.getOrElse(Map.empty).getOrElse("origAnnotID","").toInt
      val parents = BinaryOp(omAnnots,
                             Array(target),    
                             (l, r) => l.startOffset <= r.startOffset &&
                                                        l.endOffset >= r.endOffset &&
                                                        l.properties.getOrElse(Map.empty).getOrElse("parentId","0").toInt < r.properties.getOrElse(Map.empty).getOrElse("parentId","0").toInt)
      //val parents = Contains(omAnnots,Array(target)).  Can't use the default Contains so reimpmlemented above a function that meets our needs.
      for (parent <- parents) {
        // Make sure parent is <= target origAnnotId
        if (parent.properties.getOrElse(Map.empty).getOrElse("parentId","0").toInt < targetOrigAnnotID) {
          xPathBuff += (getSibling(parent,omAnnots))
        }
      }
      return "/" + xPathBuff.mkString("/") + "/" + getSibling(target,omAnnots)
    }
  
    var lastDoc= ""
    var omAnnots: Array[AQAnnotation] = Array()
    
    arr.map(ann => {
      var currDoc = ann.docId
      if (!currDoc.equals(lastDoc)) {          
        omAnnots = GetAQAnnotations(getCaretFileAnnots(omBase,currDoc),currDoc,Array("*"),Array("*"),Array("*"))
        lastDoc = currDoc
      }
      val xpath = getXPath(ann,omAnnots)
      var props = ann.properties.getOrElse(Map.empty)
      props += ("xpath" -> xpath)
      new AQAnnotation(ann.docId,                       // Map to AQAnnotation
                       ann.annotSet,
                       ann.annotType,
                       ann.startOffset,
                       ann.endOffset,
                       ann.annotId,
                       Some(props))
    })
    
  }
}   

