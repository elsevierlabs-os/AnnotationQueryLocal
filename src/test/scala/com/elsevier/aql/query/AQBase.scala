package com.elsevier.aql.query

import org.scalatest.FunSuite
import scala.io.BufferedSource

abstract class AQBase extends FunSuite {

def getCaretFileAnnots(filename: String) :Array[String] = {
      val logger = org.apache.logging.log4j.LogManager.getLogger("Helpers")
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
   
  def getStrFile(filename: String) :String = {
      val logger = org.apache.logging.log4j.LogManager.getLogger("Helpers")
      var source: Option[BufferedSource] = None
      var str = ""
      // Get the str file  
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
      str   
  }
  
}