package com.simone.documentation_converter.Handlers

import com.simone.documentation_converter.Models.{ContentRow, DataModel}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/**
  * Created by Simone van Buuren on 2016/08/12.
  */

object Wiki2DM {
  def buildStartRegex(startString: String): String = {
    "^" + startString + "[a-zA-Z0-9_].*$"
  }

  //determine the start index per start text from a list of string
  def startIndexes(lst: Option[List[String]], startText: String): Option[List[Int]] = {
    lst match {
      case Some(l) => Some(l.zipWithIndex.filter(_._1 == startText).map(_._2))
      case None => None
    }
  }

  //determine the start index per start text from a list of string
  def startStrings(lst: Option[List[String]], startText: String): Option[List[String]] = {
    lst match {
      case Some(l) => Some(l.zipWithIndex.filter(_._1 == startText).map(_._1))
      case None => None
    }
  }

  //gets the start index and name after both start and end operators have been extracted (start and end being the same)
  def startIndexAndText(lst: Option[List[String]], startText: String): Option[List[(String, Int)]] = {
    lst match {
      case Some(l) => Some(l.zipWithIndex.filter(_._1.matches(buildStartRegex(startText))))
      case None => None
    }
  }

  //creates a single instance of content row
  def buildContentRow(item: List[String]): ContentRow = {
    ContentRow(item(0), item(1), item(2), item(3))
  }

  //used to assemble all the multiple instances of content rows; ToDo - remove trailing whitespaces
  def buildContentRows(lst: List[String], index: List[Int]): ListBuffer[ContentRow] = {
    lst match {
      case l =>
        @tailrec
        def recurse(range: Range, lst: List[String], index: List[Int], acc: ListBuffer[ContentRow]): ListBuffer[ContentRow] = {
          if(range.isEmpty || range == null)
            acc
          else
            recurse(range.tail, l, index.tail, acc += buildContentRow(l(index.head).substring(1).split("""\|\|""").toList))
        }
        recurse(index.indices, lst, index,  ListBuffer())
      case _ => ListBuffer()
    }
  }

  //builds a single instance of a datamodel and of its content rows; ToDo - checking if the extracted rows have enough fields
  def buildDataModel(lst: Option[List[String]], obj: Option[(String, Int)]): Option[DataModel] = {
    extractRows(lst, obj) match {
      case Some(er) =>
        val crIndexes = er.zipWithIndex.filter(_._1.contains("|-")).map(_._2 + 1)
        Some(DataModel(er.head.substring(2, er.head.length - 2), er(1), buildContentRows(er, crIndexes).toList ))
      case None => None
    }
  }

  //builds all the data models as needed
  def buildDataModels(lst: Option[List[String]], objs: Option[List[(String, Int)]]): Option[ListBuffer[DataModel]] = {
    lst match {
      case Some(l) =>
        objs match {
          case Some(o) =>
            @tailrec
            def recurse(range: Range, lst: Option[List[String]], objs: Option[List[(String, Int)]], acc: ListBuffer[DataModel]): Option[ListBuffer[DataModel]] = {
              if(range.isEmpty || range == null)
                Some(acc)
              else
                recurse(range.tail, lst, Some(o.tail), acc += buildDataModel(lst, Some(o.head)).get)
            }
            recurse(o.indices, lst, objs, new ListBuffer())
          case None => None
        }
      case None => None
    }
  }

  //extracts all the relevant rows from the list for the datamodel
  def extractRows(list: Option[List[String]], obj: Option[(String, Int)]) : Option[List[String]] = {
    list match {
      case Some(l) =>
        obj match {
          case Some(o) =>
            extractEndIndex(list, o._2, "|}") match {
              case Some(i) => Some(l.slice(l.size - i, l.size - o._2))
              case None => None
            }
          case None => None
        }
      case None => None
    }
  }

  //determines the end index used for extracting rows; this code is ugly and needs to be reworked
  def extractEndIndex(list: Option[List[String]], index: Int, endString: String) : Option[Int] = {
    list match {
      case Some(l) =>
        var done = false
        var i = index
        var res : Option[Int] = None

        while (i <= l.length && !done) {
          if(l(i).contains(endString)) {
            res = Some(i)
            done = true
          }
          i += 1
        }
        res
      case None => None
    }
  }
}
