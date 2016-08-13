package com.simone.documentation_converter.Handlers

import com.simone.documentation_converter.Models.{ContentRow, DataModel}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/**
  * Created by Simone van Buuren on 2016/08/12.
  */

object Wiki2DM {
  def buildStartRegex(startString: String): String = "^" + startString + "[a-zA-Z0-9_].*$"

  // Determine the start index per start text from a list of string
  def startIndexes(lst: Option[List[String]], startText: String): Option[List[Int]] = lst match {
      case Some(l) => Some(l.zipWithIndex.filter(_._1 == startText).map(_._2))
      case None => None
  }

  // Determine the start index per start text from a list of string
  def startStrings(lst: Option[List[String]], startText: String): Option[List[String]] = lst match {
      case Some(l) => Some(l.zipWithIndex.filter(_._1 == startText).map(_._1))
      case None => None
    }

  // Gets the start index and name after both start and end operators have been extracted (start and end being the same)
  def startIndexAndText(lst: Option[List[String]], startText: String): Option[List[(String, Int)]] = lst match {
      case Some(l) => Some(l.zipWithIndex.filter(_._1.matches(buildStartRegex(startText))))
      case None => None
    }

  // Creates a single instance of content row
  def buildContentRow(item: List[String]): ContentRow = {
    ContentRow(item.head, item(1), item(2), item(3))
  }

  // Used to assemble all the multiple instances of content rows
  // ToDo - remove trailing whitespaces
  def buildContentRows(lst: List[String], index: List[Int]): ListBuffer[ContentRow] = lst match {
      case l => def recurse(range: Range, lst: List[String], index: List[Int], acc: ListBuffer[ContentRow]): ListBuffer[ContentRow] = {
          if(range.isEmpty || range == null) acc
          else recurse(range.tail, l, index.tail, acc += buildContentRow(l(index.head).substring(1).split("""\|\|""").toList))
        }
        recurse(index.indices, lst, index,  ListBuffer())
      case _ => ListBuffer()
    }

  // Builds a single instance of a datamodel and of its content rows
  // ToDo - checking if the extracted rows have enough fields
  def buildDataModel(lst: Option[List[String]], obj: Option[(String, Int)]): Option[DataModel] = {
    extractRows(lst.getOrElse(List()), obj) match {
      case er =>
        val crIndexes = er.zipWithIndex.filter(_._1.contains("|-")).map(_._2 + 1)
        Some(DataModel(er.head.substring(2, er.head.length - 2), er(1), buildContentRows(er, crIndexes).toList ))
      case _ => None
    }
  }

  // Builds all the data models as needed
  def buildDataModels(lst: Option[List[String]], objs: Option[List[(String, Int)]]): Option[ListBuffer[DataModel]] = lst match {
      case Some(l) => objs match {
          case Some(o) => def recurse(range: Range, ilst: List[String], objs: List[(String, Int)], acc: ListBuffer[DataModel]): ListBuffer[DataModel] = {
              if(range.isEmpty || range == null) acc
              else recurse(range.tail, l, o.tail, acc += buildDataModel(lst, Some(o.head)).get)
            }
            Some(recurse(o.indices, l, o, new ListBuffer()))
          case None => None
        }
      case None => None
    }

  // Determines the end index used for extracting rows
  def extractEndIndex(list: List[String], index: Int, endString: String) : Option[Int] = list match {
      case l => def recursive(list: List[String], index: Int, endString: String, i: Int): Option[Int] = list(i).contains(endString) match {
            case true => Some(i)
            case false => recursive(list, index, endString, i + 1)
          }
        recursive(list, index, endString, index)
      case _ => None
    }

  // Extracts all the relevant rows from the list for the data model
  def extractRows(list: List[String], obj: Option[(String, Int)]) : List[String] = list match {
      case l => obj match {
          case Some(o) => extractEndIndex(l, o._2, "|}") match {
              case Some(i) => l.slice(l.size - i, l.size - o._2)
              case None => List()
            }
          case None => List()
        }
      case _ => List()
    }
}
