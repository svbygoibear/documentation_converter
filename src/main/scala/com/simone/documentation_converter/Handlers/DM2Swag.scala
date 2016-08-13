package com.simone.documentation_converter.Handlers

import com.simone.documentation_converter.Models.{ContentRow, DataModel}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/**
  * Created by Simone van Buuren on 2016/08/12.
  */

object DM2Swag {
  /** Creates data models for swagger*/
  //creates a single field
  def createField(cr: ContentRow): String = {
    s"\t\t\t${cr.name}:\n\t\t\t\ttype:${cr.dataType}\n\t\t\t\tdescription:${cr.description}\n"
  }

  //creates all the inner fields
  def createFields(fields: List[ContentRow]): String = {
    //val iets = createField(fields.get.head)
    fields match {
      case f =>
        def recurse(range: Range, fields: List[ContentRow], acc: StringBuilder): String = {
          if(range.isEmpty)
            acc.toString
          else
            recurse(range.tail, fields.tail, acc.append(createField(fields.head)))
        }
        recurse(f.indices, f, new StringBuilder("\t\tproperties:\n"))
      case _ => ""
    }
  }

  //creates the string indicating which fields are required; ToDo - remove if and else statements and pattern match,
  // ToDo - remove required if there is no required fields
  def createRequired(fields: List[ContentRow]): String = {
    fields match {
      case f =>
        def recurse(range: Range, fields: List[ContentRow], acc: StringBuilder): String = {
          if(range.isEmpty)
            acc.toString
          else
          if(fields.head.req.toLowerCase.contains("yes") || fields.head.req.toLowerCase.contains("true"))
            recurse(range.tail, fields.tail, acc.append(s"\t\t\t-${fields.head.name}\n"))
          else
            recurse(range.tail, fields.tail, acc)
        }
        recurse(f.indices, f, new StringBuilder("\t\trequired:\n"))
      case _ => ""
    }
  }

  //creates a single object definition; ToDo - remove if and else statements and pattern match
  def createObject(dm: DataModel): ListBuffer[String] = {
    ListBuffer(s"\t${dm.name}", "\t\ttype: object", s"\t\tdescription: ${dm.description}", s"${createRequired(dm.fields)}", s"${createFields(dm.fields)}")
  }

  //creates all the object definitions given
  def createObjects(implicit dms: Option[List[DataModel]]): Option[ListBuffer[ListBuffer[String]]] = {
    dms match {
      case Some(d) =>
        @tailrec
        def recurse(range: Range, dms: Option[List[DataModel]], acc: ListBuffer[ListBuffer[String]]): Option[ListBuffer[ListBuffer[String]]] = {
          if(range.isEmpty)
            Some(acc)
          else
            recurse(range.tail, dms, acc += createObject(d.head))
        }
        recurse(d.indices, dms, new ListBuffer())
      case None => None
    }
  }

  /** Creates swagger options*/
  implicit class StringItem (g: String){
    def replaceStrings(implicit objName: String, fMap: Map[String, String => String]) = {
      def recursive(s: String, lst: List[String]): String = {
        lst match {
          case Nil => s
          case head::tail => recursive(s.replace(head, fMap(head)(objName)), tail)
        }
      }
      recursive(g, fMap.keys.toList)
    }
  }

  def swaggerCreate(swagger : Option[List[String]])(implicit objName : String, fMap : Map[String, String => String]) = swagger match {
    case Some(swagList) => swagList match {
      case l => l.map(_.replaceStrings)
      case Nil => List()
    }
    case _ => List()
  }

  def genObjSwagger(template: Option[List[String]])(implicit dms: Option[List[DataModel]], fMap : Map[String, String => String]) = template match {
    case Some(temp) => dms match {
      case Some(dm) => dm.flatMap(d => swaggerCreate(template)(d.name, fMap))
      case _ => List()
    }
    case _ => List()
  }
}
