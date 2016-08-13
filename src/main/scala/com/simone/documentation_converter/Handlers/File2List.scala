package com.simone.documentation_converter.Handlers

import com.simone.documentation_converter.Helpers.Control._

/**
  * Created by Simone van Buuren on 2016/08/12.
  */

object File2List {
  //returns the file data as Option[List[String]]
  def fromFileToOptionList(filePath: String): Option[List[String]] = {
    try
      Some(using(io.Source.fromFile(filePath)) { source =>
        (for (line <- source.getLines) yield line).toList
      })
    catch {
      case e: Exception => None
    }
  }

  //ToDo - write data model objects
}
