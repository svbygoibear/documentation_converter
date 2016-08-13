package com.simone.documentation_converter.Models

/**
  * Created by Simone van Buuren on 2016/08/12.
  */

//Case class for each individual property inside of a data model
case class ContentRow(name: String, description: String, req: String, dataType: String)

//Case class defining the data model structure
case class DataModel(name: String, description: String, fields: List[ContentRow])
