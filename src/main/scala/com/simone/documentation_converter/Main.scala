package com.simone.documentation_converter

import com.simone.documentation_converter.Models.{ContentRow, DataModel}
import org.slf4j.LoggerFactory
import com.simone.documentation_converter.Handlers.File2List._
import com.simone.documentation_converter.Handlers.Wiki2DM._
import com.simone.documentation_converter.Handlers.DM2Swag._
import scala.tools.nsc.io._

/**
  * Created by Simone van Buuren on 2016/08/10.
  */

object Main extends App {
  val logger = LoggerFactory.getLogger("DocumentationConverterLogger")
  //File paths
  val filePath = "/Users/simonevanbuuren/git/FF General/ff_documentation-converter/src/main/resources/swagger_temps/swagger_path_temp.yaml"
  val filePathDocs = "/Users/simonevanbuuren/git/FF General/ff_documentation-converter/src/main/resources/swagger_temps/wiki_doc.txt"

  //List[String] from file path
  val swagger = fromFileToOptionList(filePath)
  val wiki = fromFileToOptionList(filePathDocs)

  //gets all the data model indexes and their names
  val indexAndNames = startIndexAndText(wiki, "==")

  //this value is used to create all the data models
  implicit val allModels: Option[List[DataModel]] = Some(buildDataModels(wiki, indexAndNames).get.toList)
  //list of all the lines to be written to a file for the swagger data model definitions
  val swaggerDataModels = createObjects

  //mapping for placeholders in the HTTP header options, used implicitly when calling genObjSwagger
  implicit val swaggyMap: Map[String, String => String] = Map[String, String => String](
    ("$&OBJECTPATH$&", doObjectPath),
    ("$&OBJECTPLURAL$&", doObjectPlural),
    ("$&OBJECTGET$&", doObjectGet),
    ("$&POSTOBJECT$&", doObjectPost),
    ("$&OBJECT$&", doObject),
    ("$&OBJECTGETUUID$&", doObjectGetUUID),
    ("$&DELETEOBJECT$&", doObjectDelete),
    ("$&PATCHOBJECT$&", doObjectPatch),
    ("$&PUTOBJECT$&", doObjectPut)
  )

  //methods for each definition to be mapped
  def doObjectPath(s: String) = s"${s.toLowerCase}s"
  def doObjectPlural(s: String) = s"${s}s"
  def doObjectGet(s: String) = s"${s}Get"
  def doObjectPost(s: String) = s"${s}Post"
  def doObject(s: String) = s
  def doObjectGetUUID(s: String) = s"${s}GetByUUID"
  def doObjectDelete(s: String) = s"${s}DeleteByUUID"
  def doObjectPatch(s: String) = s"${s}PatchByUUID"
  def doObjectPut(s: String) = s"${s}PutByUUID"

  //creates http swagger
  File("/Users/simonevanbuuren/git/FF General/ff_documentation-converter/src/main/resources/swagger_temps/rr.yaml").writeAll(genObjSwagger(swagger).mkString("\n"))
  //creates data models
  File("/Users/simonevanbuuren/git/FF General/ff_documentation-converter/src/main/resources/swagger_temps/dm.yaml").writeAll(swaggerDataModels.get.flatten.mkString("\n"))

}
