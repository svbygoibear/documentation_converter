package com.simone.documentation_converter.Helpers

/**
  * Created by Simone van Buuren on 2016/08/12.
  */

//Used to help with memory management of opening and closing of resources such as text files
object Control {
  def using[A <: { def close(): Unit }, B](resource: A)(f: A => B): B =
    try
      f(resource)
    finally
      resource.close()
}
