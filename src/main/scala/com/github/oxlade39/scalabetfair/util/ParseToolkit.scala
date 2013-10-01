package com.github.oxlade39.scalabetfair.util

/**
 * Created with IntelliJ IDEA.
 * User: pic
 * Date: 10/2/13
 * Time: 12:08 AM
 */
object ParseToolkit {

  def escapedSplit(string: String, separator: String, limit: Int = 0): List[String] = {
    string.split(separator, limit).foldRight(List[String]()) { (e: String, l: List[String]) =>
      l match {
        case Nil => List(e)
        case head :: tail => if (e.endsWith("\\"))
          (e.dropRight(1) + separator + head) :: tail
        else
          e :: l
      }
    }
  }

}
