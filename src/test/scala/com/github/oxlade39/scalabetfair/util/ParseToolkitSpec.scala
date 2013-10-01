package com.github.oxlade39.scalabetfair.util

import org.specs2.mutable.Specification

/**
  * Created with IntelliJ IDEA.
 * User: pic
 * Date: 10/2/13
 * Time: 12:11 AM
 */
class ParseToolkitSpec extends Specification {

  import ParseToolkit._

  "escapedSplit" should {

    "split a string in a list of pieces" in {
      escapedSplit("ciao:miao", ":") mustEqual List("ciao", "miao")
    }
    "split a string in a list of pieces but join together the pieces separated by an escaped separator" in {
      escapedSplit("ciao:micio \\: miao", ":") mustEqual List("ciao", "micio : miao")
    }

  }

}