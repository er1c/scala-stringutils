package stringutils.generator

import better.files._
import scala.util.matching.Regex

/**
  * Generates a Scala ASCIIFolding.generated.scala based upon the Lucene ASCIIFoldingFilter.java
  * 
  * Run "sbt downloadLuceneAsciiFolding" to update the  ASCIIFoldingFilter.java from master branch.
  */
object GenerateASCIIFolding {
  private val javaFile: String = "lucene-solr/lucene/analysis/common/src/java/org/apache/lucene/analysis/miscellaneous/ASCIIFoldingFilter.java"
  private val scalaFile: File = File("core/shared/src/main/scala/stringutils/ASCIIFolding.generated.scala")
  private sealed trait Line

  private object Line {
    def parse(s: String): Option[Line] = UnicodeChar.parse(s) orElse ASCIIChar.parse(s)
  } 

  private object UnicodeChar {
    private val pattern: Regex = """          case '(\\u.+)': // (.+)""".r
    def parse(s: String): Option[UnicodeChar] = s match {
      case pattern(char, comment) => Some(UnicodeChar(char, comment))
      case _  => None
    }
  }

  private final case class UnicodeChar(char: String, comment: String) extends Line

  private object ASCIIChar {
    private val pattern: Regex = """            output\[outputPos\+\+\] = '(.)';""".r
    def parse(s: String): Option[ASCIIChar] = s match {
      case pattern(char) => Some(ASCIIChar(char.charAt(0)))
      case _ => None
    }
  }

  private final case class ASCIIChar(char: Char) extends Line

  private def getMapping: Vector[(UnicodeChar, String)] = {
    val lines: Vector[Line] = 
      Resource
        .asString(javaFile)
        .getOrElse{ throw new IllegalStateException(s"$javaFile not found.") }
        .linesIterator
        .flatMap{ Line.parse }
        .toVector

    val builder = Vector.newBuilder[(UnicodeChar, String)]

    var unicodeChars = Vector.newBuilder[UnicodeChar]
    var asciiChars = new StringBuilder()

    def reset(): Unit = {
      val ascii: String = asciiChars.result()
      unicodeChars.result().foreach{ unicode: UnicodeChar =>
        builder += ((unicode, ascii))
      }
      unicodeChars = Vector.newBuilder[UnicodeChar]
      asciiChars = new StringBuilder()
    }

    lines.foreach{
      case unicode: UnicodeChar => 
        if (asciiChars.nonEmpty) reset()
        unicodeChars += unicode
      case ASCIIChar(char) => asciiChars += char
    }

    builder.result()
  }

  def generate(): Unit = {
    val groupedByAscii: Vector[(String,Vector[UnicodeChar])] = 
      getMapping.groupBy{ _._2 }.mapValues{ _.map{ _._1 } }.toVector.sortBy{ _._1 }

    val maxExpandedLength: Int = groupedByAscii.map{ _._1.length }.max

    val maxLengthExamples: String = 
      groupedByAscii
        .filter{ _._1.length == maxExpandedLength }
        .flatMap { case (ascii: String, unicode: Vector[UnicodeChar]) =>
          unicode.map{ u: UnicodeChar => s"""'${u.char}' => "$ascii"  // ${u.comment}""" }
        }
        .mkString("", "\n    *   ", "")

    scalaFile.overwrite(s"""|/*
      | * Copyright (c) 2016 Frugal Mechanic (http://frugalmechanic.com)
      | * Copyright (c) 2020 the scala-stringutils contributors.
      | *
      | * Licensed under the Apache License, Version 2.0 (the "License");
      | * you may not use this file except in compliance with the License.
      | * You may obtain a copy of the License at
      | *
      | *     http://www.apache.org/licenses/LICENSE-2.0
      | *
      | * Unless required by applicable law or agreed to in writing, software
      | * distributed under the License is distributed on an "AS IS" BASIS,
      | * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
      | * See the License for the specific language governing permissions and
      | * limitations under the License.
      | */
      |
      |// Generated ${new java.util.Date()}
      |// AUTO GENERATED - DO NOT EDIT
      |// AUTO GENERATED - DO NOT EDIT
      |// AUTO GENERATED - DO NOT EDIT
      |// AUTO GENERATED - DO NOT EDIT
      |// AUTO GENERATED - DO NOT EDIT
      |// AUTO GENERATED - DO NOT EDIT
      |
      |package stringutils
      |
      |import java.lang.{StringBuilder => JavaStringBuilder}
      |import scala.annotation.switch
      |
      |object ASCIIFolding {
      |  /**
      |    * The maximum number of ASCII characters a single Unicode character will expand to
      |    *
      |    * All examples that expands to $maxExpandedLength characters:
      |    *
      |    *   $maxLengthExamples
      |    */
      |  val MaxASCIIExpandedLength: Int = $maxExpandedLength
      |
      |  /**
      |    * Converts Accented Characters to a Non-Accented Equivalent Char
      |    * or returns back the original Char if no mapping available.
      |    *
      |    * Note: This only works for when there is a 1 to 1 Character equivalence
      |    * (i.e. it does not work for stuff like Æ which needs to expand to AE)
      |    */
      |  def foldAsChar(c: Char): Char = {
      |    // This is potentially more JIT friendly since the JVM should be able
      |    // to inline this method and will almost always hit the common case
      |    // of just returning the original character.  The slower path will be
      |    // calling foldAccentCharImpl()
      |    if (c < '\\u0080') c else foldAccentCharImpl(c)
      |  }
      |
      |  /**
      |    * Converts Accented Characters to a Non-Accented Equivalent String or
      |    * returns back the original Char as a String if no mapping available.
      |    */
      |  def fold(c: Char): String = {
      |    if (c < '\\u0080') c.toString
      |    else {
      |      val s: String = foldAccentStringImplOrNull(c)
      |      if (null == s) c.toString
      |      else s
      |    }
      |  }
      |
      |  /**
      |    * Converts Accented Characters to the Non-Accented Equivalent String
      |    * (or None if already ASCII or no conversionn exists).
      |    */
      |  def get(c: Char): Option[String] = {
      |    if (c < '\\u0080') None
      |    else Option(foldAccentStringImplOrNull(c))
      |  }
      |
      |  /**
      |    * Converts Accented Characters to the Non-Accented Equivalent String
      |    * (or null if already ASCII or no conversion exists).
      |    */
      |  def getOrNull(c: Char): String = {
      |    if (c < '\\u0080') null
      |    else foldAccentStringImplOrNull(c)
      |  }
      |
      |  /**
      |    * Converts Accented Characters to the Non-Accented Equivalent String.
      |    *
      |    * Note: This expands stuff like Æ to AE)
      |    */
      |  def fold(s: String): String = {
      |    if (null == s) return ""
      |
      |    var i: Int = 0
      |
      |    // Skip past any ASCII characters
      |    while (i < s.length && s.charAt(i) < '\\u0080') {
      |      i += 1
      |    }
      |
      |    // If we made it through the entire string then there are no accents
      |    // otherwise we need to switch to foldStartingAt
      |    if (i == s.length) s
      |    else foldStartingAt(s, i)
      |  }
      |
      |  private def foldStartingAt(s: String, idx: Int): String = {
      |    val sb: JavaStringBuilder = new JavaStringBuilder(s.length)
      |
      |    // Add anything up to idx
      |    sb.append(s, 0, idx)
      |
      |    var i: Int = idx
      |
      |    while (i < s.length) {
      |      foldAppend(s.charAt(i), sb)
      |      i += 1
      |    }
      |
      |    sb.toString()
      |  }
      |
      |  /**
      |    * Converts Accented Characters to the Non-Accented Equivalent String.
      |    */
      |  private def foldAppend(c: Char, sb: JavaStringBuilder): Unit = {
      |    // This is potentially more JIT friendly since the JVM should be able
      |    // to inline this method and will almost always hit the common case
      |    // of just returning the original character.  The slower path will be
      |    // calling foldAccentStringImpl()
      |    if (c < '\\u0080') {
      |      sb.append(c)
      |    } else {
      |      val str: String = foldAccentStringImplOrNull(c)
      |
      |      if (null == str) sb.append(c)
      |      else sb.append(str)
      |    }
      |
      |    ()
      |  }
      |// format: off
      |${toASCIICharImpl(groupedByAscii)}
      |
      |${toASCIIStringImpl(groupedByAscii)}
      |// format: on
      |}
      |""".stripMargin
    )

    ()
  }

  private def toASCIICharImpl(groupedByAscii: Vector[(String,Vector[UnicodeChar])]): String = {
    val sb = new StringBuilder()
    def println(s: String = ""): Unit = { sb.append(s+"\n"); () }

    println("  /** Generated From Lucene's ASCIIFoldingFilter.java */")
    println("  private def foldAccentCharImpl(c: Char): Char = {")
    println("    // Quick test: if it's not in range then just keep current character")
    println("    if (c < '\\u0080') {")
    println("      c")
    println("    } else {")
    println("      (c: @switch) match {")

    groupedByAscii
      .filter{ case (ascii, _) => ascii.length == 1 }
      .foreach{ case (ascii, unicodeChars) => 
        println()
        println(s"        // ASCII: $ascii")
        println()
        unicodeChars.foreach{ unicode: UnicodeChar => println (s"        // ${unicode.comment}") }
        println(s"""        case ${unicodeChars.map{ _.char }.mkString("'","' | '","'")} => '${escapeChar(ascii)}'""")
      }

    println()
    println("        case _ => c // Default")
    println("      }")
    println("    }")
    println("  }")

    sb.result()
  }


  private def toASCIIStringImpl(groupedByAscii: Vector[(String,Vector[UnicodeChar])]): String = {
    val sb = new StringBuilder()
    def println(s: String = ""): Unit = { sb.append(s+"\n"); () }
    
    println()
    println()
    println("  /** Generated From Lucene's ASCIIFoldingFilter.java */")
    println("  private def foldAccentStringImplOrNull(c: Char): String = {")
    println("    if (c < '\\u0080') {")
    println("      null")
    println("    } else {")
    println("      (c: @switch) match {")

    groupedByAscii.foreach{ case (ascii, unicodeChars) => 
      println()
      println(s"        // ASCII: $ascii")
      println()
      unicodeChars.foreach{ unicode: UnicodeChar => println (s"        // ${unicode.comment}") }
      println(s"""        case ${unicodeChars.map{ _.char }.mkString("'","' | '","'")} => "${escapeString(ascii)}"""")
    }

    println()
    println("        case _ => null // avoid the String memory allocation from calling c.toString")
    println("      }")
    println("    }")
    println("  }")

    sb.result()
  }

  private def escapeString(s: String): String = {
    s.flatMap{ 
      case '"' => "\\\""
      case ch => ch.toString
    }
  }

  private def escapeChar(s: String): String = {
    require(s.length == 1)
    val ch: Char = s.charAt(0)
    
    if (ch == '\'') "\'"
    else ch.toString
  }
}