/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.commons.lang3.text.translate

import java.io.IOException
import java.io.StringWriter
import java.io.Writer
import java.util.Locale

/**
  * An API for translating text.
  * Its core use is to escape and unescape text. Because escaping and unescaping
  * is completely contextual, the API does not present two separate signatures.
  *
  * @since 3.0
  * @deprecated as of 3.6, use commons-text
  *             <a href="https://commons.apache.org/proper/commons-text/javadocs/api-release/org/apache/commons/text/translate/CharSequenceTranslator.html">
  *             CharSequenceTranslator</a> instead
  */
@deprecated object CharSequenceTranslator {
  private[translate] val HEX_DIGITS =
    Array('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F')

  /**
    * <p>Returns an upper case hexadecimal {@code String} for the given
    * character.</p>
    *
    * @param codepoint The codepoint to convert.
    * @return An upper case hexadecimal {@code String}
    */
  def hex(codepoint: Int): String = Integer.toHexString(codepoint).toUpperCase(Locale.ENGLISH)
}

@deprecated abstract class CharSequenceTranslator {
  protected def hex(codepoint: Int): String = CharSequenceTranslator.hex(codepoint)
  protected def HEX_DIGITS: Array[Char] = CharSequenceTranslator.HEX_DIGITS
  /**
    * Translate a set of codepoints, represented by an int index into a CharSequence,
    * into another set of codepoints. The number of codepoints consumed must be returned,
    * and the only IOExceptions thrown must be from interacting with the Writer so that
    * the top level API may reliably ignore StringWriter IOExceptions.
    *
    * @param input CharSequence that is being translated
    * @param index int representing the current point of translation
    * @param out   Writer to translate the text to
    * @return int count of codepoints consumed
    * @throws IOException if and only if the Writer produces an IOException
    */
  @throws[IOException]
  def translate(input: CharSequence, index: Int, out: Writer): Int

  /**
    * Helper for non-Writer usage.
    *
    * @param input CharSequence to be translated
    * @return String output of translation
    */
  final def translate(input: CharSequence): String = {
    if (input == null) return null
    try {
      val writer = new StringWriter(input.length * 2)
      translate(input, writer)
      writer.toString
    } catch {
      case ioe: IOException =>
        // this should never ever happen while writing to a StringWriter
        throw new RuntimeException(ioe)
    }
  }

  /**
    * Translate an input onto a Writer. This is intentionally final as its algorithm is
    * tightly coupled with the abstract method of this class.
    *
    * @param input CharSequence that is being translated
    * @param out   Writer to translate the text to
    * @throws IOException if and only if the Writer produces an IOException
    */
  @throws[IOException]
  final def translate(input: CharSequence, out: Writer): Unit = {
    if (out == null) throw new IllegalArgumentException("The Writer must not be null")
    if (input == null) return
    var pos = 0
    val len = input.length
    while (pos < len) {
      val consumed = translate(input, pos, out)
      if (consumed == 0) { // inlined implementation of Character.toChars(Character.codePointAt(input, pos))
        // avoids allocating temp char arrays and duplicate checks
        val c1: Char = input.charAt(pos)
        out.write(c1.toInt)
        pos += 1
        if (Character.isHighSurrogate(c1) && pos < len) {
          val c2: Char = input.charAt(pos)
          if (Character.isLowSurrogate(c2)) {
            out.write(c2.toInt)
            pos += 1
          }
        }
      } else {
        // contract with translators is that they have to understand codepoints
        // and they just took care of a surrogate pair
        for (_ <- 0 until consumed) {
          pos += Character.charCount(Character.codePointAt(input, pos))
        }
      }
    }
  }

  /**
    * Helper method to create a merger of this translator with another set of
    * translators. Useful in customizing the standard functionality.
    *
    * @param translators CharSequenceTranslator array of translators to merge with this one
    * @return CharSequenceTranslator merging this translator with the others
    */
  final def `with`(translators: CharSequenceTranslator*): CharSequenceTranslator = {
    val newArray = new Array[CharSequenceTranslator](translators.length + 1)
    newArray(0) = this
    System.arraycopy(translators, 0, newArray, 1, translators.length)
    new AggregateTranslator(newArray: _*)
  }
}
