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
import java.io.Writer
//import java.util
//import scala.collection.JavaConverters._

/**
  * Translate XML numeric entities of the form &amp;#[xX]?\d+;? to
  * the specific codepoint.
  *
  * Note that the semi-colon is optional.
  *
  * @since 3.0
  * @deprecated as of 3.6, use commons-text
  *             <a href="https://commons.apache.org/proper/commons-text/javadocs/api-release/org/apache/commons/text/translate/NumericEntityUnescaper.html">
  *             NumericEntityUnescaper</a> instead
  */
@deprecated object NumericEntityUnescaper {
  type OPTION = OPTION.Value
  object OPTION extends Enumeration {
    val semiColonRequired, semiColonOptional, errorIfNoSemiColon = Value
  }
}

@deprecated class NumericEntityUnescaper
  extends CharSequenceTranslator {

  // TODO?: Create an OptionsSet class to hide some of the conditional logic below
  final private var options: Set[NumericEntityUnescaper.OPTION] =
    Set(NumericEntityUnescaper.OPTION.semiColonRequired)

  /**
    * Create a UnicodeUnescaper.
    *
    * The constructor takes a list of options, only one type of which is currently
    * available (whether to allow, error or ignore the semi-colon on the end of a
    * numeric entity to being missing).
    *
    * For example, to support numeric entities without a ';':
    * new NumericEntityUnescaper(NumericEntityUnescaper.OPTION.semiColonOptional)
    * and to throw an IllegalArgumentException when they're missing:
    * new NumericEntityUnescaper(NumericEntityUnescaper.OPTION.errorIfNoSemiColon)
    *
    * Note that the default behaviour is to ignore them.
    *
    * @param options to apply to this unescaper
    */
  def this(options: NumericEntityUnescaper.OPTION*) = {
    this()
    if (options.nonEmpty) this.options = options.toSet
  }

  /**
    * Whether the passed in option is currently set.
    *
    * @param option to check state of
    * @return whether the option is set
    */
  def isSet(option: NumericEntityUnescaper.OPTION): Boolean =
    options != null && options.contains(option)

  /**
    * {@inheritDoc }
    */
  @throws[IOException]
  override def translate(input: CharSequence, index: Int, out: Writer): Int = {
    val seqEnd = input.length
    // Uses -2 to ensure there is something after the &#
    if (input.charAt(index) == '&' && index < seqEnd - 2 && input.charAt(index + 1) == '#') {
      var start = index + 2
      var isHex = false
      val firstChar = input.charAt(start)
      if (firstChar == 'x' || firstChar == 'X') {
        start += 1
        isHex = true
        // Check there's more than just an x after the &#
        if (start == seqEnd) return 0
      }
      var `end` = start
      // Note that this supports character codes without a ; on the end
      while (
        `end` < seqEnd &&
          (
            input.charAt(`end`) >= '0' && input.charAt(`end`) <= '9' ||
              input.charAt(`end`) >= 'a' && input.charAt(`end`) <= 'f' ||
              input.charAt(`end`) >= 'A' &&
              input.charAt(`end`) <= 'F'
          )
      ) `end` += 1

      val semiNext = `end` != seqEnd && input.charAt(`end`) == ';'

      if (!semiNext) if (isSet(NumericEntityUnescaper.OPTION.semiColonRequired)) return 0
      else if (isSet(NumericEntityUnescaper.OPTION.errorIfNoSemiColon)) throw new IllegalArgumentException("Semi-colon required at end of numeric entity")

      var entityValue = 0

      try {
        if (isHex) entityValue = Integer.parseInt(input.subSequence(start, `end`).toString, 16)
        else entityValue = Integer.parseInt(input.subSequence(start, `end`).toString, 10)
      } catch {
        case _: NumberFormatException => return 0
      }

      if (entityValue > 0xFFFF) {
        val chars = Character.toChars(entityValue)
        out.write(chars(0).toInt)
        out.write(chars(1).toInt)
      } else out.write(entityValue)

      return 2 +
        `end` - start +
        (if (isHex) 1 else 0) +
        (if (semiNext) 1 else 0)
    }

    0
  }
}