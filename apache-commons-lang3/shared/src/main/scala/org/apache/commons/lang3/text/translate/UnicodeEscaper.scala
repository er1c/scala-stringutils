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

/**
  * Translates codepoints to their Unicode escaped value.
  *
  * @since 3.0
  * @deprecated as of 3.6, use commons-text
  *             <a href="https://commons.apache.org/proper/commons-text/javadocs/api-release/org/apache/commons/text/translate/UnicodeEscaper.html">
  *             UnicodeEscaper</a> instead
  */
@deprecated object UnicodeEscaper {
  /**
    * <p>Constructs a {@code UnicodeEscaper} below the specified value (exclusive). </p>
    *
    * @param codepoint below which to escape
    * @return the newly created {@code UnicodeEscaper} instance
    */
  def below(codepoint: Int): UnicodeEscaper = outsideOf(codepoint, Integer.MAX_VALUE)

  /**
    * <p>Constructs a {@code UnicodeEscaper} above the specified value (exclusive). </p>
    *
    * @param codepoint above which to escape
    * @return the newly created {@code UnicodeEscaper} instance
    */
  def above(codepoint: Int): UnicodeEscaper = outsideOf(0, codepoint)

  /**
    * <p>Constructs a {@code UnicodeEscaper} outside of the specified values (exclusive). </p>
    *
    * @param codepointLow  below which to escape
    * @param codepointHigh above which to escape
    * @return the newly created {@code UnicodeEscaper} instance
    */
  def outsideOf(codepointLow: Int, codepointHigh: Int) = new UnicodeEscaper(codepointLow, codepointHigh, false)

  /**
    * <p>Constructs a {@code UnicodeEscaper} between the specified values (inclusive). </p>
    *
    * @param codepointLow  above which to escape
    * @param codepointHigh below which to escape
    * @return the newly created {@code UnicodeEscaper} instance
    */
  def between(codepointLow: Int, codepointHigh: Int) = new UnicodeEscaper(codepointLow, codepointHigh, true)
}

@deprecated class UnicodeEscaper protected(val below: Int, val above: Int, val between: Boolean)

/**
  * <p>Constructs a {@code UnicodeEscaper} for the specified range. This is
  * the underlying method for the other constructors/builders. The {@code below}
  * and {@code above} boundaries are inclusive when {@code between} is
  * {@code true} and exclusive when it is {@code false}. </p>
  *
  * @param below   int value representing the lowest codepoint boundary
  * @param above   int value representing the highest codepoint boundary
  * @param between whether to escape between the boundaries or outside them
  */
  extends CodePointTranslator {
  /**
    * <p>Constructs a {@code UnicodeEscaper} for all characters. </p>
    */
  def this() = {
    this(0, Integer.MAX_VALUE, true)
  }

  /**
    * {@inheritDoc }
    */
  @throws[IOException]
  override def translate(codepoint: Int, out: Writer): Boolean = {
    if (between) if (codepoint < below || codepoint > above) return false
    else if (codepoint >= below && codepoint <= above) return false
    // TODO: Handle potential + sign per various Unicode escape implementations
    if (codepoint > 0xffff) out.write(toUtf16Escape(codepoint))
    else {
      out.write("\\u")
      out.write(HEX_DIGITS((codepoint >> 12) & 15).toInt)
      out.write(HEX_DIGITS((codepoint >> 8) & 15).toInt)
      out.write(HEX_DIGITS((codepoint >> 4) & 15).toInt)
      out.write(HEX_DIGITS(codepoint & 15).toInt)
    }
    true
  }

  /**
    * Converts the given codepoint to a hex string of the form {@code "\\uXXXX"}
    *
    * @param codepoint
    * a Unicode code point
    * @return the hex string for the given codepoint
    * @since 3.2
    */
  protected def toUtf16Escape(codepoint: Int): String = "\\u" + hex(codepoint)
}