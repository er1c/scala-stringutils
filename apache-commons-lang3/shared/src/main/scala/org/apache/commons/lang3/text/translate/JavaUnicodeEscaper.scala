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

/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */


/**
  * Translates codepoints to their Unicode escaped value suitable for Java source.
  *
  * @since 3.2
  * @deprecated as of 3.6, use commons-text
  *             <a href="https://commons.apache.org/proper/commons-text/javadocs/api-release/org/apache/commons/text/translate/UnicodeEscaper.html">
  *             UnicodeEscaper</a> instead
  */
@deprecated object JavaUnicodeEscaper {
  /**
    * <p>
    * Constructs a {@code JavaUnicodeEscaper} above the specified value (exclusive).
    * </p>
    *
    * @param codepoint
    * above which to escape
    * @return the newly created {@code UnicodeEscaper} instance
    */
  def above(codepoint: Int): JavaUnicodeEscaper = outsideOf(0, codepoint)

  /**
    * <p>
    * Constructs a {@code JavaUnicodeEscaper} below the specified value (exclusive).
    * </p>
    *
    * @param codepoint
    * below which to escape
    * @return the newly created {@code UnicodeEscaper} instance
    */
  def below(codepoint: Int): JavaUnicodeEscaper = outsideOf(codepoint, Integer.MAX_VALUE)

  /**
    * <p>
    * Constructs a {@code JavaUnicodeEscaper} between the specified values (inclusive).
    * </p>
    *
    * @param codepointLow
    * above which to escape
    * @param codepointHigh
    * below which to escape
    * @return the newly created {@code UnicodeEscaper} instance
    */
  def between(codepointLow: Int, codepointHigh: Int) = new JavaUnicodeEscaper(codepointLow, codepointHigh, true)

  /**
    * <p>
    * Constructs a {@code JavaUnicodeEscaper} outside of the specified values (exclusive).
    * </p>
    *
    * @param codepointLow
    * below which to escape
    * @param codepointHigh
    * above which to escape
    * @return the newly created {@code UnicodeEscaper} instance
    */
  def outsideOf(codepointLow: Int, codepointHigh: Int) = new JavaUnicodeEscaper(codepointLow, codepointHigh, false)
}

@deprecated class JavaUnicodeEscaper(val below: Int, val above: Int, val between: Boolean)

/**
  * <p>
  * Constructs a {@code JavaUnicodeEscaper} for the specified range. This is the underlying method for the
  * other constructors/builders. The {@code below} and {@code above} boundaries are inclusive when
  * {@code between} is {@code true} and exclusive when it is {@code false}.
  * </p>
  *
  * @param below
  * int value representing the lowest codepoint boundary
  * @param above
  * int value representing the highest codepoint boundary
  * @param between
  * whether to escape between the boundaries or outside them
  */
  extends UnicodeEscaper(below, above, between) {
  /**
    * Converts the given codepoint to a hex string of the form {@code "\\uXXXX\\uXXXX"}
    *
    * @param codepoint
    * a Unicode code point
    * @return the hex string for the given codepoint
    */
  override protected def toUtf16Escape(codepoint: Int): String = {
    val surrogatePair = Character.toChars(codepoint)
    "\\u" + hex(surrogatePair(0).toInt) + "\\u" + hex(surrogatePair(1).toInt)
  }
}