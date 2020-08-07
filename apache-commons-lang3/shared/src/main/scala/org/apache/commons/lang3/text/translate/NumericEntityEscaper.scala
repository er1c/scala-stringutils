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
  * Translates codepoints to their XML numeric entity escaped value.
  *
  * @since 3.0
  * @deprecated as of 3.6, use commons-text
  *             <a href="https://commons.apache.org/proper/commons-text/javadocs/api-release/org/apache/commons/text/translate/NumericEntityEscaper.html">
  *             NumericEntityEscaper</a> instead
  */
@deprecated object NumericEntityEscaper {
  /**
    * <p>Constructs a {@code NumericEntityEscaper} below the specified value (exclusive). </p>
    *
    * @param codepoint below which to escape
    * @return the newly created {@code NumericEntityEscaper} instance
    */
  def below(codepoint: Int): NumericEntityEscaper = outsideOf(codepoint, Integer.MAX_VALUE)

  /**
    * <p>Constructs a {@code NumericEntityEscaper} above the specified value (exclusive). </p>
    *
    * @param codepoint above which to escape
    * @return the newly created {@code NumericEntityEscaper} instance
    */
  def above(codepoint: Int): NumericEntityEscaper = outsideOf(0, codepoint)

  /**
    * <p>Constructs a {@code NumericEntityEscaper} between the specified values (inclusive). </p>
    *
    * @param codepointLow  above which to escape
    * @param codepointHigh below which to escape
    * @return the newly created {@code NumericEntityEscaper} instance
    */
  def between(codepointLow: Int, codepointHigh: Int) = new NumericEntityEscaper(codepointLow, codepointHigh, true)

  /**
    * <p>Constructs a {@code NumericEntityEscaper} outside of the specified values (exclusive). </p>
    *
    * @param codepointLow  below which to escape
    * @param codepointHigh above which to escape
    * @return the newly created {@code NumericEntityEscaper} instance
    */
  def outsideOf(codepointLow: Int, codepointHigh: Int) = new NumericEntityEscaper(codepointLow, codepointHigh, false)
}

@deprecated class NumericEntityEscaper private (val below: Int, val above: Int, val between: Boolean)

/**
  * <p>Constructs a {@code NumericEntityEscaper} for the specified range. This is
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
    * <p>Constructs a {@code NumericEntityEscaper} for all characters. </p>
    */
  def this() = {
    this(0, Integer.MAX_VALUE, true)
  }

  /**
    * {@inheritDoc }
    */
  @throws[IOException]
  override def translate(codepoint: Int, out: Writer): Boolean = {
    if (between)
      if (codepoint < below || codepoint > above) return false
      else if (codepoint >= below && codepoint <= above) return false
    out.write("&#")
    out.write(Integer.toString(codepoint, 10))
    out.write(';')
    true
  }
}
