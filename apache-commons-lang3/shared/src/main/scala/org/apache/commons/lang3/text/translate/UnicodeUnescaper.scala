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
  * Translates escaped Unicode values of the form \\u+\d\d\d\d back to
  * Unicode. It supports multiple 'u' characters and will work with or
  * without the +.
  *
  * @since 3.0
  * @deprecated as of 3.6, use commons-text
  *             <a href="https://commons.apache.org/proper/commons-text/javadocs/api-release/org/apache/commons/text/translate/UnicodeUnescaper.html">
  *             UnicodeUnescaper</a> instead
  */
@deprecated class UnicodeUnescaper extends CharSequenceTranslator {
  /**
    * {@inheritDoc }
    */
  @throws[IOException]
  override def translate(input: CharSequence, index: Int, out: Writer): Int = {
    if (input.charAt(index) == '\\' && index + 1 < input.length && input.charAt(index + 1) == 'u') { // consume optional additional 'u' chars
      var i = 2
      while (index + i < input.length && input.charAt(index + i) == 'u') i += 1

      if (index + i < input.length && input.charAt(index + i) == '+') i += 1
      if (index + i + 4 <= input.length) { // Get 4 hex digits
        val unicode = input.subSequence(index + i, index + i + 4)
        try {
          val value = Integer.parseInt(unicode.toString, 16)
          out.write(value)
        } catch {
          case nfe: NumberFormatException =>
            throw new IllegalArgumentException("Unable to parse unicode value: " + unicode, nfe)
        }
        return i + 4
      }
      throw new IllegalArgumentException("Less than 4 hex digits in unicode value: '" + input.subSequence(index, input.length) + "' due to end of CharSequence")
    }
    0
  }
}