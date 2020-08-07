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
  * Translate escaped octal Strings back to their octal values.
  *
  * For example, "\45" should go back to being the specific value (a %).
  *
  * Note that this currently only supports the viable range of octal for Java; namely
  * 1 to 377. This is because parsing Java is the main use case.
  *
  * @since 3.0
  * @deprecated as of 3.6, use commons-text
  *             <a href="https://commons.apache.org/proper/commons-text/javadocs/api-release/org/apache/commons/text/translate/OctalUnescaper.html">
  *             OctalUnescaper</a> instead
  */
@deprecated class OctalUnescaper extends CharSequenceTranslator {
  /**
    * {@inheritDoc }
    */
  @throws[IOException]
  override def translate(input: CharSequence, index: Int, out: Writer): Int = {
    val remaining = input.length - index - 1 // how many characters left, ignoring the first \
    val builder = new StringBuilder
    if (input.charAt(index) == '\\' && remaining > 0 && isOctalDigit(input.charAt(index + 1))) {
      val next = index + 1
      val next2 = index + 2
      val next3 = index + 3
      // we know this is good as we checked it in the if block above
      builder.append(input.charAt(next))
      if (remaining > 1 && isOctalDigit(input.charAt(next2))) {
        builder.append(input.charAt(next2))
        if (remaining > 2 && isZeroToThree(input.charAt(next)) && isOctalDigit(input.charAt(next3)))
          builder.append(input.charAt(next3))
      }
      out.write(Integer.parseInt(builder.toString, 8))
      return 1 + builder.length
    }
    0
  }

  /**
    * Checks if the given char is an octal digit. Octal digits are the character representations of the digits 0 to 7.
    *
    * @param ch the char to check
    * @return true if the given char is the character representation of one of the digits from 0 to 7
    */
  private def isOctalDigit(ch: Char) = ch >= '0' && ch <= '7'

  /**
    * Checks if the given char is the character representation of one of the digit from 0 to 3.
    *
    * @param ch the char to check
    * @return true if the given char is the character representation of one of the digits from 0 to 3
    */
  private def isZeroToThree(ch: Char) = ch >= '0' && ch <= '3'
}
