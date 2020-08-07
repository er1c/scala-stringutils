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

package org.apache.commons.lang3

import java.nio.charset.Charset

/**
  * Internal use only.
  * <p>
  * Provides utilities for {@link Charset}.
  * </p>
  * <p>
  * Package private since Apache Commons IO already provides a Charsets because {@link Charset} is in
  * {@code java.nio.charset}.
  * </p>
  *
  * @since 3.10
  */
object Charsets {
  /**
    * Returns the given {@code charset} or the default Charset if {@code charset} is null.
    *
    * @param charset a Charset or null.
    * @return the given {@code charset} or the default Charset if {@code charset} is null.
    */
  private[lang3] def toCharset(charset: Charset) =
    if (charset == null) Charset.defaultCharset
    else charset

  /**
    * Returns the given {@code charset} or the default Charset if {@code charset} is null.
    *
    * @param charsetName a Charset or null.
    * @return the given {@code charset} or the default Charset if {@code charset} is null.
    * @throws UnsupportedCharsetException If no support for the named charset is available in this instance of the Java
    *                                     virtual machine
    */
  private[lang3] def toCharset(charsetName: String) =
    if (charsetName == null) Charset.defaultCharset
    else Charset.forName(charsetName)

  /**
    * Returns the given {@code charset} or the default Charset if {@code charset} is null.
    *
    * @param charsetName a Charset or null.
    * @return the given {@code charset} or the default Charset if {@code charset} is null.
    */
  private[lang3] def toCharsetName(charsetName: String) =
    if (charsetName == null) Charset.defaultCharset.name
    else charsetName
}