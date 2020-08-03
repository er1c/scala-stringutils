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
import java.util

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
  * Translates a value using a lookup table.
  *
  * @since 3.0
  * @deprecated as of 3.6, use commons-text
  *             <a href="https://commons.apache.org/proper/commons-text/javadocs/api-release/org/apache/commons/text/translate/LookupTranslator.html">
  *             LookupTranslator</a> instead
  */
@deprecated class LookupTranslator
/**
  * Define the lookup table to be used in translation
  *
  * Note that, as of Lang 3.1, the key to the lookup table is converted to a
  * java.lang.String. This is because we need the key to support hashCode and
  * equals(Object), allowing it to be the key for a HashMap. See LANG-882.
  *
  * @param lookup CharSequence[][] table of size [*][2]
  */
  (val lookup: Array[CharSequence]*)

/**
  * Define the lookup table to be used in translation
  *
  * Note that, as of Lang 3.1, the key to the lookup table is converted to a
  * java.lang.String. This is because we need the key to support hashCode and
  * equals(Object), allowing it to be the key for a HashMap. See LANG-882.
  *
  * @param lookup CharSequence[][] table of size [*][2]
  */
  extends CharSequenceTranslator {

  final private var lookupMap: util.HashMap[String, String] = new util.HashMap[String, String]
  final private var prefixSet: util.HashSet[Character] = new util.HashSet[Character]
  final private var shortest = 0
  final private var longest = 0

  private var _shortest: Int = Integer.MAX_VALUE
  private var _longest = 0

  if (lookup != null) {
    for (seq: Array[CharSequence] <- lookup) {
      this.lookupMap.put(seq(0).toString, seq(1).toString)
      this.prefixSet.add(seq(0).charAt(0))
      val sz = seq(0).length
      if (sz < _shortest) _shortest = sz
      if (sz > _longest) _longest = sz
    }
  }

  shortest = _shortest
  longest = _longest


  /**
    * {@inheritDoc }
    */
  @throws[IOException]
  override def translate(input: CharSequence, index: Int, out: Writer): Int = { // check if translation exists for the input at position index
    if (prefixSet.contains(input.charAt(index))) {
      var max = longest
      if (index + longest > input.length) max = input.length - index
      // implement greedy algorithm by trying maximum match first
      for (i <- max to shortest by -1) {
        val subSeq = input.subSequence(index, index + i)
        val result = lookupMap.get(subSeq.toString)
        if (result != null) {
          out.write(result)
          return i
        }
      }
    }
    0
  }
}