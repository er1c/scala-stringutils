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

/**
  * <p>Operations on {@link CharSequence} that are
  * {@code null} safe.</p>
  *
  * @see CharSequence
  * @since 3.0
  */
object CharSequenceUtils {
  private val NOT_FOUND = -1
//
//  /**
//    * <p>Returns a new {@code CharSequence} that is a subsequence of this
//    * sequence starting with the {@code char} value at the specified index.</p>
//    *
//    * <p>This provides the {@code CharSequence} equivalent to {@link String# substring ( int )}.
//    * The length (in {@code char}) of the returned sequence is {@code length() - start},
//    * so if {@code start == end} then an empty sequence is returned.</p>
//    *
//    * @param cs    the specified subsequence, null returns null
//    * @param start the start index, inclusive, valid
//    * @return a new subsequence, may be null
//    * @throws IndexOutOfBoundsException if {@code start} is negative or if
//    *                                   {@code start} is greater than {@code length()}
//    */
//  def subSequence(cs: CharSequence, start: Int): CharSequence = if (cs == null) null
//  else cs.subSequence(start, cs.length)
//
  /**
    * Returns the index within {@code cs} of the first occurrence of the
    * specified character, starting the search at the specified index.
    * <p>
    * If a character with value {@code searchChar} occurs in the
    * character sequence represented by the {@code cs}
    * object at an index no smaller than {@code start}, then
    * the index of the first such occurrence is returned. For values
    * of {@code searchChar} in the range from 0 to 0xFFFF (inclusive),
    * this is the smallest value <i>k</i> such that:
    * <blockquote><pre>
    * (this.charAt(<i>k</i>) == searchChar) &amp;&amp; (<i>k</i> &gt;= start)
    * </pre></blockquote>
    * is true. For other values of {@code searchChar}, it is the
    * smallest value <i>k</i> such that:
    * <blockquote><pre>
    * (this.codePointAt(<i>k</i>) == searchChar) &amp;&amp; (<i>k</i> &gt;= start)
    * </pre></blockquote>
    * is true. In either case, if no such character occurs inm {@code cs}
    * at or after position {@code start}, then
    * {@code -1} is returned.
    *
    * <p>
    * There is no restriction on the value of {@code start}. If it
    * is negative, it has the same effect as if it were zero: the entire
    * {@code CharSequence} may be searched. If it is greater than
    * the length of {@code cs}, it has the same effect as if it were
    * equal to the length of {@code cs}: {@code -1} is returned.
    *
    * <p>All indices are specified in {@code char} values
    * (Unicode code units).
    *
    * @param cs         the {@code CharSequence} to be processed, not null
    * @param searchChar the char to be searched for
    * @param start      the start index, negative starts at the string start
    * @return the index where the search char was found, -1 if not found
    * @since 3.6 updated to behave more like {@code String}
    */
  private[lang3] def indexOf(cs: CharSequence, searchChar: Int, start: Int): Int = {
    if (cs.isInstanceOf[String]) return cs.asInstanceOf[String].indexOf(searchChar, start)
    val sz = cs.length
    val startIndex: Int = if (start < 0) 0 else start

    if (searchChar < Character.MIN_SUPPLEMENTARY_CODE_POINT) for (i <- startIndex until sz) {
      if (cs.charAt(i) == searchChar) return i
    }

    //supplementary characters (LANG1300)
    if (searchChar <= Character.MAX_CODE_POINT) {
      val chars = Character.toChars(searchChar)
      for (i <- startIndex until sz - 1) {
        val high = cs.charAt(i)
        val low = cs.charAt(i + 1)
        if (high == chars(0) && low == chars(1)) return i
      }
    }

    NOT_FOUND
  }

  /**
    * Used by the indexOf(CharSequence methods) as a green implementation of indexOf.
    *
    * @param cs         the {@code CharSequence} to be processed
    * @param searchChar the {@code CharSequence} to be searched for
    * @param start      the start index
    * @return the index where the search sequence was found
    */
  private[lang3] def indexOf(cs: CharSequence, searchChar: CharSequence, start: Int): Int = {
    if (cs.isInstanceOf[String]) return cs.asInstanceOf[String].indexOf(searchChar.toString, start)
    else if (cs.isInstanceOf[StringBuilder]) return cs.asInstanceOf[StringBuilder].indexOf(searchChar.toString, start)
    else if (cs.isInstanceOf[StringBuffer]) return cs.asInstanceOf[StringBuffer].indexOf(searchChar.toString, start)
    cs.toString.indexOf(searchChar.toString, start)
    //        if (cs instanceof String && searchChar instanceof String) {
    //            // TODO: Do we assume searchChar is usually relatively small;
    //            //       If so then calling toString() on it is better than reverting to
    //            //       the green implementation in the else block
    //            return ((String) cs).indexOf((String) searchChar, start);
    //        } else {
    //            // TODO: Implement rather than convert to String
    //            return cs.toString().indexOf(searchChar.toString(), start);
    //        }
  }

  /**
    * Returns the index within {@code cs} of the last occurrence of
    * the specified character, searching backward starting at the
    * specified index. For values of {@code searchChar} in the range
    * from 0 to 0xFFFF (inclusive), the index returned is the largest
    * value <i>k</i> such that:
    * <blockquote><pre>
    * (this.charAt(<i>k</i>) == searchChar) &amp;&amp; (<i>k</i> &lt;= start)
    * </pre></blockquote>
    * is true. For other values of {@code searchChar}, it is the
    * largest value <i>k</i> such that:
    * <blockquote><pre>
    * (this.codePointAt(<i>k</i>) == searchChar) &amp;&amp; (<i>k</i> &lt;= start)
    * </pre></blockquote>
    * is true. In either case, if no such character occurs in {@code cs}
    * at or before position {@code start}, then {@code -1} is returned.
    *
    * <p>All indices are specified in {@code char} values
    * (Unicode code units).
    *
    * @param cs         the {@code CharSequence} to be processed
    * @param searchChar the char to be searched for
    * @param start      the start index, negative returns -1, beyond length starts at end
    * @return the index where the search char was found, -1 if not found
    * @since 3.6 updated to behave more like {@code String}
    */
  private[lang3] def lastIndexOf(cs: CharSequence, searchChar: Int, start: Int): Int = {
    if (cs.isInstanceOf[String]) return cs.asInstanceOf[String].lastIndexOf(searchChar, start)
    val sz = cs.length
    if (start < 0) return NOT_FOUND
    val startIndex = if (start >= sz) sz - 1 else start

    if (searchChar < Character.MIN_SUPPLEMENTARY_CODE_POINT) for (i <- startIndex to 0 by -1) {
      if (cs.charAt(i) == searchChar) return i
    }
    //NOTE - we must do a forward traversal for this to avoid duplicating code points
    if (searchChar <= Character.MAX_CODE_POINT) {
      val chars = Character.toChars(searchChar)
      //make sure it's not the last index
      if (startIndex == sz - 1) return NOT_FOUND
      for (i <- startIndex to 0 by -1) {
        val high = cs.charAt(i)
        val low = cs.charAt(i + 1)
        if (chars(0) == high && chars(1) == low) return i
      }
    }
    NOT_FOUND
  }

  private[lang3] val TO_STRING_LIMIT = 16

  /**
    * Used by the lastIndexOf(CharSequence methods) as a green implementation of lastIndexOf
    *
    * @param cs         the {@code CharSequence} to be processed
    * @param searchChar the {@code CharSequence} to be searched for
    * @param start      the start index
    * @return the index where the search sequence was found
    */
  private[lang3] def lastIndexOf(cs: CharSequence, searchChar: CharSequence, start: Int): Int = {
    if (searchChar.isInstanceOf[String])
      if (cs.isInstanceOf[String]) return cs.asInstanceOf[String].lastIndexOf(searchChar.asInstanceOf[String], start)
      else if (cs.isInstanceOf[StringBuilder])
        return cs.asInstanceOf[StringBuilder].lastIndexOf(searchChar.asInstanceOf[String], start)
      else if (cs.isInstanceOf[StringBuffer])
        return cs.asInstanceOf[StringBuffer].lastIndexOf(searchChar.asInstanceOf[String], start)
    val len1 = cs.length
    val len2 = searchChar.length

    var startIndex = if (start > len1) len1 else start

    if (startIndex < 0 || len2 < 0 || len2 > len1) return -1
    if (len2 == 0) return startIndex

    if (len2 <= TO_STRING_LIMIT)
      if (cs.isInstanceOf[String]) return cs.asInstanceOf[String].lastIndexOf(searchChar.toString, startIndex)
      else if (cs.isInstanceOf[StringBuilder])
        return cs.asInstanceOf[StringBuilder].lastIndexOf(searchChar.toString, startIndex)
      else if (cs.isInstanceOf[StringBuffer])
        return cs.asInstanceOf[StringBuffer].lastIndexOf(searchChar.toString, startIndex)

    if (startIndex + len2 > len1) startIndex = len1 - len2
    val char0 = searchChar.charAt(0)
    var i = startIndex
    while (true) {
      while (cs.charAt(i) != char0) {
        i -= 1
        if (i < 0) return -1
      }
      if (checkLaterThan1(cs, searchChar, len2, i)) return i
      i -= 1
      if (i < 0) return -1
    }

    -1
  }

  private def checkLaterThan1(cs: CharSequence, searchChar: CharSequence, len2: Int, start1: Int): Boolean = {
    var i = 1
    var j = len2 - 1
    while ({
      i <= j
    }) {
      if (cs.charAt(start1 + i) != searchChar.charAt(i) || cs.charAt(start1 + j) != searchChar.charAt(j)) return false

      i += 1
      j -= 1
    }
    true
  }

  /**
    * Converts the given CharSequence to a char[].
    *
    * @param source the {@code CharSequence} to be processed.
    * @return the resulting char array, never null.
    * @since 3.11
    */
  def toCharArray(source: CharSequence): Array[Char] = {
    val len = StringUtils.length(source)

    if (len == 0) return ArrayUtils.EMPTY_CHAR_ARRAY
    if (source.isInstanceOf[String]) return source.asInstanceOf[String].toCharArray
    val array = new Array[Char](len)

    for (i <- 0 until len) {
      array(i) = source.charAt(i)
    }

    array
  }

  /**
    * Green implementation of regionMatches.
    *
    * @param cs         the {@code CharSequence} to be processed
    * @param ignoreCase whether or not to be case insensitive
    * @param thisStart  the index to start on the {@code cs} CharSequence
    * @param substring  the {@code CharSequence} to be looked for
    * @param start      the index to start on the {@code substring} CharSequence
    * @param length     character length of the region
    * @return whether the region matched
    */
  private[lang3] def regionMatches(
    cs: CharSequence,
    ignoreCase: Boolean,
    thisStart: Int,
    substring: CharSequence,
    start: Int,
    length: Int): Boolean = {
    if (cs.isInstanceOf[String] && substring.isInstanceOf[String])
      return cs.asInstanceOf[String].regionMatches(ignoreCase, thisStart, substring.asInstanceOf[String], start, length)
    var index1 = thisStart
    var index2 = start
    var tmpLen = length
    // Extract these first so we detect NPEs the same as the java.lang.String version
    val srcLen = cs.length - thisStart
    val otherLen = substring.length - start
    // Check for invalid parameters
    if (thisStart < 0 || start < 0 || length < 0) return false
    // Check that the regions are long enough
    if (srcLen < length || otherLen < length) return false
    while (tmpLen > 0) {
      tmpLen -= 1

      val c1 = cs.charAt(index1)
      index1 += 1

      val c2 = substring.charAt(index2)
      index2 += 1

      if (c1 != c2) {
        if (!ignoreCase) return false
        else {
          // The real same check as in String.regionMatches():
          val u1 = Character.toUpperCase(c1)
          val u2 = Character.toUpperCase(c2)
          if (u1 != u2 && Character.toLowerCase(u1) != Character.toLowerCase(u2)) return false
        }

      }
    }
    true
  }
}
