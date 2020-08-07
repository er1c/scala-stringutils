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
  * <p>Operations on char primitives and Character objects.</p>
  *
  * <p>This class tries to handle {@code null} input gracefully.
  * An exception will not be thrown for a {@code null} input.
  * Each method documents its behavior in more detail.</p>
  *
  * <p>#ThreadSafe#</p>
  *
  * @since 2.1
  */
object CharUtils {
  private val CHAR_STRING_ARRAY: Array[String] = (0 until 128).map { c: Int =>
    String.valueOf(c)
  }.toArray

  private val HEX_DIGITS: Array[Char] =
    Array('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f')
  /**
    * Linefeed character LF ({@code '\n'}, Unicode 000a).
    *
    * @see <a href="http://docs.oracle.com/javase/specs/jls/se7/html/jls-3.html#jls-3.10.6">JLF: Escape Sequences
    *      for Character and String Literals</a>
    * @since 2.2
    */
  val LF = '\n'
  /**
    * Carriage return characterf CR ('\r', Unicode 000d).
    *
    * @see <a href="http://docs.oracle.com/javase/specs/jls/se7/html/jls-3.html#jls-3.10.6">JLF: Escape Sequences
    *      for Character and String Literals</a>
    * @since 2.2
    */
  val CR = '\r'
  /**
    * {@code \u0000} null control character ('\0'), abbreviated NUL.
    *
    * @since 3.6
    */
  val NUL = '\u0000'

  /**
    * <p>Converts the character to a Character.</p>
    *
    * <p>For ASCII 7 bit characters, this uses a cache that will return the
    * same Character object each time.</p>
    *
    * <pre>
    * CharUtils.toCharacterObject(' ')  = ' '
    * CharUtils.toCharacterObject('A')  = 'A'
    * </pre>
    *
    * @deprecated Java 5 introduced {@link Character# valueOf ( char )} which caches chars 0 through 127.
    * @param ch the character to convert
    * @return a Character of the specified character
    */
  @deprecated def toCharacterObject(ch: Char): Character = Character.valueOf(ch)

  /**
    * <p>Converts the String to a Character using the first character, returning
    * null for empty Strings.</p>
    *
    * <p>For ASCII 7 bit characters, this uses a cache that will return the
    * same Character object each time.</p>
    *
    * <pre>
    * CharUtils.toCharacterObject(null) = null
    * CharUtils.toCharacterObject("")   = null
    * CharUtils.toCharacterObject("A")  = 'A'
    * CharUtils.toCharacterObject("BA") = 'B'
    * </pre>
    *
    * @param str the character to convert
    * @return the Character value of the first letter of the String
    */
  def toCharacterObject(str: String): Character = {
    if (StringUtils.isEmpty(str)) null
    else Character.valueOf(str.charAt(0))
  }

  /**
    * <p>Converts the Character to a char throwing an exception for {@code null}.</p>
    *
    * <pre>
    * CharUtils.toChar(' ')  = ' '
    * CharUtils.toChar('A')  = 'A'
    * CharUtils.toChar(null) throws IllegalArgumentException
    * </pre>
    *
    * @param ch the character to convert
    * @return the char value of the Character
    * @throws NullPointerException if the Character is null
    */
  def toChar(ch: Character): Char = {
    Validate.notNull(ch, "The Character must not be null")
    ch.charValue
  }

  /**
    * <p>Converts the Character to a char handling {@code null}.</p>
    *
    * <pre>
    * CharUtils.toChar(null, 'X') = 'X'
    * CharUtils.toChar(' ', 'X')  = ' '
    * CharUtils.toChar('A', 'X')  = 'A'
    * </pre>
    *
    * @param ch           the character to convert
    * @param defaultValue the value to use if the  Character is null
    * @return the char value of the Character or the default if null
    */
  def toChar(ch: Character, defaultValue: Char): Char = {
    if (ch == null) defaultValue
    else ch.charValue
  }

  /**
    * <p>Converts the String to a char using the first character, throwing
    * an exception on empty Strings.</p>
    *
    * <pre>
    * CharUtils.toChar("A")  = 'A'
    * CharUtils.toChar("BA") = 'B'
    * CharUtils.toChar(null) throws IllegalArgumentException
    * CharUtils.toChar("")   throws IllegalArgumentException
    * </pre>
    *
    * @param str the character to convert
    * @return the char value of the first letter of the String
    * @throws NullPointerException     if the string is null
    * @throws IllegalArgumentException if the String is empty
    */
  def toChar(str: String): Char = {
    Validate.notEmpty(str, "The String must not be empty")
    str.charAt(0)
  }

  /**
    * <p>Converts the String to a char using the first character, defaulting
    * the value on empty Strings.</p>
    *
    * <pre>
    * CharUtils.toChar(null, 'X') = 'X'
    * CharUtils.toChar("", 'X')   = 'X'
    * CharUtils.toChar("A", 'X')  = 'A'
    * CharUtils.toChar("BA", 'X') = 'B'
    * </pre>
    *
    * @param str          the character to convert
    * @param defaultValue the value to use if the  Character is null
    * @return the char value of the first letter of the String or the default if null
    */
  def toChar(str: String, defaultValue: Char): Char = {
    if (StringUtils.isEmpty(str)) return defaultValue
    str.charAt(0)
  }

  /**
    * <p>Converts the character to the Integer it represents, throwing an
    * exception if the character is not numeric.</p>
    *
    * <p>This method converts the char '1' to the int 1 and so on.</p>
    *
    * <pre>
    * CharUtils.toIntValue('3')  = 3
    * CharUtils.toIntValue('A')  throws IllegalArgumentException
    * </pre>
    *
    * @param ch the character to convert
    * @return the int value of the character
    * @throws IllegalArgumentException if the character is not ASCII numeric
    */
  def toIntValue(ch: Char): Int = {
    if (!isAsciiNumeric(ch))
      throw new IllegalArgumentException("The character " + ch + " is not in the range '0' - '9'")
    ch - 48
  }

  /**
    * <p>Converts the character to the Integer it represents, throwing an
    * exception if the character is not numeric.</p>
    *
    * <p>This method converts the char '1' to the int 1 and so on.</p>
    *
    * <pre>
    * CharUtils.toIntValue('3', -1)  = 3
    * CharUtils.toIntValue('A', -1)  = -1
    * </pre>
    *
    * @param ch           the character to convert
    * @param defaultValue the default value to use if the character is not numeric
    * @return the int value of the character
    */
  def toIntValue(ch: Char, defaultValue: Int): Int = {
    if (!isAsciiNumeric(ch)) return defaultValue
    ch - 48
  }

  /**
    * <p>Converts the character to the Integer it represents, throwing an
    * exception if the character is not numeric.</p>
    *
    * <p>This method converts the char '1' to the int 1 and so on.</p>
    *
    * <pre>
    * CharUtils.toIntValue('3')  = 3
    * CharUtils.toIntValue(null) throws IllegalArgumentException
    * CharUtils.toIntValue('A')  throws IllegalArgumentException
    * </pre>
    *
    * @param ch the character to convert, not null
    * @return the int value of the character
    * @throws NullPointerException     if the Character is null
    * @throws IllegalArgumentException if the Character is not ASCII numeric
    */
  def toIntValue(ch: Character): Int = {
    Validate.notNull(ch, "The character must not be null")
    toIntValue(ch.charValue)
  }

  /**
    * <p>Converts the character to the Integer it represents, throwing an
    * exception if the character is not numeric.</p>
    *
    * <p>This method converts the char '1' to the int 1 and so on.</p>
    *
    * <pre>
    * CharUtils.toIntValue(null, -1) = -1
    * CharUtils.toIntValue('3', -1)  = 3
    * CharUtils.toIntValue('A', -1)  = -1
    * </pre>
    *
    * @param ch           the character to convert
    * @param defaultValue the default value to use if the character is not numeric
    * @return the int value of the character
    */
  def toIntValue(ch: Character, defaultValue: Int): Int = {
    if (ch == null) return defaultValue
    toIntValue(ch.charValue, defaultValue)
  }

  /**
    * <p>Converts the character to a String that contains the one character.</p>
    *
    * <p>For ASCII 7 bit characters, this uses a cache that will return the
    * same String object each time.</p>
    *
    * <pre>
    * CharUtils.toString(' ')  = " "
    * CharUtils.toString('A')  = "A"
    * </pre>
    *
    * @param ch the character to convert
    * @return a String containing the one specified character
    */
  def toString(ch: Char): String = {
    if (ch < 128) CHAR_STRING_ARRAY(ch.toInt)
    else new String(Array[Char](ch))
  }

  /**
    * <p>Converts the character to a String that contains the one character.</p>
    *
    * <p>For ASCII 7 bit characters, this uses a cache that will return the
    * same String object each time.</p>
    *
    * <p>If {@code null} is passed in, {@code null} will be returned.</p>
    *
    * <pre>
    * CharUtils.toString(null) = null
    * CharUtils.toString(' ')  = " "
    * CharUtils.toString('A')  = "A"
    * </pre>
    *
    * @param ch the character to convert
    * @return a String containing the one specified character
    */
  def toString(ch: Character): String = {
    if (ch == null) return null
    toString(ch.charValue)
  }

  /**
    * <p>Converts the string to the Unicode format '\u0020'.</p>
    *
    * <p>This format is the Java source code format.</p>
    *
    * <pre>
    * CharUtils.unicodeEscaped(' ') = "\u0020"
    * CharUtils.unicodeEscaped('A') = "\u0041"
    * </pre>
    *
    * @param ch the character to convert
    * @return the escaped Unicode string
    */
  def unicodeEscaped(ch: Char): String =
    "\\u" +
      HEX_DIGITS((ch >> 12) & 15) +
      HEX_DIGITS((ch >> 8) & 15) +
      HEX_DIGITS((ch >> 4) & 15) +
      HEX_DIGITS(ch & 15)

  /**
    * <p>Converts the string to the Unicode format '\u0020'.</p>
    *
    * <p>This format is the Java source code format.</p>
    *
    * <p>If {@code null} is passed in, {@code null} will be returned.</p>
    *
    * <pre>
    * CharUtils.unicodeEscaped(null) = null
    * CharUtils.unicodeEscaped(' ')  = "\u0020"
    * CharUtils.unicodeEscaped('A')  = "\u0041"
    * </pre>
    *
    * @param ch the character to convert, may be null
    * @return the escaped Unicode string, null if null input
    */
  def unicodeEscaped(ch: Character): String = {
    if (ch == null) null
    else unicodeEscaped(ch.charValue)
  }

  /**
    * <p>Checks whether the character is ASCII 7 bit.</p>
    *
    * <pre>
    * CharUtils.isAscii('a')  = true
    * CharUtils.isAscii('A')  = true
    * CharUtils.isAscii('3')  = true
    * CharUtils.isAscii('-')  = true
    * CharUtils.isAscii('\n') = true
    * CharUtils.isAscii('&copy;') = false
    * </pre>
    *
    * @param ch the character to check
    * @return true if less than 128
    */
  def isAscii(ch: Char): Boolean = ch < 128

  /**
    * <p>Checks whether the character is ASCII 7 bit printable.</p>
    *
    * <pre>
    * CharUtils.isAsciiPrintable('a')  = true
    * CharUtils.isAsciiPrintable('A')  = true
    * CharUtils.isAsciiPrintable('3')  = true
    * CharUtils.isAsciiPrintable('-')  = true
    * CharUtils.isAsciiPrintable('\n') = false
    * CharUtils.isAsciiPrintable('&copy;') = false
    * </pre>
    *
    * @param ch the character to check
    * @return true if between 32 and 126 inclusive
    */
  def isAsciiPrintable(ch: Char): Boolean = ch >= 32 && ch < 127

  /**
    * <p>Checks whether the character is ASCII 7 bit control.</p>
    *
    * <pre>
    * CharUtils.isAsciiControl('a')  = false
    * CharUtils.isAsciiControl('A')  = false
    * CharUtils.isAsciiControl('3')  = false
    * CharUtils.isAsciiControl('-')  = false
    * CharUtils.isAsciiControl('\n') = true
    * CharUtils.isAsciiControl('&copy;') = false
    * </pre>
    *
    * @param ch the character to check
    * @return true if less than 32 or equals 127
    */
  def isAsciiControl(ch: Char): Boolean = ch < 32 || ch == 127

  /**
    * <p>Checks whether the character is ASCII 7 bit alphabetic.</p>
    *
    * <pre>
    * CharUtils.isAsciiAlpha('a')  = true
    * CharUtils.isAsciiAlpha('A')  = true
    * CharUtils.isAsciiAlpha('3')  = false
    * CharUtils.isAsciiAlpha('-')  = false
    * CharUtils.isAsciiAlpha('\n') = false
    * CharUtils.isAsciiAlpha('&copy;') = false
    * </pre>
    *
    * @param ch the character to check
    * @return true if between 65 and 90 or 97 and 122 inclusive
    */
  def isAsciiAlpha(ch: Char): Boolean = isAsciiAlphaUpper(ch) || isAsciiAlphaLower(ch)

  /**
    * <p>Checks whether the character is ASCII 7 bit alphabetic upper case.</p>
    *
    * <pre>
    * CharUtils.isAsciiAlphaUpper('a')  = false
    * CharUtils.isAsciiAlphaUpper('A')  = true
    * CharUtils.isAsciiAlphaUpper('3')  = false
    * CharUtils.isAsciiAlphaUpper('-')  = false
    * CharUtils.isAsciiAlphaUpper('\n') = false
    * CharUtils.isAsciiAlphaUpper('&copy;') = false
    * </pre>
    *
    * @param ch the character to check
    * @return true if between 65 and 90 inclusive
    */
  def isAsciiAlphaUpper(ch: Char): Boolean = ch >= 'A' && ch <= 'Z'

  /**
    * <p>Checks whether the character is ASCII 7 bit alphabetic lower case.</p>
    *
    * <pre>
    * CharUtils.isAsciiAlphaLower('a')  = true
    * CharUtils.isAsciiAlphaLower('A')  = false
    * CharUtils.isAsciiAlphaLower('3')  = false
    * CharUtils.isAsciiAlphaLower('-')  = false
    * CharUtils.isAsciiAlphaLower('\n') = false
    * CharUtils.isAsciiAlphaLower('&copy;') = false
    * </pre>
    *
    * @param ch the character to check
    * @return true if between 97 and 122 inclusive
    */
  def isAsciiAlphaLower(ch: Char): Boolean = ch >= 'a' && ch <= 'z'

  /**
    * <p>Checks whether the character is ASCII 7 bit numeric.</p>
    *
    * <pre>
    * CharUtils.isAsciiNumeric('a')  = false
    * CharUtils.isAsciiNumeric('A')  = false
    * CharUtils.isAsciiNumeric('3')  = true
    * CharUtils.isAsciiNumeric('-')  = false
    * CharUtils.isAsciiNumeric('\n') = false
    * CharUtils.isAsciiNumeric('&copy;') = false
    * </pre>
    *
    * @param ch the character to check
    * @return true if between 48 and 57 inclusive
    */
  def isAsciiNumeric(ch: Char): Boolean = ch >= '0' && ch <= '9'

  /**
    * <p>Checks whether the character is ASCII 7 bit numeric.</p>
    *
    * <pre>
    * CharUtils.isAsciiAlphanumeric('a')  = true
    * CharUtils.isAsciiAlphanumeric('A')  = true
    * CharUtils.isAsciiAlphanumeric('3')  = true
    * CharUtils.isAsciiAlphanumeric('-')  = false
    * CharUtils.isAsciiAlphanumeric('\n') = false
    * CharUtils.isAsciiAlphanumeric('&copy;') = false
    * </pre>
    *
    * @param ch the character to check
    * @return true if between 48 and 57 or 65 and 90 or 97 and 122 inclusive
    */
  def isAsciiAlphanumeric(ch: Char): Boolean = isAsciiAlpha(ch) || isAsciiNumeric(ch)

  /**
    * <p>Compares two {@code char} values numerically. This is the same functionality as provided in Java 7.</p>
    *
    * @param x the first {@code char} to compare
    * @param y the second {@code char} to compare
    * @return the value {@code 0} if {@code x == y};
    *         a value less than {@code 0} if {@code x < y}; and
    *         a value greater than {@code 0} if {@code x > y}
    * @since 3.4
    */
  def compare(x: Char, y: Char): Int = x - y
}
