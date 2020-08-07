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

import org.apache.commons.lang3.math.NumberUtils
import java.lang.{Boolean => JavaBoolean}

/**
  * <p>Operations on boolean primitives and JavaBoolean objects.</p>
  *
  * <p>This class tries to handle {@code null} input gracefully.
  * An exception will not be thrown for a {@code null} input.
  * Each method documents its behavior in more detail.</p>
  *
  * <p>#ThreadSafe#</p>
  *
  * @since 2.0
  */
object BooleanUtils {
  /**
    * <p>Negates the specified boolean.</p>
    *
    * <p>If {@code null} is passed in, {@code null} will be returned.</p>
    *
    * <p>NOTE: This returns {@code null} and will throw a {@code NullPointerException}
    * if unboxed to a boolean. </p>
    *
    * <pre>
    * JavaBooleanUtils.negate(JavaBoolean.TRUE)  = JavaBoolean.FALSE;
    * JavaBooleanUtils.negate(JavaBoolean.FALSE) = JavaBoolean.TRUE;
    * JavaBooleanUtils.negate(null)          = null;
    * </pre>
    *
    * @param bool the JavaBoolean to negate, may be null
    * @return the negated JavaBoolean, or {@code null} if {@code null} input
    */
  def negate(bool: JavaBoolean): JavaBoolean = {
    if (bool == null) return null
    if (bool.booleanValue) JavaBoolean.FALSE
    else JavaBoolean.TRUE
  }

  /**
    * <p>Checks if a {@code JavaBoolean} value is {@code true},
    * handling {@code null} by returning {@code false}.</p>
    *
    * <pre>
    * JavaBooleanUtils.isTrue(JavaBoolean.TRUE)  = true
    * JavaBooleanUtils.isTrue(JavaBoolean.FALSE) = false
    * JavaBooleanUtils.isTrue(null)          = false
    * </pre>
    *
    * @param bool the boolean to check, {@code null} returns {@code false}
    * @return {@code true} only if the input is non-null and true
    * @since 2.1
    */
  def isTrue(bool: JavaBoolean): JavaBoolean = JavaBoolean.TRUE == bool

  /**
    * <p>Checks if a {@code JavaBoolean} value is <i>not</i> {@code true},
    * handling {@code null} by returning {@code true}.</p>
    *
    * <pre>
    * JavaBooleanUtils.isNotTrue(JavaBoolean.TRUE)  = false
    * JavaBooleanUtils.isNotTrue(JavaBoolean.FALSE) = true
    * JavaBooleanUtils.isNotTrue(null)          = true
    * </pre>
    *
    * @param bool the boolean to check, null returns {@code true}
    * @return {@code true} if the input is null or false
    * @since 2.3
    */
  def isNotTrue(bool: JavaBoolean): JavaBoolean = !isTrue(bool)

  /**
    * <p>Checks if a {@code JavaBoolean} value is {@code false},
    * handling {@code null} by returning {@code false}.</p>
    *
    * <pre>
    * JavaBooleanUtils.isFalse(JavaBoolean.TRUE)  = false
    * JavaBooleanUtils.isFalse(JavaBoolean.FALSE) = true
    * JavaBooleanUtils.isFalse(null)          = false
    * </pre>
    *
    * @param bool the boolean to check, null returns {@code false}
    * @return {@code true} only if the input is non-{@code null} and {@code false}
    * @since 2.1
    */
  def isFalse(bool: JavaBoolean): JavaBoolean = JavaBoolean.FALSE == bool

  /**
    * <p>Checks if a {@code JavaBoolean} value is <i>not</i> {@code false},
    * handling {@code null} by returning {@code true}.</p>
    *
    * <pre>
    * JavaBooleanUtils.isNotFalse(JavaBoolean.TRUE)  = true
    * JavaBooleanUtils.isNotFalse(JavaBoolean.FALSE) = false
    * JavaBooleanUtils.isNotFalse(null)          = true
    * </pre>
    *
    * @param bool the boolean to check, null returns {@code true}
    * @return {@code true} if the input is {@code null} or {@code true}
    * @since 2.3
    */
  def isNotFalse(bool: JavaBoolean): JavaBoolean = !isFalse(bool)

  /**
    * <p>Converts a JavaBoolean to a boolean handling {@code null}
    * by returning {@code false}.</p>
    *
    * <pre>
    * JavaBooleanUtils.toJavaBoolean(JavaBoolean.TRUE)  = true
    * JavaBooleanUtils.toJavaBoolean(JavaBoolean.FALSE) = false
    * JavaBooleanUtils.toJavaBoolean(null)          = false
    * </pre>
    *
    * @param bool the boolean to convert
    * @return {@code true} or {@code false}, {@code null} returns {@code false}
    */
  def toJavaBoolean(bool: JavaBoolean): JavaBoolean = bool != null && bool.booleanValue

  /**
    * <p>Converts a JavaBoolean to a boolean handling {@code null}.</p>
    *
    * <pre>
    * JavaBooleanUtils.toJavaBooleanDefaultIfNull(JavaBoolean.TRUE, false)  = true
    * JavaBooleanUtils.toJavaBooleanDefaultIfNull(JavaBoolean.TRUE, true)   = true
    * JavaBooleanUtils.toJavaBooleanDefaultIfNull(JavaBoolean.FALSE, true)  = false
    * JavaBooleanUtils.toJavaBooleanDefaultIfNull(JavaBoolean.FALSE, false) = false
    * JavaBooleanUtils.toJavaBooleanDefaultIfNull(null, true)           = true
    * JavaBooleanUtils.toJavaBooleanDefaultIfNull(null, false)          = false
    * </pre>
    *
    * @param bool        the boolean object to convert to primitive
    * @param valueIfNull the boolean value to return if the parameter {@code bool} is {@code null}
    * @return {@code true} or {@code false}
    */
  def toJavaBooleanDefaultIfNull(bool: JavaBoolean, valueIfNull: JavaBoolean): JavaBoolean = {
    if (bool == null) return valueIfNull
    bool.booleanValue
  }

  /**
    * <p>Converts an int to a boolean using the convention that {@code zero}
    * is {@code false}, everything else is {@code true}.</p>
    *
    * <pre>
    * JavaBooleanUtils.toJavaBoolean(0) = false
    * JavaBooleanUtils.toJavaBoolean(1) = true
    * JavaBooleanUtils.toJavaBoolean(2) = true
    * </pre>
    *
    * @param value the int to convert
    * @return {@code true} if non-zero, {@code false}
    *         if zero
    */
  def toJavaBoolean(value: Int): JavaBoolean = value != 0

  /**
    * <p>Converts an int to a JavaBoolean using the convention that {@code zero}
    * is {@code false}, everything else is {@code true}.</p>
    *
    * <pre>
    * JavaBooleanUtils.toJavaBoolean(0) = JavaBoolean.FALSE
    * JavaBooleanUtils.toJavaBoolean(1) = JavaBoolean.TRUE
    * JavaBooleanUtils.toJavaBoolean(2) = JavaBoolean.TRUE
    * </pre>
    *
    * @param value the int to convert
    * @return JavaBoolean.TRUE if non-zero, JavaBoolean.FALSE if zero,
    *         {@code null} if {@code null}
    */
  def toJavaBooleanObject(value: Int): JavaBoolean =
    if (value == 0) JavaBoolean.FALSE
    else JavaBoolean.TRUE

  /**
    * <p>Converts an Integer to a JavaBoolean using the convention that {@code zero}
    * is {@code false}, every other numeric value is {@code true}.</p>
    *
    * <p>{@code null} will be converted to {@code null}.</p>
    *
    * <p>NOTE: This method may return {@code null} and may throw a {@code NullPointerException}
    * if unboxed to a {@code boolean}.</p>
    *
    * <pre>
    * JavaBooleanUtils.toJavaBoolean(Integer.valueOf(0))    = JavaBoolean.FALSE
    * JavaBooleanUtils.toJavaBoolean(Integer.valueOf(1))    = JavaBoolean.TRUE
    * JavaBooleanUtils.toJavaBoolean(Integer.valueOf(null)) = null
    * </pre>
    *
    * @param value the Integer to convert
    * @return JavaBoolean.TRUE if non-zero, JavaBoolean.FALSE if zero,
    *         {@code null} if {@code null} input
    */
  def toJavaBooleanObject(value: Integer): JavaBoolean = {
    if (value == null) return null
    if (value.intValue == 0) JavaBoolean.FALSE
    else JavaBoolean.TRUE
  }

  /**
    * <p>Converts an int to a boolean specifying the conversion values.</p>
    *
    * <p>If the {@code trueValue} and {@code falseValue} are the same number then
    * the return value will be {@code true} in case {@code value} matches it.</p>
    *
    * <pre>
    * JavaBooleanUtils.toJavaBoolean(0, 1, 0) = false
    * JavaBooleanUtils.toJavaBoolean(1, 1, 0) = true
    * JavaBooleanUtils.toJavaBoolean(1, 1, 1) = true
    * JavaBooleanUtils.toJavaBoolean(2, 1, 2) = false
    * JavaBooleanUtils.toJavaBoolean(2, 2, 0) = true
    * </pre>
    *
    * @param value      the {@code Integer} to convert
    * @param trueValue  the value to match for {@code true}
    * @param falseValue the value to match for {@code false}
    * @return {@code true} or {@code false}
    * @throws IllegalArgumentException if {@code value} does not match neither
    *                                  {@code trueValue} no {@code falseValue}
    */
  def toJavaBoolean(value: Int, trueValue: Int, falseValue: Int): JavaBoolean = {
    if (value == trueValue) return true
    if (value == falseValue) return false
    throw new IllegalArgumentException("The Integer did not match either specified value")
  }

  /**
    * <p>Converts an Integer to a boolean specifying the conversion values.</p>
    *
    * <pre>
    * JavaBooleanUtils.toJavaBoolean(Integer.valueOf(0), Integer.valueOf(1), Integer.valueOf(0)) = false
    * JavaBooleanUtils.toJavaBoolean(Integer.valueOf(1), Integer.valueOf(1), Integer.valueOf(0)) = true
    * JavaBooleanUtils.toJavaBoolean(Integer.valueOf(2), Integer.valueOf(1), Integer.valueOf(2)) = false
    * JavaBooleanUtils.toJavaBoolean(Integer.valueOf(2), Integer.valueOf(2), Integer.valueOf(0)) = true
    * JavaBooleanUtils.toJavaBoolean(null, null, Integer.valueOf(0))                     = true
    * </pre>
    *
    * @param value      the Integer to convert
    * @param trueValue  the value to match for {@code true}, may be {@code null}
    * @param falseValue the value to match for {@code false}, may be {@code null}
    * @return {@code true} or {@code false}
    * @throws IllegalArgumentException if no match
    */
  def toJavaBoolean(value: Integer, trueValue: Integer, falseValue: Integer): JavaBoolean = {
    if (value == null) {
      if (trueValue == null) return true
      if (falseValue == null) return false
    } else if (value == trueValue) return true
    else if (value == falseValue) return false
    throw new IllegalArgumentException("The Integer did not match either specified value")
  }

  /**
    * <p>Converts an int to a JavaBoolean specifying the conversion values.</p>
    *
    * <p>NOTE: This method may return {@code null} and may throw a {@code NullPointerException}
    * if unboxed to a {@code boolean}.</p>
    *
    * <p>The checks are done first for the {@code trueValue}, then for the {@code falseValue} and
    * finally for the {@code nullValue}.</p>
    *
    * <pre>
    * JavaBooleanUtils.toJavaBooleanObject(0, 0, 2, 3) = JavaBoolean.TRUE
    * JavaBooleanUtils.toJavaBooleanObject(0, 0, 0, 3) = JavaBoolean.TRUE
    * JavaBooleanUtils.toJavaBooleanObject(0, 0, 0, 0) = JavaBoolean.TRUE
    * JavaBooleanUtils.toJavaBooleanObject(2, 1, 2, 3) = JavaBoolean.FALSE
    * JavaBooleanUtils.toJavaBooleanObject(2, 1, 2, 2) = JavaBoolean.FALSE
    * JavaBooleanUtils.toJavaBooleanObject(3, 1, 2, 3) = null
    * </pre>
    *
    * @param value      the Integer to convert
    * @param trueValue  the value to match for {@code true}
    * @param falseValue the value to match for {@code false}
    * @param nullValue  the value to to match for {@code null}
    * @return JavaBoolean.TRUE, JavaBoolean.FALSE, or {@code null}
    * @throws IllegalArgumentException if no match
    */
  def toJavaBooleanObject(value: Int, trueValue: Int, falseValue: Int, nullValue: Int): JavaBoolean = {
    if (value == trueValue) return JavaBoolean.TRUE
    if (value == falseValue) return JavaBoolean.FALSE
    if (value == nullValue) return null
    throw new IllegalArgumentException("The Integer did not match any specified value")
  }

  /**
    * <p>Converts an Integer to a JavaBoolean specifying the conversion values.</p>
    *
    * <p>NOTE: This method may return {@code null} and may throw a {@code NullPointerException}
    * if unboxed to a {@code boolean}.</p>
    *
    * <p>The checks are done first for the {@code trueValue}, then for the {@code falseValue} and
    * finally for the {@code nullValue}.</p>
    * *
    * <pre>
    * JavaBooleanUtils.toJavaBooleanObject(Integer.valueOf(0), Integer.valueOf(0), Integer.valueOf(2), Integer.valueOf(3)) = JavaBoolean.TRUE
    * JavaBooleanUtils.toJavaBooleanObject(Integer.valueOf(0), Integer.valueOf(0), Integer.valueOf(0), Integer.valueOf(3)) = JavaBoolean.TRUE
    * JavaBooleanUtils.toJavaBooleanObject(Integer.valueOf(0), Integer.valueOf(0), Integer.valueOf(0), Integer.valueOf(0)) = JavaBoolean.TRUE
    * JavaBooleanUtils.toJavaBooleanObject(Integer.valueOf(2), Integer.valueOf(1), Integer.valueOf(2), Integer.valueOf(3)) = JavaBoolean.FALSE
    * JavaBooleanUtils.toJavaBooleanObject(Integer.valueOf(2), Integer.valueOf(1), Integer.valueOf(2), Integer.valueOf(2)) = JavaBoolean.FALSE
    * JavaBooleanUtils.toJavaBooleanObject(Integer.valueOf(3), Integer.valueOf(1), Integer.valueOf(2), Integer.valueOf(3)) = null
    * </pre>
    *
    * @param value      the Integer to convert
    * @param trueValue  the value to match for {@code true}, may be {@code null}
    * @param falseValue the value to match for {@code false}, may be {@code null}
    * @param nullValue  the value to to match for {@code null}, may be {@code null}
    * @return JavaBoolean.TRUE, JavaBoolean.FALSE, or {@code null}
    * @throws IllegalArgumentException if no match
    */
  def toJavaBooleanObject(value: Integer, trueValue: Integer, falseValue: Integer, nullValue: Integer): JavaBoolean = {
    if (value == null) {
      if (trueValue == null) return JavaBoolean.TRUE
      if (falseValue == null) return JavaBoolean.FALSE
      if (nullValue == null) return null
    } else if (value == trueValue) return JavaBoolean.TRUE
    else if (value == falseValue) return JavaBoolean.FALSE
    else if (value == nullValue) return null
    throw new IllegalArgumentException("The Integer did not match any specified value")
  }

  /**
    * <p>Converts a boolean to an int using the convention that
    * {@code true} is {@code 1} and {@code false} is {@code 0}.</p>
    *
    * <pre>
    * JavaBooleanUtils.toInteger(true)  = 1
    * JavaBooleanUtils.toInteger(false) = 0
    * </pre>
    *
    * @param bool the boolean to convert
    * @return one if {@code true}, zero if {@code false}
    */
  def toInteger(bool: JavaBoolean): Int =
    if (bool) 1
    else 0

  /**
    * <p>Converts a boolean to an Integer using the convention that
    * {@code true} is {@code 1} and {@code false} is {@code 0}.</p>
    *
    * <pre>
    * JavaBooleanUtils.toIntegerObject(true)  = Integer.valueOf(1)
    * JavaBooleanUtils.toIntegerObject(false) = Integer.valueOf(0)
    * </pre>
    *
    * @param bool the boolean to convert
    * @return one if {@code true}, zero if {@code false}
    */
  def toIntegerObject(bool: Boolean): Integer =
    if (bool) NumberUtils.INTEGER_ONE
    else NumberUtils.INTEGER_ZERO

  /**
    * <p>Converts a JavaBoolean to a Integer using the convention that
    * {@code zero} is {@code false}.</p>
    *
    * <p>{@code null} will be converted to {@code null}.</p>
    *
    * <pre>
    * JavaBooleanUtils.toIntegerObject(JavaBoolean.TRUE)  = Integer.valueOf(1)
    * JavaBooleanUtils.toIntegerObject(JavaBoolean.FALSE) = Integer.valueOf(0)
    * </pre>
    *
    * @param bool the JavaBoolean to convert
    * @return one if JavaBoolean.TRUE, zero if JavaBoolean.FALSE, {@code null} if {@code null}
    */
  def toIntegerObject(bool: JavaBoolean): Integer = {
    if (bool == null) return null
    if (bool.booleanValue) NumberUtils.INTEGER_ONE
    else NumberUtils.INTEGER_ZERO
  }

  /**
    * <p>Converts a boolean to an int specifying the conversion values.</p>
    *
    * <pre>
    * JavaBooleanUtils.toInteger(true, 1, 0)  = 1
    * JavaBooleanUtils.toInteger(false, 1, 0) = 0
    * </pre>
    *
    * @param bool       the to convert
    * @param trueValue  the value to return if {@code true}
    * @param falseValue the value to return if {@code false}
    * @return the appropriate value
    */
  def toInteger(bool: JavaBoolean, trueValue: Int, falseValue: Int): Int =
    if (bool) trueValue
    else falseValue

  /**
    * <p>Converts a JavaBoolean to an int specifying the conversion values.</p>
    *
    * <pre>
    * JavaBooleanUtils.toInteger(JavaBoolean.TRUE, 1, 0, 2)  = 1
    * JavaBooleanUtils.toInteger(JavaBoolean.FALSE, 1, 0, 2) = 0
    * JavaBooleanUtils.toInteger(null, 1, 0, 2)          = 2
    * </pre>
    *
    * @param bool       the JavaBoolean to convert
    * @param trueValue  the value to return if {@code true}
    * @param falseValue the value to return if {@code false}
    * @param nullValue  the value to return if {@code null}
    * @return the appropriate value
    */
  def toInteger(bool: JavaBoolean, trueValue: Int, falseValue: Int, nullValue: Int): Int = {
    if (bool == null) return nullValue
    if (bool.booleanValue) trueValue
    else falseValue
  }

  /**
    * <p>Converts a boolean to an Integer specifying the conversion values.</p>
    *
    * <pre>
    * BooleanUtils.toIntegerObject(true, Integer.valueOf(1), Integer.valueOf(0))  = Integer.valueOf(1)
    * BooleanUtils.toIntegerObject(false, Integer.valueOf(1), Integer.valueOf(0)) = Integer.valueOf(0)
    * </pre>
    *
    * @param bool       the to convert
    * @param trueValue  the value to return if {@code true}, may be {@code null}
    * @param falseValue the value to return if {@code false}, may be {@code null}
    * @return the appropriate value
    */
  def toIntegerObject(bool: Boolean, trueValue: Integer, falseValue: Integer): Integer =
    if (bool) trueValue
    else falseValue

  /**
    * <p>Converts a JavaBoolean to an Integer specifying the conversion values.</p>
    *
    * <pre>
    * JavaBooleanUtils.toIntegerObject(JavaBoolean.TRUE, Integer.valueOf(1), Integer.valueOf(0), Integer.valueOf(2))  = Integer.valueOf(1)
    * JavaBooleanUtils.toIntegerObject(JavaBoolean.FALSE, Integer.valueOf(1), Integer.valueOf(0), Integer.valueOf(2)) = Integer.valueOf(0)
    * JavaBooleanUtils.toIntegerObject(null, Integer.valueOf(1), Integer.valueOf(0), Integer.valueOf(2))          = Integer.valueOf(2)
    * </pre>
    *
    * @param bool       the JavaBoolean to convert
    * @param trueValue  the value to return if {@code true}, may be {@code null}
    * @param falseValue the value to return if {@code false}, may be {@code null}
    * @param nullValue  the value to return if {@code null}, may be {@code null}
    * @return the appropriate value
    */
  def toIntegerObject(bool: JavaBoolean, trueValue: Integer, falseValue: Integer, nullValue: Integer): Integer = {
    if (bool == null) return nullValue
    if (bool.booleanValue) trueValue
    else falseValue
  }

  /**
    * <p>Converts a String to a JavaBoolean.</p>
    *
    * <p>{@code 'true'}, {@code 'on'}, {@code 'y'}, {@code 't'}, {@code 'yes'}
    * or {@code '1'} (case insensitive) will return {@code true}.
    * {@code 'false'}, {@code 'off'}, {@code 'n'}, {@code 'f'}, {@code 'no'}
    * or {@code '0'} (case insensitive) will return {@code false}.
    * Otherwise, {@code null} is returned.</p>
    *
    * <p>NOTE: This method may return {@code null} and may throw a {@code NullPointerException}
    * if unboxed to a {@code boolean}.</p>
    *
    * <pre>
    * // N.B. case is not significant
    * JavaBooleanUtils.toJavaBooleanObject(null)    = null
    * JavaBooleanUtils.toJavaBooleanObject("true")  = JavaBoolean.TRUE
    * JavaBooleanUtils.toJavaBooleanObject("T")     = JavaBoolean.TRUE // i.e. T[RUE]
    * JavaBooleanUtils.toJavaBooleanObject("false") = JavaBoolean.FALSE
    * JavaBooleanUtils.toJavaBooleanObject("f")     = JavaBoolean.FALSE // i.e. f[alse]
    * JavaBooleanUtils.toJavaBooleanObject("No")    = JavaBoolean.FALSE
    * JavaBooleanUtils.toJavaBooleanObject("n")     = JavaBoolean.FALSE // i.e. n[o]
    * JavaBooleanUtils.toJavaBooleanObject("on")    = JavaBoolean.TRUE
    * JavaBooleanUtils.toJavaBooleanObject("ON")    = JavaBoolean.TRUE
    * JavaBooleanUtils.toJavaBooleanObject("off")   = JavaBoolean.FALSE
    * JavaBooleanUtils.toJavaBooleanObject("oFf")   = JavaBoolean.FALSE
    * JavaBooleanUtils.toJavaBooleanObject("yes")   = JavaBoolean.TRUE
    * JavaBooleanUtils.toJavaBooleanObject("Y")     = JavaBoolean.TRUE // i.e. Y[ES]
    * JavaBooleanUtils.toJavaBooleanObject("1")     = JavaBoolean.TRUE
    * JavaBooleanUtils.toJavaBooleanObject("0")     = JavaBoolean.FALSE
    * JavaBooleanUtils.toJavaBooleanObject("blue")  = null
    * JavaBooleanUtils.toJavaBooleanObject("true ") = null // trailing space (too long)
    * JavaBooleanUtils.toJavaBooleanObject("ono")   = null // does not match on or no
    * </pre>
    *
    * @param str the String to check; upper and lower case are treated as the same
    * @return the JavaBoolean value of the string, {@code null} if no match or {@code null} input
    */
  def toJavaBooleanObject(str: String): JavaBoolean = { // Previously used equalsIgnoreCase, which was fast for interned 'true'.
    // Non interned 'true' matched 15 times slower.
    //
    // Optimisation provides same performance as before for interned 'true'.
    // Similar performance for null, 'false', and other strings not length 2/3/4.
    // 'true'/'TRUE' match 4 times slower, 'tRUE'/'True' 7 times slower.
    if (str eq "true") return JavaBoolean.TRUE
    if (str == null) return null
    str.length match {
      case 1 =>
        val ch0 = str.charAt(0)
        if (ch0 == 'y' || ch0 == 'Y' || ch0 == 't' || ch0 == 'T' || ch0 == '1') return JavaBoolean.TRUE
        if (ch0 == 'n' || ch0 == 'N' || ch0 == 'f' || ch0 == 'F' || ch0 == '0') return JavaBoolean.FALSE

      case 2 =>
        val ch0 = str.charAt(0)
        val ch1 = str.charAt(1)
        if ((ch0 == 'o' || ch0 == 'O') && (ch1 == 'n' || ch1 == 'N')) return JavaBoolean.TRUE
        if ((ch0 == 'n' || ch0 == 'N') && (ch1 == 'o' || ch1 == 'O')) return JavaBoolean.FALSE

      case 3 =>
        val ch0 = str.charAt(0)
        val ch1 = str.charAt(1)
        val ch2 = str.charAt(2)
        if ((ch0 == 'y' || ch0 == 'Y') && (ch1 == 'e' || ch1 == 'E') && (ch2 == 's' || ch2 == 'S'))
          return JavaBoolean.TRUE
        if ((ch0 == 'o' || ch0 == 'O') && (ch1 == 'f' || ch1 == 'F') && (ch2 == 'f' || ch2 == 'F'))
          return JavaBoolean.FALSE

      case 4 =>
        val ch0 = str.charAt(0)
        val ch1 = str.charAt(1)
        val ch2 = str.charAt(2)
        val ch3 = str.charAt(3)
        if ((ch0 == 't' || ch0 == 'T') && (ch1 == 'r' || ch1 == 'R') && (ch2 == 'u' || ch2 == 'U') && (ch3 == 'e' || ch3 == 'E'))
          return JavaBoolean.TRUE

      case 5 =>
        val ch0 = str.charAt(0)
        val ch1 = str.charAt(1)
        val ch2 = str.charAt(2)
        val ch3 = str.charAt(3)
        val ch4 = str.charAt(4)
        if ((ch0 == 'f' || ch0 == 'F') && (ch1 == 'a' || ch1 == 'A') && (ch2 == 'l' || ch2 == 'L') && (ch3 == 's' || ch3 == 'S') && (ch4 == 'e' || ch4 == 'E'))
          return JavaBoolean.FALSE

      case _ =>
    }
    null
  }

  /**
    * <p>Converts a String to a JavaBoolean throwing an exception if no match.</p>
    *
    * <p>NOTE: This method may return {@code null} and may throw a {@code NullPointerException}
    * if unboxed to a {@code boolean}.</p>
    *
    * <pre>
    * JavaBooleanUtils.toJavaBooleanObject("true", "true", "false", "null")   = JavaBoolean.TRUE
    * JavaBooleanUtils.toJavaBooleanObject(null, null, "false", "null")       = JavaBoolean.TRUE
    * JavaBooleanUtils.toJavaBooleanObject(null, null, null, "null")          = JavaBoolean.TRUE
    * JavaBooleanUtils.toJavaBooleanObject(null, null, null, null)            = JavaBoolean.TRUE
    * JavaBooleanUtils.toJavaBooleanObject("false", "true", "false", "null")  = JavaBoolean.FALSE
    * JavaBooleanUtils.toJavaBooleanObject("false", "true", "false", "false") = JavaBoolean.FALSE
    * JavaBooleanUtils.toJavaBooleanObject(null, "true", null, "false")       = JavaBoolean.FALSE
    * JavaBooleanUtils.toJavaBooleanObject(null, "true", null, null)          = JavaBoolean.FALSE
    * JavaBooleanUtils.toJavaBooleanObject("null", "true", "false", "null")   = null
    * </pre>
    *
    * @param str         the String to check
    * @param trueString  the String to match for {@code true} (case sensitive), may be {@code null}
    * @param falseString the String to match for {@code false} (case sensitive), may be {@code null}
    * @param nullString  the String to match for {@code null} (case sensitive), may be {@code null}
    * @return the JavaBoolean value of the string, {@code null} if either the String matches {@code nullString}
    *         or if {@code null} input and {@code nullString} is {@code null}
    * @throws IllegalArgumentException if the String doesn't match
    */
  def toJavaBooleanObject(str: String, trueString: String, falseString: String, nullString: String): JavaBoolean = {
    if (str == null) {
      if (trueString == null) return JavaBoolean.TRUE
      if (falseString == null) return JavaBoolean.FALSE
      if (nullString == null) return null
    } else if (str == trueString) return JavaBoolean.TRUE
    else if (str == falseString) return JavaBoolean.FALSE
    else if (str == nullString) return null
    // no match
    throw new IllegalArgumentException("The String did not match any specified value")
  }

  /**
    * <p>Converts a String to a boolean (optimised for performance).</p>
    *
    * <p>{@code 'true'}, {@code 'on'}, {@code 'y'}, {@code 't'} or {@code 'yes'}
    * (case insensitive) will return {@code true}. Otherwise,
    * {@code false} is returned.</p>
    *
    * <p>This method performs 4 times faster (JDK1.4) than
    * {@code JavaBoolean.valueOf(String)}. However, this method accepts
    * 'on' and 'yes', 't', 'y' as true values.
    *
    * <pre>
    * JavaBooleanUtils.toJavaBoolean(null)    = false
    * JavaBooleanUtils.toJavaBoolean("true")  = true
    * JavaBooleanUtils.toJavaBoolean("TRUE")  = true
    * JavaBooleanUtils.toJavaBoolean("tRUe")  = true
    * JavaBooleanUtils.toJavaBoolean("on")    = true
    * JavaBooleanUtils.toJavaBoolean("yes")   = true
    * JavaBooleanUtils.toJavaBoolean("false") = false
    * JavaBooleanUtils.toJavaBoolean("x gti") = false
    * JavaBooleanUtils.toJavaBooleanObject("y") = true
    * JavaBooleanUtils.toJavaBooleanObject("n") = false
    * JavaBooleanUtils.toJavaBooleanObject("t") = true
    * JavaBooleanUtils.toJavaBooleanObject("f") = false
    * </pre>
    *
    * @param str the String to check
    * @return the boolean value of the string, {@code false} if no match or the String is null
    */
  def toJavaBoolean(str: String): JavaBoolean = toJavaBooleanObject(str) eq JavaBoolean.TRUE

  /**
    * <p>Converts a String to a JavaBoolean throwing an exception if no match found.</p>
    *
    * <pre>
    * JavaBooleanUtils.toJavaBoolean("true", "true", "false")  = true
    * JavaBooleanUtils.toJavaBoolean("false", "true", "false") = false
    * </pre>
    *
    * @param str         the String to check
    * @param trueString  the String to match for {@code true} (case sensitive), may be {@code null}
    * @param falseString the String to match for {@code false} (case sensitive), may be {@code null}
    * @return the boolean value of the string
    * @throws IllegalArgumentException if the String doesn't match
    */
  def toJavaBoolean(str: String, trueString: String, falseString: String): JavaBoolean = {
    if (str eq trueString) return true
    else if (str eq falseString) return false
    else if (str != null)
      if (str == trueString) return true
      else if (str == falseString) return false
    throw new IllegalArgumentException("The String did not match either specified value")
  }

  /**
    * <p>Converts a JavaBoolean to a String returning {@code 'true'},
    * {@code 'false'}, or {@code null}.</p>
    *
    * <pre>
    * JavaBooleanUtils.toStringTrueFalse(JavaBoolean.TRUE)  = "true"
    * JavaBooleanUtils.toStringTrueFalse(JavaBoolean.FALSE) = "false"
    * JavaBooleanUtils.toStringTrueFalse(null)          = null;
    * </pre>
    *
    * @param bool the JavaBoolean to check
    * @return {@code 'true'}, {@code 'false'}, or {@code null}
    */
  def toStringTrueFalse(bool: JavaBoolean): String = toString(bool, "true", "false", null)

  /**
    * <p>Converts a JavaBoolean to a String returning {@code 'on'},
    * {@code 'off'}, or {@code null}.</p>
    *
    * <pre>
    * JavaBooleanUtils.toStringOnOff(JavaBoolean.TRUE)  = "on"
    * JavaBooleanUtils.toStringOnOff(JavaBoolean.FALSE) = "off"
    * JavaBooleanUtils.toStringOnOff(null)          = null;
    * </pre>
    *
    * @param bool the JavaBoolean to check
    * @return {@code 'on'}, {@code 'off'}, or {@code null}
    */
  def toStringOnOff(bool: JavaBoolean): String = toString(bool, "on", "off", null)

  /**
    * <p>Converts a Boolean to a String returning {@code 'yes'},
    * {@code 'no'}, or {@code null}.</p>
    *
    * <pre>
    * BooleanUtils.toStringYesNo(Boolean.TRUE)  = "yes"
    * BooleanUtils.toStringYesNo(Boolean.FALSE) = "no"
    * BooleanUtils.toStringYesNo(null)          = null;
    * </pre>
    *
    * @param bool the JavaBoolean to check
    * @return {@code 'yes'}, {@code 'no'}, or {@code null}
    */
  def toStringYesNo(bool: JavaBoolean): String = toString(bool, "yes", "no", null)

  /**
    * <p>Converts a JavaBoolean to a String returning one of the input Strings.</p>
    *
    * <pre>
    * JavaBooleanUtils.toString(JavaBoolean.TRUE, "true", "false", null)   = "true"
    * JavaBooleanUtils.toString(JavaBoolean.FALSE, "true", "false", null)  = "false"
    * JavaBooleanUtils.toString(null, "true", "false", null)           = null;
    * </pre>
    *
    * @param bool        the JavaBoolean to check
    * @param trueString  the String to return if {@code true}, may be {@code null}
    * @param falseString the String to return if {@code false}, may be {@code null}
    * @param nullString  the String to return if {@code null}, may be {@code null}
    * @return one of the three input Strings
    */
  def toString(bool: JavaBoolean, trueString: String, falseString: String, nullString: String): String = {
    if (bool == null) return nullString
    if (bool.booleanValue) trueString
    else falseString
  }

  /**
    * <p>Converts a boolean to a String returning {@code 'true'}
    * or {@code 'false'}.</p>
    *
    * <pre>
    * JavaBooleanUtils.toStringTrueFalse(true)   = "true"
    * JavaBooleanUtils.toStringTrueFalse(false)  = "false"
    * </pre>
    *
    * @param bool the JavaBoolean to check
    * @return {@code 'true'}, {@code 'false'}, or {@code null}
    */
  def toStringTrueFalse(bool: Boolean): String = toString(bool, "true", "false")

  /**
    * <p>Converts a boolean to a String returning {@code 'on'}
    * or {@code 'off'}.</p>
    *
    * <pre>
    * JavaBooleanUtils.toStringOnOff(true)   = "on"
    * JavaBooleanUtils.toStringOnOff(false)  = "off"
    * </pre>
    *
    * @param bool the JavaBoolean to check
    * @return {@code 'on'}, {@code 'off'}, or {@code null}
    */
  def toStringOnOff(bool: Boolean): String = toString(bool, "on", "off")

  /**
    * <p>Converts a boolean to a String returning {@code 'yes'}
    * or {@code 'no'}.</p>
    *
    * <pre>
    * BooleanUtils.toStringYesNo(true)   = "yes"
    * BooleanUtils.toStringYesNo(false)  = "no"
    * </pre>
    *
    * @param bool the JavaBoolean to check
    * @return {@code 'yes'}, {@code 'no'}, or {@code null}
    */
  def toStringYesNo(bool: Boolean): String = toString(bool, "yes", "no")

  /**
    * <p>Converts a boolean to a String returning one of the input Strings.</p>
    *
    * <pre>
    * JavaBooleanUtils.toString(true, "true", "false")   = "true"
    * JavaBooleanUtils.toString(false, "true", "false")  = "false"
    * </pre>
    *
    * @param bool        the JavaBoolean to check
    * @param trueString  the String to return if {@code true}, may be {@code null}
    * @param falseString the String to return if {@code false}, may be {@code null}
    * @return one of the two input Strings
    */
  def toString(bool: JavaBoolean, trueString: String, falseString: String): String =
    if (bool) trueString
    else falseString

  /**
    * <p>Performs an 'and' operation on a set of booleans.</p>
    *
    * <pre>
    * JavaBooleanUtils.and(true, true)         = true
    * JavaBooleanUtils.and(false, false)       = false
    * JavaBooleanUtils.and(true, false)        = false
    * JavaBooleanUtils.and(true, true, false)  = false
    * JavaBooleanUtils.and(true, true, true)   = true
    * </pre>
    *
    * @param array an array of {@code boolean}s
    * @return the result of the logical 'and' operation. That is {@code false}
    *         if any of the parameters is {@code false} and {@code true} otherwise.
    * @throws IllegalArgumentException if {@code array} is {@code null}
    * @throws IllegalArgumentException if {@code array} is empty.
    * @since 3.0.1
    */
  def and(array: Boolean*): Boolean = { // Validates input
    if (array == null) throw new IllegalArgumentException("The Array must not be null")
    if (array.length == 0) throw new IllegalArgumentException("Array is empty")

    for (element <- array) {
      if (!element) return false
    }

    true
  }

  /**
    * <p>Performs an 'and' operation on an array of Booleans.</p>
    *
    * <pre>
    * BooleanUtils.and(Boolean.TRUE, Boolean.TRUE)                 = Boolean.TRUE
    * BooleanUtils.and(Boolean.FALSE, Boolean.FALSE)               = Boolean.FALSE
    * BooleanUtils.and(Boolean.TRUE, Boolean.FALSE)                = Boolean.FALSE
    * BooleanUtils.and(Boolean.TRUE, Boolean.TRUE, Boolean.TRUE)   = Boolean.TRUE
    * BooleanUtils.and(Boolean.FALSE, Boolean.FALSE, Boolean.TRUE) = Boolean.FALSE
    * BooleanUtils.and(Boolean.TRUE, Boolean.FALSE, Boolean.TRUE)  = Boolean.FALSE
    * </pre>
    *
    * @param array an array of {@code JavaBoolean}s
    * @return the result of the logical 'and' operation. That is {@code false}
    *         if any of the parameters is {@code false} and {@code true} otherwise.
    * @throws IllegalArgumentException if {@code array} is {@code null}
    * @throws IllegalArgumentException if {@code array} is empty.
    * @throws IllegalArgumentException if {@code array} contains a {@code null}
    * @since 3.0.1
    */
  def and(array: JavaBoolean*): JavaBoolean = {
    if (array == null) throw new IllegalArgumentException("The Array must not be null")
    if (array.length == 0) throw new IllegalArgumentException("Array is empty")

    try {
      val primitive = ArrayUtils.toPrimitive(array.toArray)
      if (and(primitive: _*)) JavaBoolean.TRUE
      else JavaBoolean.FALSE
    } catch {
      case _: NullPointerException =>
        throw new IllegalArgumentException("The array must not contain any null elements")
    }
  }

  /**
    * <p>Performs an 'or' operation on a set of booleans.</p>
    *
    * <pre>
    * BooleanUtils.or(true, true)          = true
    * BooleanUtils.or(false, false)        = false
    * BooleanUtils.or(true, false)         = true
    * BooleanUtils.or(true, true, false)   = true
    * BooleanUtils.or(true, true, true)    = true
    * BooleanUtils.or(false, false, false) = false
    * </pre>
    *
    * @param array an array of {@code boolean}s
    * @return {@code true} if any of the arguments is {@code true}, and it returns {@code false} otherwise.
    * @throws IllegalArgumentException if {@code array} is {@code null}
    * @throws IllegalArgumentException if {@code array} is empty.
    * @since 3.0.1
    */
  def or(array: Boolean*): Boolean = {
    if (array == null) throw new IllegalArgumentException("The Array must not be null")
    if (array.length == 0) throw new IllegalArgumentException("Array is empty")

    for (element <- array) {
      if (element) return true
    }

    false
  }

  /**
    * <p>Performs an 'or' operation on an array of JavaBooleans.</p>
    *
    * <pre>
    * BooleanUtils.or(Boolean.TRUE, Boolean.TRUE)                  = Boolean.TRUE
    * BooleanUtils.or(Boolean.FALSE, Boolean.FALSE)                = Boolean.FALSE
    * BooleanUtils.or(Boolean.TRUE, Boolean.FALSE)                 = Boolean.TRUE
    * BooleanUtils.or(Boolean.TRUE, Boolean.TRUE, Boolean.TRUE)    = Boolean.TRUE
    * BooleanUtils.or(Boolean.FALSE, Boolean.FALSE, Boolean.TRUE)  = Boolean.TRUE
    * BooleanUtils.or(Boolean.TRUE, Boolean.FALSE, Boolean.TRUE)   = Boolean.TRUE
    * BooleanUtils.or(Boolean.FALSE, Boolean.FALSE, Boolean.FALSE) = Boolean.FALSE
    * </pre>
    *
    * @param array an array of {@code JavaBoolean}s
    * @return {@code true} if any of the arguments is {@code true}, and it returns {@code false} otherwise.
    * @throws IllegalArgumentException if {@code array} is {@code null}
    * @throws IllegalArgumentException if {@code array} is empty.
    * @throws IllegalArgumentException if {@code array} contains a {@code null}
    * @since 3.0.1
    */
  def or(array: JavaBoolean*): JavaBoolean = {
    if (array == null) throw new IllegalArgumentException("The Array must not be null")
    if (array.length == 0) throw new IllegalArgumentException("Array is empty")

    try {
      val primitive = ArrayUtils.toPrimitive(array.toArray)
      if (or(primitive: _*)) JavaBoolean.TRUE
      else JavaBoolean.FALSE
    } catch {
      case _: NullPointerException =>
        throw new IllegalArgumentException("The array must not contain any null elements")
    }
  }

  /**
    * <p>Performs an xor on a set of booleans.</p>
    *
    * <pre>
    * JavaBooleanUtils.xor(true, true)   = false
    * JavaBooleanUtils.xor(false, false) = false
    * JavaBooleanUtils.xor(true, false)  = true
    * </pre>
    *
    * @param array an array of {@code boolean}s
    * @return the result of the xor operations
    * @throws IllegalArgumentException if {@code array} is {@code null}
    * @throws IllegalArgumentException if {@code array} is empty.
    */
  def xor(array: Boolean*): Boolean = {
    if (array == null) throw new IllegalArgumentException("The Array must not be null")
    if (array.length == 0) throw new IllegalArgumentException("Array is empty")

    // false if the neutral element of the xor operator
    var result = false
    for (element <- array) {
      result ^= element
    }

    result
  }

  /**
    * <p>Performs an xor on an array of JavaBooleans.</p>
    *
    * <pre>
    * JavaBooleanUtils.xor(new JavaBoolean[] { JavaBoolean.TRUE, JavaBoolean.TRUE })   = JavaBoolean.FALSE
    * JavaBooleanUtils.xor(new JavaBoolean[] { JavaBoolean.FALSE, JavaBoolean.FALSE }) = JavaBoolean.FALSE
    * JavaBooleanUtils.xor(new JavaBoolean[] { JavaBoolean.TRUE, JavaBoolean.FALSE })  = JavaBoolean.TRUE
    * JavaBooleanUtils.xor(JavaBoolean.TRUE, JavaBoolean.FALSE, JavaBoolean.FALSE)     = JavaBoolean.TRUE
    * </pre>
    *
    * @param array an array of {@code JavaBoolean}s
    * @return the result of the xor operations
    * @throws IllegalArgumentException if {@code array} is {@code null}
    * @throws IllegalArgumentException if {@code array} is empty.
    * @throws IllegalArgumentException if {@code array} contains a {@code null}
    */
  def xor(array: JavaBoolean*): JavaBoolean = {
    if (array == null) throw new IllegalArgumentException("The Array must not be null")
    if (array.length == 0) throw new IllegalArgumentException("Array is empty")

    try {
      val primitive = ArrayUtils.toPrimitive(array.toArray)
      if (xor(primitive: _*)) JavaBoolean.TRUE
      else JavaBoolean.FALSE
    } catch {
      case _: NullPointerException =>
        throw new IllegalArgumentException("The array must not contain any null elements")
    }
  }

  /**
    * <p>Compares two {@code boolean} values. This is the same functionality as provided in Java 7.</p>
    *
    * @param x the first {@code boolean} to compare
    * @param y the second {@code boolean} to compare
    * @return the value {@code 0} if {@code x == y};
    *         a value less than {@code 0} if {@code !x && y}; and
    *         a value greater than {@code 0} if {@code x && !y}
    * @since 3.4
    */
  def compare(x: JavaBoolean, y: JavaBoolean): Int = {
    if (x == y) return 0
    if (x) 1
    else -1
  }
}
