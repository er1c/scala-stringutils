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

package org.apache.commons.lang3.math

import java.lang.reflect
import java.math.BigDecimal
import java.math.BigInteger
import java.math.RoundingMode
import org.apache.commons.lang3.StringUtils
import org.apache.commons.lang3.Validate
import scala.util.control.Breaks
import java.lang.{Long => JavaLong}

/**
  * <p>Provides extra functionality for Java Number classes.</p>
  *
  * @since 2.0
  */
object NumberUtils {
  /**
    * Our own copy of breaks to avoid conflicts with any other breaks:
    * "Calls to break from one instantiation of Breaks will never target breakable objects of some other instantiation."
    */
  private val breaks: Breaks = new Breaks
  import breaks._

  /** Reusable Long constant for zero. */
  val LONG_ZERO: Long = 0L
  /** Reusable Long constant for one. */
  val LONG_ONE: Long = 1L
  /** Reusable Long constant for minus one. */
  val LONG_MINUS_ONE: Long = -1L
  /** Reusable Integer constant for zero. */
  val INTEGER_ZERO: Integer = Integer.valueOf(0)
  /** Reusable Integer constant for one. */
  val INTEGER_ONE: Integer = Integer.valueOf(1)
  /** Reusable Integer constant for two */
  val INTEGER_TWO: Integer = Integer.valueOf(2)
  /** Reusable Integer constant for minus one. */
  val INTEGER_MINUS_ONE: Integer = Integer.valueOf(-1)
  /** Reusable Short constant for zero. */
  val SHORT_ZERO: Short = 0.toShort
  /** Reusable Short constant for one. */
  val SHORT_ONE: Short = 1.toShort
  /** Reusable Short constant for minus one. */
  val SHORT_MINUS_ONE: Short = (-1).toShort
  /** Reusable Byte constant for zero. */
  val BYTE_ZERO: Byte = 0.toByte
  /** Reusable Byte constant for one. */
  val BYTE_ONE: Byte = 1.toByte
  /** Reusable Byte constant for minus one. */
  val BYTE_MINUS_ONE: Byte = (-1).toByte
  /** Reusable Double constant for zero. */
  val DOUBLE_ZERO: Double = 0.0d
  /** Reusable Double constant for one. */
  val DOUBLE_ONE: Double = 1.0d
  /** Reusable Double constant for minus one. */
  val DOUBLE_MINUS_ONE: Double = -1.0d
  /** Reusable Float constant for zero. */
  val FLOAT_ZERO: Float = 0.0f
  /** Reusable Float constant for one. */
  val FLOAT_ONE: Float = 1.0f
  /** Reusable Float constant for minus one. */
  val FLOAT_MINUS_ONE: Float = -1.0f

  /**
    * <p>Convert a {@code String} to an {@code int}, returning
    * {@code zero} if the conversion fails.</p>
    *
    * <p>If the string is {@code null}, {@code zero} is returned.</p>
    *
    * <pre>
    * NumberUtils.toInt(null) = 0
    * NumberUtils.toInt("")   = 0
    * NumberUtils.toInt("1")  = 1
    * </pre>
    *
    * @param str the string to convert, may be null
    * @return the int represented by the string, or {@code zero} if
    *         conversion fails
    * @since 2.1
    */
  def toInt(str: String): Int = toInt(str, 0)

  /**
    * <p>Convert a {@code String} to an {@code int}, returning a
    * default value if the conversion fails.</p>
    *
    * <p>If the string is {@code null}, the default value is returned.</p>
    *
    * <pre>
    * NumberUtils.toInt(null, 1) = 1
    * NumberUtils.toInt("", 1)   = 1
    * NumberUtils.toInt("1", 0)  = 1
    * </pre>
    *
    * @param str          the string to convert, may be null
    * @param defaultValue the default value
    * @return the int represented by the string, or the default if conversion fails
    * @since 2.1
    */
  def toInt(str: String, defaultValue: Int): Int = {
    if (str == null) return defaultValue
    try str.toInt
    catch {
      case _: NumberFormatException => defaultValue
    }
  }

  /**
    * <p>Convert a {@code String} to a {@code long}, returning
    * {@code zero} if the conversion fails.</p>
    *
    * <p>If the string is {@code null}, {@code zero} is returned.</p>
    *
    * <pre>
    * NumberUtils.toLong(null) = 0L
    * NumberUtils.toLong("")   = 0L
    * NumberUtils.toLong("1")  = 1L
    * </pre>
    *
    * @param str the string to convert, may be null
    * @return the long represented by the string, or {@code 0} if
    *         conversion fails
    * @since 2.1
    */
  def toLong(str: String): Long = toLong(str, 0L)

  /**
    * <p>Convert a {@code String} to a {@code long}, returning a
    * default value if the conversion fails.</p>
    *
    * <p>If the string is {@code null}, the default value is returned.</p>
    *
    * <pre>
    * NumberUtils.toLong(null, 1L) = 1L
    * NumberUtils.toLong("", 1L)   = 1L
    * NumberUtils.toLong("1", 0L)  = 1L
    * </pre>
    *
    * @param str          the string to convert, may be null
    * @param defaultValue the default value
    * @return the long represented by the string, or the default if conversion fails
    * @since 2.1
    */
  def toLong(str: String, defaultValue: Long): Long = {
    if (str == null) return defaultValue
    try str.toLong
    catch {
      case _: NumberFormatException => defaultValue
    }
  }

  /**
    * <p>Convert a {@code String} to a {@code float}, returning
    * {@code 0.0f} if the conversion fails.</p>
    *
    * <p>If the string {@code str} is {@code null},
    * {@code 0.0f} is returned.</p>
    *
    * <pre>
    * NumberUtils.toFloat(null)   = 0.0f
    * NumberUtils.toFloat("")     = 0.0f
    * NumberUtils.toFloat("1.5")  = 1.5f
    * </pre>
    *
    * @param str the string to convert, may be {@code null}
    * @return the float represented by the string, or {@code 0.0f}
    *         if conversion fails
    * @since 2.1
    */
  def toFloat(str: String): Float = toFloat(str, 0.0f)

  /**
    * <p>Convert a {@code String} to a {@code float}, returning a
    * default value if the conversion fails.</p>
    *
    * <p>If the string {@code str} is {@code null}, the default
    * value is returned.</p>
    *
    * <pre>
    * NumberUtils.toFloat(null, 1.1f)   = 1.0f
    * NumberUtils.toFloat("", 1.1f)     = 1.1f
    * NumberUtils.toFloat("1.5", 0.0f)  = 1.5f
    * </pre>
    *
    * @param str          the string to convert, may be {@code null}
    * @param defaultValue the default value
    * @return the float represented by the string, or defaultValue
    *         if conversion fails
    * @since 2.1
    */
  def toFloat(str: String, defaultValue: Float): Float = {
    if (str == null) return defaultValue
    try str.toFloat
    catch {
      case _: NumberFormatException => defaultValue
    }
  }

  /**
    * <p>Convert a {@code String} to a {@code double}, returning
    * {@code 0.0d} if the conversion fails.</p>
    *
    * <p>If the string {@code str} is {@code null},
    * {@code 0.0d} is returned.</p>
    *
    * <pre>
    * NumberUtils.toDouble(null)   = 0.0d
    * NumberUtils.toDouble("")     = 0.0d
    * NumberUtils.toDouble("1.5")  = 1.5d
    * </pre>
    *
    * @param str the string to convert, may be {@code null}
    * @return the double represented by the string, or {@code 0.0d}
    *         if conversion fails
    * @since 2.1
    */
  def toDouble(str: String): Double = toDouble(str, 0.0d)

  /**
    * <p>Convert a {@code String} to a {@code double}, returning a
    * default value if the conversion fails.</p>
    *
    * <p>If the string {@code str} is {@code null}, the default
    * value is returned.</p>
    *
    * <pre>
    * NumberUtils.toDouble(null, 1.1d)   = 1.1d
    * NumberUtils.toDouble("", 1.1d)     = 1.1d
    * NumberUtils.toDouble("1.5", 0.0d)  = 1.5d
    * </pre>
    *
    * @param str          the string to convert, may be {@code null}
    * @param defaultValue the default value
    * @return the double represented by the string, or defaultValue
    *         if conversion fails
    * @since 2.1
    */
  def toDouble(str: String, defaultValue: Double): Double = {
    if (str == null) return defaultValue
    try str.toDouble
    catch {
      case _: NumberFormatException => defaultValue
    }
  }

  /**
    * <p>Convert a {@code BigDecimal} to a {@code double}.</p>
    *
    * <p>If the {@code BigDecimal} {@code value} is
    * {@code null}, then the specified default value is returned.</p>
    *
    * <pre>
    * NumberUtils.toDouble(null)                     = 0.0d
    * NumberUtils.toDouble(BigDecimal.valudOf(8.5d)) = 8.5d
    * </pre>
    *
    * @param value the {@code BigDecimal} to convert, may be {@code null}.
    * @return the double represented by the {@code BigDecimal} or
    *         {@code 0.0d} if the {@code BigDecimal} is {@code null}.
    * @since 3.8
    */
  def toDouble(value: BigDecimal): Double = toDouble(value, 0.0d)

  /**
    * <p>Convert a {@code BigDecimal} to a {@code double}.</p>
    *
    * <p>If the {@code BigDecimal} {@code value} is
    * {@code null}, then the specified default value is returned.</p>
    *
    * <pre>
    * NumberUtils.toDouble(null, 1.1d)                     = 1.1d
    * NumberUtils.toDouble(BigDecimal.valudOf(8.5d), 1.1d) = 8.5d
    * </pre>
    *
    * @param value        the {@code BigDecimal} to convert, may be {@code null}.
    * @param defaultValue the default value
    * @return the double represented by the {@code BigDecimal} or the
    *         defaultValue if the {@code BigDecimal} is {@code null}.
    * @since 3.8
    */
  def toDouble(value: BigDecimal, defaultValue: Double): Double = if (value == null) defaultValue
  else value.doubleValue

  /**
    * <p>Convert a {@code String} to a {@code byte}, returning
    * {@code zero} if the conversion fails.</p>
    *
    * <p>If the string is {@code null}, {@code zero} is returned.</p>
    *
    * <pre>
    * NumberUtils.toByte(null) = 0
    * NumberUtils.toByte("")   = 0
    * NumberUtils.toByte("1")  = 1
    * </pre>
    *
    * @param str the string to convert, may be null
    * @return the byte represented by the string, or {@code zero} if
    *         conversion fails
    * @since 2.5
    */
  def toByte(str: String): Byte = toByte(str, 0.toByte)

  /**
    * <p>Convert a {@code String} to a {@code byte}, returning a
    * default value if the conversion fails.</p>
    *
    * <p>If the string is {@code null}, the default value is returned.</p>
    *
    * <pre>
    * NumberUtils.toByte(null, 1) = 1
    * NumberUtils.toByte("", 1)   = 1
    * NumberUtils.toByte("1", 0)  = 1
    * </pre>
    *
    * @param str          the string to convert, may be null
    * @param defaultValue the default value
    * @return the byte represented by the string, or the default if conversion fails
    * @since 2.5
    */
  def toByte(str: String, defaultValue: Byte): Byte = {
    if (str == null) return defaultValue
    try str.toByte
    catch {
      case _: NumberFormatException => defaultValue
    }
  }

  /**
    * <p>Convert a {@code String} to a {@code short}, returning
    * {@code zero} if the conversion fails.</p>
    *
    * <p>If the string is {@code null}, {@code zero} is returned.</p>
    *
    * <pre>
    * NumberUtils.toShort(null) = 0
    * NumberUtils.toShort("")   = 0
    * NumberUtils.toShort("1")  = 1
    * </pre>
    *
    * @param str the string to convert, may be null
    * @return the short represented by the string, or {@code zero} if
    *         conversion fails
    * @since 2.5
    */
  def toShort(str: String): Short = toShort(str, 0.toShort)

  /**
    * <p>Convert a {@code String} to an {@code short}, returning a
    * default value if the conversion fails.</p>
    *
    * <p>If the string is {@code null}, the default value is returned.</p>
    *
    * <pre>
    * NumberUtils.toShort(null, 1) = 1
    * NumberUtils.toShort("", 1)   = 1
    * NumberUtils.toShort("1", 0)  = 1
    * </pre>
    *
    * @param str          the string to convert, may be null
    * @param defaultValue the default value
    * @return the short represented by the string, or the default if conversion fails
    * @since 2.5
    */
  def toShort(str: String, defaultValue: Short): Short = {
    if (str == null) return defaultValue
    try str.toShort
    catch {
      case _: NumberFormatException => defaultValue
    }
  }

  /**
    * Convert a {@code BigDecimal} to a {@code BigDecimal} with a scale of
    * two that has been rounded using {@code RoundingMode.HALF_EVEN}. If the supplied
    * {@code value} is null, then {@code BigDecimal.ZERO} is returned.
    *
    * <p>Note, the scale of a {@code BigDecimal} is the number of digits to the right of the
    * decimal point.</p>
    *
    * @param value the {@code BigDecimal} to convert, may be null.
    * @return the scaled, with appropriate rounding, {@code BigDecimal}.
    * @since 3.8
    */
  def toScaledBigDecimal(value: BigDecimal): BigDecimal = toScaledBigDecimal(value, INTEGER_TWO, RoundingMode.HALF_EVEN)

  /**
    * Convert a {@code BigDecimal} to a {@code BigDecimal} whose scale is the
    * specified value with a {@code RoundingMode} applied. If the input {@code value}
    * is {@code null}, we simply return {@code BigDecimal.ZERO}.
    *
    * @param value        the {@code BigDecimal} to convert, may be null.
    * @param scale        the number of digits to the right of the decimal point.
    * @param roundingMode a rounding behavior for numerical operations capable of
    *                     discarding precision.
    * @return the scaled, with appropriate rounding, {@code BigDecimal}.
    * @since 3.8
    */
  def toScaledBigDecimal(value: BigDecimal, scale: Int, roundingMode: RoundingMode): BigDecimal = {
    if (value == null) return BigDecimal.ZERO
    value.setScale(scale,
      if (roundingMode == null) RoundingMode.HALF_EVEN
      else roundingMode
    )
  }

  /**
    * Convert a {@code Float} to a {@code BigDecimal} with a scale of
    * two that has been rounded using {@code RoundingMode.HALF_EVEN}. If the supplied
    * {@code value} is null, then {@code BigDecimal.ZERO} is returned.
    *
    * <p>Note, the scale of a {@code BigDecimal} is the number of digits to the right of the
    * decimal point.</p>
    *
    * @param value the {@code Float} to convert, may be null.
    * @return the scaled, with appropriate rounding, {@code BigDecimal}.
    * @since 3.8
    */
  def toScaledBigDecimal(value: Float): BigDecimal = toScaledBigDecimal(value, INTEGER_TWO, RoundingMode.HALF_EVEN)

  /**
    * Convert a {@code Float} to a {@code BigDecimal} whose scale is the
    * specified value with a {@code RoundingMode} applied. If the input {@code value}
    * is {@code null}, we simply return {@code BigDecimal.ZERO}.
    *
    * @param value        the {@code Float} to convert, may be null.
    * @param scale        the number of digits to the right of the decimal point.
    * @param roundingMode a rounding behavior for numerical operations capable of
    *                     discarding precision.
    * @return the scaled, with appropriate rounding, {@code BigDecimal}.
    * @since 3.8
    */
  def toScaledBigDecimal(value: Float, scale: Int, roundingMode: RoundingMode): BigDecimal = {
    if (value == null) return BigDecimal.ZERO
    toScaledBigDecimal(BigDecimal.valueOf(value.toDouble), scale, roundingMode)
  }

  /**
    * Convert a {@code Double} to a {@code BigDecimal} with a scale of
    * two that has been rounded using {@code RoundingMode.HALF_EVEN}. If the supplied
    * {@code value} is null, then {@code BigDecimal.ZERO} is returned.
    *
    * <p>Note, the scale of a {@code BigDecimal} is the number of digits to the right of the
    * decimal point.</p>
    *
    * @param value the {@code Double} to convert, may be null.
    * @return the scaled, with appropriate rounding, {@code BigDecimal}.
    * @since 3.8
    */
  def toScaledBigDecimal(value: Double): BigDecimal = toScaledBigDecimal(value, INTEGER_TWO, RoundingMode.HALF_EVEN)

  /**
    * Convert a {@code Double} to a {@code BigDecimal} whose scale is the
    * specified value with a {@code RoundingMode} applied. If the input {@code value}
    * is {@code null}, we simply return {@code BigDecimal.ZERO}.
    *
    * @param value        the {@code Double} to convert, may be null.
    * @param scale        the number of digits to the right of the decimal point.
    * @param roundingMode a rounding behavior for numerical operations capable of
    *                     discarding precision.
    * @return the scaled, with appropriate rounding, {@code BigDecimal}.
    * @since 3.8
    */
  def toScaledBigDecimal(value: Double, scale: Int, roundingMode: RoundingMode): BigDecimal = {
    if (value == null) return BigDecimal.ZERO
    toScaledBigDecimal(BigDecimal.valueOf(value), scale, roundingMode)
  }

  /**
    * Convert a {@code String} to a {@code BigDecimal} with a scale of
    * two that has been rounded using {@code RoundingMode.HALF_EVEN}. If the supplied
    * {@code value} is null, then {@code BigDecimal.ZERO} is returned.
    *
    * <p>Note, the scale of a {@code BigDecimal} is the number of digits to the right of the
    * decimal point.</p>
    *
    * @param value the {@code String} to convert, may be null.
    * @return the scaled, with appropriate rounding, {@code BigDecimal}.
    * @since 3.8
    */
  def toScaledBigDecimal(value: String): BigDecimal = toScaledBigDecimal(value, INTEGER_TWO, RoundingMode.HALF_EVEN)

  /**
    * Convert a {@code String} to a {@code BigDecimal} whose scale is the
    * specified value with a {@code RoundingMode} applied. If the input {@code value}
    * is {@code null}, we simply return {@code BigDecimal.ZERO}.
    *
    * @param value        the {@code String} to convert, may be null.
    * @param scale        the number of digits to the right of the decimal point.
    * @param roundingMode a rounding behavior for numerical operations capable of
    *                     discarding precision.
    * @return the scaled, with appropriate rounding, {@code BigDecimal}.
    * @since 3.8
    */
  def toScaledBigDecimal(value: String, scale: Int, roundingMode: RoundingMode): BigDecimal = {
    if (value == null) return BigDecimal.ZERO
    toScaledBigDecimal(createBigDecimal(value), scale, roundingMode)
  }

  /**
    * <p>Turns a string value into a java.lang.Number.</p>
    *
    * <p>If the string starts with {@code 0x} or {@code -0x} (lower or upper case) or {@code #} or {@code -#}, it
    * will be interpreted as a hexadecimal Integer - or Long, if the number of digits after the
    * prefix is more than 8 - or BigInteger if there are more than 16 digits.
    * </p>
    * <p>Then, the value is examined for a type qualifier on the end, i.e. one of
    * {@code 'f', 'F', 'd', 'D', 'l', 'L'}.  If it is found, it starts
    * trying to create successively larger types from the type specified
    * until one is found that can represent the value.</p>
    *
    * <p>If a type specifier is not found, it will check for a decimal point
    * and then try successively larger types from {@code Integer} to
    * {@code BigInteger} and from {@code Float} to
    * {@code BigDecimal}.</p>
    *
    * <p>
    * Integral values with a leading {@code 0} will be interpreted as octal; the returned number will
    * be Integer, Long or BigDecimal as appropriate.
    * </p>
    *
    * <p>Returns {@code null} if the string is {@code null}.</p>
    *
    * <p>This method does not trim the input string, i.e., strings with leading
    * or trailing spaces will generate NumberFormatExceptions.</p>
    *
    * @param str String containing a number, may be null
    * @return Number created from the string (or null if the input is null)
    * @throws NumberFormatException if the value cannot be converted
    */
  def createNumber(str: String): Number = {
    if (str == null) return null

    if (StringUtils.isBlank(str)) throw new NumberFormatException("A blank string is not a valid number")
    // Need to deal with all possible hex prefixes here
    val hex_prefixes = Array("0x", "0X", "-0x", "-0X", "#", "-#")
    val length = str.length
    var pfxLen = 0

    breakable {
      for (pfx: String <- hex_prefixes) {
        if (str.startsWith(pfx)) {
          pfxLen += pfx.length
          break()
        }
      }
    }

    if (pfxLen > 0) { // we have a hex number
      var firstSigDigit: Char = '0' // strip leading zeroes

      breakable {
        for (i: Int <- pfxLen until length) {
          firstSigDigit = str.charAt(i)
          if (firstSigDigit == '0') { // count leading zeroes
            pfxLen += 1
          } else break()
        }
      }

      val hexDigits = length - pfxLen
      if (hexDigits > 16 || hexDigits == 16 && firstSigDigit > '7') { // too many for Long
        return createBigInteger(str)
      }
      if (hexDigits > 8 || hexDigits == 8 && firstSigDigit > '7') { // too many for an int
        return createLong(str)
      }
      return createInteger(str)
    }

    val lastChar: Char = str.charAt(length - 1)
    var mant: String = null
    var dec: String = null
    var exp: String = null
    val decPos: Int = str.indexOf('.')
    val expPos: Int = str.indexOf('e') + str.indexOf('E') + 1 // assumes both not present

    // if both e and E are present, this is caught by the checks on expPos (which prevent IOOBE)
    // and the parsing which will detect if e or E appear in a number due to using the wrong offset
    if (decPos > -1) { // there is a decimal point
      if (expPos > -1) { // there is an exponent
        if (expPos < decPos || expPos > length) { // prevents double exponent causing IOOBE
          throw new NumberFormatException(str + " is not a valid number.")
        }
        dec = str.substring(decPos + 1, expPos)
      }
      else dec = str.substring(decPos + 1)
      mant = getMantissa(str, decPos)
    }
    else {
      if (expPos > -1) {
        if (expPos > length) throw new NumberFormatException(str + " is not a valid number.")
        mant = getMantissa(str, expPos)
      }
      else mant = getMantissa(str)
      dec = null
    }
    if (!Character.isDigit(lastChar) && lastChar != '.') {
      if (expPos > -1 && expPos < length - 1) exp = str.substring(expPos + 1, length - 1)
      else exp = null
      //Requesting a specific type..
      val numeric = str.substring(0, length - 1)
      val allZeros = isAllZeros(mant) && isAllZeros(exp)
      lastChar match {
        case 'l' =>
        case 'L' =>
          if (dec == null && exp == null && (!numeric.isEmpty && numeric.charAt(0) == '-' && isDigits(numeric.substring(1)) || isDigits(numeric))) {
            try return createLong(numeric)
            catch {
              case _: NumberFormatException =>

              // NOPMD
              // Too big for a long
            }
            return createBigInteger(numeric)
          }
          throw new NumberFormatException(str + " is not a valid number.")
        case 'f' =>
        case 'F' =>
          try {
            val f = createFloat(str)
            if (!(f.isInfinite || f.floatValue == 0.0F && !(allZeros))) { //If it's too big for a float or the float value = 0 and the string
              //has non-zeros in it, then float does not have the precision we want
              return f
            }
          } catch {
            case _: NumberFormatException =>

            // ignore the bad number
          }
        //$FALL-THROUGH$
        case 'd' =>
        case 'D' =>
          try {
            val d = createDouble(str)
            if (!(d.isInfinite || d.floatValue == 0.0D && !(allZeros))) return d
          } catch {
            case _: NumberFormatException =>

          }
          try return createBigDecimal(numeric)
          catch {
            case _: NumberFormatException =>

          }
        case _ =>
          throw new NumberFormatException(str + " is not a valid number.")
      }
    }
    //User doesn't have a preference on the return type, so let's start
    //small and go from there...
    if (expPos > -1 && expPos < length - 1) exp = str.substring(expPos + 1)
    else exp = null
    if (dec == null && exp == null) { // no decimal point and no exponent
      //Must be an Integer, Long, Biginteger
      try {
        return createInteger(str)
      } catch {
        case _: NumberFormatException =>

      }
      try {
        return createLong(str)
      } catch {
        case _: NumberFormatException =>

      }
      return createBigInteger(str)
    }
    //Must be a Float, Double, BigDecimal
    val allZeros = isAllZeros(mant) && isAllZeros(exp)
    try {
      val f = createFloat(str)
      val d = createDouble(str)
      if (!f.isInfinite && !(f.floatValue == 0.0F && !(allZeros)) && f.toString == d.toString) return f
      if (!d.isInfinite && !(d.doubleValue == 0.0D && !(allZeros))) {
        val b = createBigDecimal(str)
        if (b.compareTo(BigDecimal.valueOf(d.doubleValue)) == 0) return d
        return b
      }
    } catch {
      case _: NumberFormatException =>

    }
    createBigDecimal(str)
  }

  /**
    * <p>Utility method for {@link #createNumber ( java.lang.String )}.</p>
    *
    * <p>Returns mantissa of the given number.</p>
    *
    * @param str the string representation of the number
    * @return mantissa of the given number
    */
  private def getMantissa(str: String): String = getMantissa(str, str.length)

  /**
    * <p>Utility method for {@link #createNumber ( java.lang.String )}.</p>
    *
    * <p>Returns mantissa of the given number.</p>
    *
    * @param str     the string representation of the number
    * @param stopPos the position of the exponent or decimal point
    * @return mantissa of the given number
    */
  private def getMantissa(str: String, stopPos: Int): String = {
    val firstChar = str.charAt(0)
    val hasSign = firstChar == '-' || firstChar == '+'
    if (hasSign) str.substring(1, stopPos)
    else str.substring(0, stopPos)
  }

  /**
    * <p>Utility method for {@link #createNumber ( java.lang.String )}.</p>
    *
    * <p>Returns {@code true} if s is {@code null}.</p>
    *
    * @param str the String to check
    * @return if it is all zeros or {@code null}
    */
  private def isAllZeros(str: String): Boolean = {
    if (str == null) return true
    for (i <- str.length - 1 to 0 by -1) {
      if (str.charAt(i) != '0') return false
    }
    !str.isEmpty
  }

  /**
    * <p>Convert a {@code String} to a {@code Float}.</p>
    *
    * <p>Returns {@code null} if the string is {@code null}.</p>
    *
    * @param str a {@code String} to convert, may be null
    * @return converted {@code Float} (or null if the input is null)
    * @throws NumberFormatException if the value cannot be converted
    */
  def createFloat(str: String): Float = {
    if (str == null) return null.asInstanceOf[Float]
    str.toFloat
  }

  /**
    * <p>Convert a {@code String} to a {@code Double}.</p>
    *
    * <p>Returns {@code null} if the string is {@code null}.</p>
    *
    * @param str a {@code String} to convert, may be null
    * @return converted {@code Double} (or null if the input is null)
    * @throws NumberFormatException if the value cannot be converted
    */
  def createDouble(str: String): Double = {
    if (str == null) return null.asInstanceOf[Double]
    str.toDouble
  }

  /**
    * <p>Convert a {@code String} to a {@code Integer}, handling
    * hex (0xhhhh) and octal (0dddd) notations.
    * N.B. a leading zero means octal; spaces are not trimmed.</p>
    *
    * <p>Returns {@code null} if the string is {@code null}.</p>
    *
    * @param str a {@code String} to convert, may be null
    * @return converted {@code Integer} (or null if the input is null)
    * @throws NumberFormatException if the value cannot be converted
    */
  def createInteger(str: String): Integer = {
    if (str == null) return null
    // decode() handles 0xAABD and 0777 (hex and octal) as well.
    Integer.decode(str)
  }

  /**
    * <p>Convert a {@code String} to a {@code Long};
    * since 3.1 it handles hex (0Xhhhh) and octal (0ddd) notations.
    * N.B. a leading zero means octal; spaces are not trimmed.</p>
    *
    * <p>Returns {@code null} if the string is {@code null}.</p>
    *
    * @param str a {@code String} to convert, may be null
    * @return converted {@code Long} (or null if the input is null)
    * @throws NumberFormatException if the value cannot be converted
    */
  def createLong(str: String): Long = {
    if (str == null) return null.asInstanceOf[Long]
    JavaLong.decode(str)
  }

  /**
    * <p>Convert a {@code String} to a {@code BigInteger};
    * since 3.2 it handles hex (0x or #) and octal (0) notations.</p>
    *
    * <p>Returns {@code null} if the string is {@code null}.</p>
    *
    * @param str a {@code String} to convert, may be null
    * @return converted {@code BigInteger} (or null if the input is null)
    * @throws NumberFormatException if the value cannot be converted
    */
  def createBigInteger(str: String): BigInteger = {
    if (str == null) return null
    var pos = 0 // offset within string
    var radix = 10
    var negate = false // need to negate later?
    if (str.startsWith("-")) {
      negate = true
      pos = 1
    }
    if (str.startsWith("0x", pos) || str.startsWith("0X", pos)) { // hex
      radix = 16
      pos += 2
    } else {
      if (str.startsWith("#", pos)) { // alternative hex (allowed by Long/Integer)
        radix = 16
        pos += 1
      } else {
        if (str.startsWith("0", pos) && str.length > pos + 1) { // octal; so long as there are additional digits
          radix = 8
          pos += 1
          // default is to treat as decimal}}}
          val value = new BigInteger(str.substring(pos), radix)
          return if (negate) value.negate
          else value
        }
      }
    }
    ???
  }

  /**
    * <p>Convert a {@code String} to a {@code BigDecimal}.</p>
    *
    * <p>Returns {@code null} if the string is {@code null}.</p>
    *
    * @param str a {@code String} to convert, may be null
    * @return converted {@code BigDecimal} (or null if the input is null)
    * @throws NumberFormatException if the value cannot be converted
    */
  def createBigDecimal(str: String): BigDecimal = {
    if (str == null) return null
    // handle JDK1.3.1 bug where "" throws IndexOutOfBoundsException
    if (StringUtils.isBlank(str)) throw new NumberFormatException("A blank string is not a valid number")
    new BigDecimal(str)
  }

  /**
    * <p>Returns the minimum value in an array.</p>
    *
    * @param array an array, must not be null or empty
    * @return the minimum value in the array
    * @throws IllegalArgumentException if {@code array} is {@code null}
    * @throws IllegalArgumentException if {@code array} is empty
    * @since 3.4 Changed signature from min(long[]) to min(long...)
    */
  def min(array: Long*) = { // Validates input
    validateArray(array)
    // Finds and returns min
    var min = array(0)
    for (i <- 1 until array.length) {
      if (array(i) < min) min = array(i)
    }
    min
  }

  /**
    * <p>Returns the minimum value in an array.</p>
    *
    * @param array an array, must not be null or empty
    * @return the minimum value in the array
    * @throws IllegalArgumentException if {@code array} is {@code null}
    * @throws IllegalArgumentException if {@code array} is empty
    * @since 3.4 Changed signature from min(int[]) to min(int...)
    */
  def min(array: Int*) = {
    validateArray(array)
    var min = array(0)
    for (j <- 1 until array.length) {
      if (array(j) < min) min = array(j)
    }
    min
  }

  /**
    * <p>Returns the minimum value in an array.</p>
    *
    * @param array an array, must not be null or empty
    * @return the minimum value in the array
    * @throws IllegalArgumentException if {@code array} is {@code null}
    * @throws IllegalArgumentException if {@code array} is empty
    * @since 3.4 Changed signature from min(short[]) to min(short...)
    */
  def min(array: Short*) = {
    validateArray(array)
    var min = array(0)
    for (i <- 1 until array.length) {
      if (array(i) < min) min = array(i)
    }
    min
  }

  /**
    * <p>Returns the minimum value in an array.</p>
    *
    * @param array an array, must not be null or empty
    * @return the minimum value in the array
    * @throws IllegalArgumentException if {@code array} is {@code null}
    * @throws IllegalArgumentException if {@code array} is empty
    * @since 3.4 Changed signature from min(byte[]) to min(byte...)
    */
  def min(array: Byte*) = {
    validateArray(array)
    var min = array(0)
    for (i <- 1 until array.length) {
      if (array(i) < min) min = array(i)
    }
    min
  }

  /**
    * <p>Returns the minimum value in an array.</p>
    *
    * @param array an array, must not be null or empty
    * @return the minimum value in the array
    * @throws IllegalArgumentException if {@code array} is {@code null}
    * @throws IllegalArgumentException if {@code array} is empty
    * @see IEEE754rUtils#min(double[]) IEEE754rUtils for a version of this method that handles NaN differently
    * @since 3.4 Changed signature from min(double[]) to min(double...)
    */
  def min(array: Double*): Double = {
    validateArray(array)
    var min = array(0)
    for (i <- 1 until array.length) {
      if (Double.NaN == array(i)) return Double.NaN
      if (array(i) < min) min = array(i)
    }
    min
  }

  /**
    * <p>Returns the minimum value in an array.</p>
    *
    * @param array an array, must not be null or empty
    * @return the minimum value in the array
    * @throws IllegalArgumentException if {@code array} is {@code null}
    * @throws IllegalArgumentException if {@code array} is empty
    * @see IEEE754rUtils#min(float[]) IEEE754rUtils for a version of this method that handles NaN differently
    * @since 3.4 Changed signature from min(float[]) to min(float...)
    */
  def min(array: Float*): Float = {
    validateArray(array)
    var min = array(0)
    for (i <- 1 until array.length) {
      if (Float.NaN == array(i)) return Float.NaN
      if (array(i) < min) min = array(i)
    }
    min
  }

  /**
    * <p>Returns the maximum value in an array.</p>
    *
    * @param array an array, must not be null or empty
    * @return the maximum value in the array
    * @throws IllegalArgumentException if {@code array} is {@code null}
    * @throws IllegalArgumentException if {@code array} is empty
    * @since 3.4 Changed signature from max(long[]) to max(long...)
    */
  def max(array: Long*) = {
    validateArray(array)
    // Finds and returns max
    var max = array(0)
    for (j <- 1 until array.length) {
      if (array(j) > max) max = array(j)
    }
    max
  }

  /**
    * <p>Returns the maximum value in an array.</p>
    *
    * @param array an array, must not be null or empty
    * @return the maximum value in the array
    * @throws IllegalArgumentException if {@code array} is {@code null}
    * @throws IllegalArgumentException if {@code array} is empty
    * @since 3.4 Changed signature from max(int[]) to max(int...)
    */
  def max(array: Int*) = {
    validateArray(array)
    var max = array(0)
    for (j <- 1 until array.length) {
      if (array(j) > max) max = array(j)
    }
    max
  }

  /**
    * <p>Returns the maximum value in an array.</p>
    *
    * @param array an array, must not be null or empty
    * @return the maximum value in the array
    * @throws IllegalArgumentException if {@code array} is {@code null}
    * @throws IllegalArgumentException if {@code array} is empty
    * @since 3.4 Changed signature from max(short[]) to max(short...)
    */
  def max(array: Short*) = {
    validateArray(array)
    var max = array(0)
    for (i <- 1 until array.length) {
      if (array(i) > max) max = array(i)
    }
    max
  }

  /**
    * <p>Returns the maximum value in an array.</p>
    *
    * @param array an array, must not be null or empty
    * @return the maximum value in the array
    * @throws IllegalArgumentException if {@code array} is {@code null}
    * @throws IllegalArgumentException if {@code array} is empty
    * @since 3.4 Changed signature from max(byte[]) to max(byte...)
    */
  def max(array: Byte*) = {
    validateArray(array)
    var max = array(0)
    for (i <- 1 until array.length) {
      if (array(i) > max) max = array(i)
    }
    max
  }

  /**
    * <p>Returns the maximum value in an array.</p>
    *
    * @param array an array, must not be null or empty
    * @return the maximum value in the array
    * @throws IllegalArgumentException if {@code array} is {@code null}
    * @throws IllegalArgumentException if {@code array} is empty
    * @see IEEE754rUtils#max(double[]) IEEE754rUtils for a version of this method that handles NaN differently
    * @since 3.4 Changed signature from max(double[]) to max(double...)
    */
  def max(array: Double*): Double = {
    validateArray(array)
    var max = array(0)
    for (j <- 1 until array.length) {
      if (Double.NaN == array(j)) return Double.NaN
      if (array(j) > max) max = array(j)
    }
    max
  }

  /**
    * <p>Returns the maximum value in an array.</p>
    *
    * @param array an array, must not be null or empty
    * @return the maximum value in the array
    * @throws IllegalArgumentException if {@code array} is {@code null}
    * @throws IllegalArgumentException if {@code array} is empty
    * @see IEEE754rUtils#max(float[]) IEEE754rUtils for a version of this method that handles NaN differently
    * @since 3.4 Changed signature from max(float[]) to max(float...)
    */
  def max(array: Float*): Float = {
    validateArray(array)
    var max = array(0)
    for (j <- 1 until array.length) {
      if (Float.NaN ==  array(j)) return Float.NaN
      if (array(j) > max) max = array(j)
    }
    max
  }

  /**
    * Checks if the specified array is neither null nor empty.
    *
    * @param array the array to check
    * @throws IllegalArgumentException if {@code array} is either {@code null} or empty
    */
  private def validateArray(array: Any): Unit = {
    Validate.notNull(array, "The Array must not be null")
    Validate.isTrue(reflect.Array.getLength(array) != 0, "Array cannot be empty.")
  }

  /**
    * <p>Gets the minimum of three {@code long} values.</p>
    *
    * @param a value 1
    * @param b value 2
    * @param c value 3
    * @return the smallest of the values
    */
  def min(a: Long, b: Long, c: Long): Long = {
    var ret: Long = a
    if (b < a) ret = b
    if (c < a) ret = c
    ret
  }

  /**
    * <p>Gets the minimum of three {@code int} values.</p>
    *
    * @param a value 1
    * @param b value 2
    * @param c value 3
    * @return the smallest of the values
    */
  def min(a: Int, b: Int, c: Int): Int = {
    var ret: Int = a
    if (b < a) ret = b
    if (c < a) ret = c
    ret
  }

  /**
    * <p>Gets the minimum of three {@code short} values.</p>
    *
    * @param a value 1
    * @param b value 2
    * @param c value 3
    * @return the smallest of the values
    */
  def min(a: Short, b: Short, c: Short): Short = {
    var ret: Short = a
    if (b < a) ret = b
    if (c < a) ret = c
    ret
  }

  /**
    * <p>Gets the minimum of three {@code byte} values.</p>
    *
    * @param a value 1
    * @param b value 2
    * @param c value 3
    * @return the smallest of the values
    */
  def min(a: Byte, b: Byte, c: Byte): Byte = {
    var ret: Byte = a
    if (b < a) ret = b
    if (c < a) ret = c
    ret
  }

  /**
    * <p>Gets the minimum of three {@code double} values.</p>
    *
    * <p>If any value is {@code NaN}, {@code NaN} is
    * returned. Infinity is handled.</p>
    *
    * @param a value 1
    * @param b value 2
    * @param c value 3
    * @return the smallest of the values
    * @see IEEE754rUtils#min(double, double, double) for a version of this method that handles NaN differently
    */
  def min(a: Double, b: Double, c: Double): Double = Math.min(Math.min(a, b), c)

  /**
    * <p>Gets the minimum of three {@code float} values.</p>
    *
    * <p>If any value is {@code NaN}, {@code NaN} is
    * returned. Infinity is handled.</p>
    *
    * @param a value 1
    * @param b value 2
    * @param c value 3
    * @return the smallest of the values
    * @see IEEE754rUtils#min(float, float, float) for a version of this method that handles NaN differently
    */
  def min(a: Float, b: Float, c: Float): Float = Math.min(Math.min(a, b), c)

  /**
    * <p>Gets the maximum of three {@code long} values.</p>
    *
    * @param a value 1
    * @param b value 2
    * @param c value 3
    * @return the largest of the values
    */
  def max(a: Long, b: Long, c: Long): Long = {
    var ret: Long = a
    if (b > a) ret = b
    if (c > a) ret = c
    ret
  }

  /**
    * <p>Gets the maximum of three {@code int} values.</p>
    *
    * @param a value 1
    * @param b value 2
    * @param c value 3
    * @return the largest of the values
    */
  def max(a: Int, b: Int, c: Int): Int = {
    var ret: Int = a
    if (b > a) ret = b
    if (c > a) ret = c
    ret
  }

  /**
    * <p>Gets the maximum of three {@code short} values.</p>
    *
    * @param a value 1
    * @param b value 2
    * @param c value 3
    * @return the largest of the values
    */
  def max(a: Short, b: Short, c: Short): Short = {
    var ret: Short = a
    if (b > a) ret = b
    if (c > a) ret = c
    ret
  }

  /**
    * <p>Gets the maximum of three {@code byte} values.</p>
    *
    * @param a value 1
    * @param b value 2
    * @param c value 3
    * @return the largest of the values
    */
  def max(a: Byte, b: Byte, c: Byte): Byte = {
    var ret: Byte = a
    if (b > a) ret = b
    if (c > a) ret = c
    ret
  }

  /**
    * <p>Gets the maximum of three {@code double} values.</p>
    *
    * <p>If any value is {@code NaN}, {@code NaN} is
    * returned. Infinity is handled.</p>
    *
    * @param a value 1
    * @param b value 2
    * @param c value 3
    * @return the largest of the values
    * @see IEEE754rUtils#max(double, double, double) for a version of this method that handles NaN differently
    */
  def max(a: Double, b: Double, c: Double): Double = Math.max(Math.max(a, b), c)

  /**
    * <p>Gets the maximum of three {@code float} values.</p>
    *
    * <p>If any value is {@code NaN}, {@code NaN} is
    * returned. Infinity is handled.</p>
    *
    * @param a value 1
    * @param b value 2
    * @param c value 3
    * @return the largest of the values
    * @see IEEE754rUtils#max(float, float, float) for a version of this method that handles NaN differently
    */
  def max(a: Float, b: Float, c: Float): Float = Math.max(Math.max(a, b), c)

  /**
    * <p>Checks whether the {@code String} contains only
    * digit characters.</p>
    *
    * <p>{@code Null} and empty String will return
    * {@code false}.</p>
    *
    * @param str the {@code String} to check
    * @return {@code true} if str contains only Unicode numeric
    */
  def isDigits(str: String): Boolean = StringUtils.isNumeric(str)

  /**
    * <p>Checks whether the String a valid Java number.</p>
    *
    * <p>Valid numbers include hexadecimal marked with the {@code 0x} or
    * {@code 0X} qualifier, octal numbers, scientific notation and
    * numbers marked with a type qualifier (e.g. 123L).</p>
    *
    * <p>Non-hexadecimal strings beginning with a leading zero are
    * treated as octal values. Thus the string {@code 09} will return
    * {@code false}, since {@code 9} is not a valid octal value.
    * However, numbers beginning with {@code 0.} are treated as decimal.</p>
    *
    * <p>{@code null} and empty/blank {@code String} will return
    * {@code false}.</p>
    *
    * <p>Note, {@link #createNumber ( String )} should return a number for every
    * input resulting in {@code true}.</p>
    *
    * @param str the {@code String} to check
    * @return {@code true} if the string is a correctly formatted number
    * @since 3.3 the code supports hex {@code 0Xhhh} an
    *        octal {@code 0ddd} validation
    * @deprecated This feature will be removed in Lang 4.0,
    *             use {@link NumberUtils# isCreatable ( String )} instead
    */
  @deprecated def isNumber(str: String): Boolean = isCreatable(str)

  /**
    * <p>Checks whether the String a valid Java number.</p>
    *
    * <p>Valid numbers include hexadecimal marked with the {@code 0x} or
    * {@code 0X} qualifier, octal numbers, scientific notation and
    * numbers marked with a type qualifier (e.g. 123L).</p>
    *
    * <p>Non-hexadecimal strings beginning with a leading zero are
    * treated as octal values. Thus the string {@code 09} will return
    * {@code false}, since {@code 9} is not a valid octal value.
    * However, numbers beginning with {@code 0.} are treated as decimal.</p>
    *
    * <p>{@code null} and empty/blank {@code String} will return
    * {@code false}.</p>
    *
    * <p>Note, {@link #createNumber ( String )} should return a number for every
    * input resulting in {@code true}.</p>
    *
    * @param str the {@code String} to check
    * @return {@code true} if the string is a correctly formatted number
    * @since 3.5
    */
  def isCreatable(str: String): Boolean = {
    if (StringUtils.isEmpty(str)) return false
    val chars = str.toCharArray
    var sz = chars.length
    var hasExp = false
    var hasDecPoint = false
    var allowSigns = false
    var foundDigit = false
    // deal with any possible sign up front
    val start = if (chars(0) == '-' || chars(0) == '+') 1
    else 0
    if (sz > start + 1 && chars(start) == '0' && !StringUtils.contains(str, '.')) { // leading 0, skip if is a decimal number
      if (chars(start + 1) == 'x' || chars(start + 1) == 'X') { // leading 0x/0X
        var i = start + 2
        if (i == sz) return false // str == "0x"
        // checking hex (it can't be anything else)

        while ( {
          i < chars.length
        }) {
          if ((chars(i) < '0' || chars(i) > '9') && (chars(i) < 'a' || chars(i) > 'f') && (chars(i) < 'A' || chars(i) > 'F')) return false

          i += 1
        }
        return true
      }
      else if (Character.isDigit(chars(start + 1))) { // leading 0, but not hex, must be octal
        var i = start + 1

        while ( {
          i < chars.length
        }) {
          if (chars(i) < '0' || chars(i) > '7') return false

          i += 1
        }
        return true
      }
    }
    sz -= 1 // don't want to loop to the last char, check it afterwords

    // for type qualifiers
    var i = start
    // loop to the next to last char or to the last char if we need another digit to
    // make a valid number (e.g. chars[0..5] = "1234E")
    while ( {
      i < sz || i < sz + 1 && allowSigns && !foundDigit
    }) {
      if (chars(i) >= '0' && chars(i) <= '9') {
        foundDigit = true
        allowSigns = false
      }
      else if (chars(i) == '.') {
        if (hasDecPoint || hasExp) { // two decimal points or dec in exponent
          return false
        }
        hasDecPoint = true
      }
      else if (chars(i) == 'e' || chars(i) == 'E') { // we've already taken care of hex.
        if (hasExp) { // two E's
          return false
        }
        if (!foundDigit) return false
        hasExp = true
        allowSigns = true
      }
      else if (chars(i) == '+' || chars(i) == '-') {
        if (!allowSigns) return false
        allowSigns = false
        foundDigit = false // we need a digit after the E

      }
      else return false
      i += 1
    }
    if (i < chars.length) {
      if (chars(i) >= '0' && chars(i) <= '9') { // no type qualifier, OK
        return true
      }
      if (chars(i) == 'e' || chars(i) == 'E') { // can't have an E at the last byte
        return false
      }
      if (chars(i) == '.') {
        if (hasDecPoint || hasExp) return false
        // single trailing decimal point after non-exponent is ok
        return foundDigit
      }
      if (!allowSigns && (chars(i) == 'd' || chars(i) == 'D' || chars(i) == 'f' || chars(i) == 'F')) return foundDigit
      if (chars(i) == 'l' || chars(i) == 'L') { // not allowing L with an exponent or decimal point
        return foundDigit && !hasExp && !hasDecPoint
      }
      // last character is illegal
      return false
    }
    // allowSigns is true iff the val ends in 'E'
    // found digit it to make sure weird stuff like '.' and '1E-' doesn't pass
    !allowSigns && foundDigit
  }

  /**
    * <p>Checks whether the given String is a parsable number.</p>
    *
    * <p>Parsable numbers include those Strings understood by {@link Integer# parseInt ( String )},
    * {@link Long# parseLong ( String )}, {@link Float# parseFloat ( String )} or
    * {@link Double# parseDouble ( String )}. This method can be used instead of catching {@link java.text.ParseException}
    * when calling one of those methods.</p>
    *
    * <p>Hexadecimal and scientific notations are <strong>not</strong> considered parsable.
    * See {@link #isCreatable ( String )} on those cases.</p>
    *
    * <p>{@code Null} and empty String will return {@code false}.</p>
    *
    * @param str the String to check.
    * @return {@code true} if the string is a parsable number.
    * @since 3.4
    */
  def isParsable(str: String): Boolean = {
    if (StringUtils.isEmpty(str)) return false
    if (str.charAt(str.length - 1) == '.') return false
    if (str.charAt(0) == '-') {
      if (str.length == 1) return false
      return withDecimalsParsing(str, 1)
    }
    withDecimalsParsing(str, 0)
  }

  private def withDecimalsParsing(str: String, beginIdx: Int): Boolean = {
    var decimalPoints = 0
    for (i <- beginIdx until str.length) {
      val isDecimalPoint = str.charAt(i) == '.'
      if (isDecimalPoint) decimalPoints += 1
      if (decimalPoints > 1) return false
      if (!isDecimalPoint && !Character.isDigit(str.charAt(i))) return false
    }
    true
  }

  /**
    * <p>Compares two {@code int} values numerically. This is the same functionality as provided in Java 7.</p>
    *
    * @param x the first {@code int} to compare
    * @param y the second {@code int} to compare
    * @return the value {@code 0} if {@code x == y};
    *         a value less than {@code 0} if {@code x < y}; and
    *         a value greater than {@code 0} if {@code x > y}
    * @since 3.4
    */
  def compare(x: Int, y: Int): Int = {
    if (x == y) return 0
    if (x < y) -1
    else 1
  }

  /**
    * <p>Compares to {@code long} values numerically. This is the same functionality as provided in Java 7.</p>
    *
    * @param x the first {@code long} to compare
    * @param y the second {@code long} to compare
    * @return the value {@code 0} if {@code x == y};
    *         a value less than {@code 0} if {@code x < y}; and
    *         a value greater than {@code 0} if {@code x > y}
    * @since 3.4
    */
  def compare(x: Long, y: Long): Int = {
    if (x == y) return 0
    if (x < y) -1
    else 1
  }

  /**
    * <p>Compares to {@code short} values numerically. This is the same functionality as provided in Java 7.</p>
    *
    * @param x the first {@code short} to compare
    * @param y the second {@code short} to compare
    * @return the value {@code 0} if {@code x == y};
    *         a value less than {@code 0} if {@code x < y}; and
    *         a value greater than {@code 0} if {@code x > y}
    * @since 3.4
    */
  def compare(x: Short, y: Short): Int = {
    if (x == y) return 0
    if (x < y) -1
    else 1
  }

  /**
    * <p>Compares two {@code byte} values numerically. This is the same functionality as provided in Java 7.</p>
    *
    * @param x the first {@code byte} to compare
    * @param y the second {@code byte} to compare
    * @return the value {@code 0} if {@code x == y};
    *         a value less than {@code 0} if {@code x < y}; and
    *         a value greater than {@code 0} if {@code x > y}
    * @since 3.4
    */
  def compare(x: Byte, y: Byte): Int = x - y
}
