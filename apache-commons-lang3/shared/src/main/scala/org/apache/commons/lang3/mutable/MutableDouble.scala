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

package org.apache.commons.lang3.mutable

import java.lang.{Double => JavaDouble}

/**
  * A mutable {@code double} wrapper.
  * <p>
  * Note that as MutableJavaDouble does not extend JavaDouble, it is not treated by String.format as a JavaDouble parameter.
  *
  * @see JavaDouble
  * @since 2.1
  */
@SerialVersionUID(1587163916L)
class MutableJavaDouble()

/**
  * Constructs a new MutableJavaDouble with the default value of zero.
  */
  extends Number with Comparable[MutableJavaDouble] with Mutable[Number] {
  /** The mutable value. */
  private var value: Double = 0.0d

  /**
    * Constructs a new MutableJavaDouble with the specified value.
    *
    * @param value the initial value to store
    */
  def this(value: Double) = {
    this()
    this.value = value
  }

  /**
    * Constructs a new MutableJavaDouble with the specified value.
    *
    * @param value the initial value to store, not null
    * @throws NullPointerException if the object is null
    */
  def this(value: Number) = {
    this()
    this.value = value.doubleValue
  }

  /**
    * Constructs a new MutableJavaDouble parsing the given string.
    *
    * @param value the string to parse, not null
    * @throws NumberFormatException if the string cannot be parsed into a double
    * @since 2.5
    */
  def this(value: String) = {
    this()
    this.value = JavaDouble.parseDouble(value)
  }

  /**
    * Gets the value as a JavaDouble instance.
    *
    * @return the value as a JavaDouble, never null
    */
  override def getValue: JavaDouble = JavaDouble.valueOf(this.value)

  /**
    * Sets the value.
    *
    * @param value the value to set
    */
  def setValue(value: Double): Unit = {
    this.value = value
  }

  /**
    * Sets the value from any Number instance.
    *
    * @param value the value to set, not null
    * @throws NullPointerException if the object is null
    */
  override def setValue(value: Number): Unit = {
    this.value = value.doubleValue
  }

  /**
    * Checks whether the double value is the special NaN value.
    *
    * @return true if NaN
    */
  def isNaN: Boolean = JavaDouble.isNaN(value)

  /**
    * Checks whether the double value is infinite.
    *
    * @return true if infinite
    */
  def isInfinite: Boolean = JavaDouble.isInfinite(value)

  /**
    * Increments the value.
    *
    * @since 2.2
    */
  def increment(): Unit = {
    value += 1
  }

  /**
    * Increments this instance's value by 1; this method returns the value associated with the instance
    * immediately prior to the increment operation. This method is not thread safe.
    *
    * @return the value associated with the instance before it was incremented
    * @since 3.5
    */
  def getAndIncrement: Double = {
    val last = value
    value += 1
    last
  }

  /**
    * Increments this instance's value by 1; this method returns the value associated with the instance
    * immediately after the increment operation. This method is not thread safe.
    *
    * @return the value associated with the instance after it is incremented
    * @since 3.5
    */
  def incrementAndGet: Double = {
    value += 1
    value
  }

  /**
    * Decrements the value.
    *
    * @since 2.2
    */
  def decrement(): Unit = {
    value -= 1
  }

  /**
    * Decrements this instance's value by 1; this method returns the value associated with the instance
    * immediately prior to the decrement operation. This method is not thread safe.
    *
    * @return the value associated with the instance before it was decremented
    * @since 3.5
    */
  def getAndDecrement: Double = {
    val last = value
    value -= 1
    last
  }

  /**
    * Decrements this instance's value by 1; this method returns the value associated with the instance
    * immediately after the decrement operation. This method is not thread safe.
    *
    * @return the value associated with the instance after it is decremented
    * @since 3.5
    */
  def decrementAndGet: Double = {
    value -= 1
    value
  }

  /**
    * Adds a value to the value of this instance.
    *
    * @param operand the value to add
    * @since 2.2
    */
  def add(operand: Double): Unit = {
    this.value += operand
  }

  /**
    * Adds a value to the value of this instance.
    *
    * @param operand the value to add, not null
    * @throws NullPointerException if the object is null
    * @since 2.2
    */
  def add(operand: Number): Unit = {
    this.value += operand.doubleValue
  }

  /**
    * Subtracts a value from the value of this instance.
    *
    * @param operand the value to subtract, not null
    * @since 2.2
    */
  def subtract(operand: Double): Unit = {
    this.value -= operand
  }

  /**
    * Subtracts a value from the value of this instance.
    *
    * @param operand the value to subtract, not null
    * @throws NullPointerException if the object is null
    * @since 2.2
    */
  def subtract(operand: Number): Unit = {
    this.value -= operand.doubleValue
  }

  /**
    * Increments this instance's value by {@code operand}; this method returns the value associated with the instance
    * immediately after the addition operation. This method is not thread safe.
    *
    * @param operand the quantity to add, not null
    * @return the value associated with this instance after adding the operand
    * @since 3.5
    */
  def addAndGet(operand: Double): Double = {
    this.value += operand
    value
  }

  /**
    * Increments this instance's value by {@code operand}; this method returns the value associated with the instance
    * immediately after the addition operation. This method is not thread safe.
    *
    * @param operand the quantity to add, not null
    * @throws NullPointerException if {@code operand} is null
    * @return the value associated with this instance after adding the operand
    * @since 3.5
    */
  def addAndGet(operand: Number): Double = {
    this.value += operand.doubleValue
    value
  }

  /**
    * Increments this instance's value by {@code operand}; this method returns the value associated with the instance
    * immediately prior to the addition operation. This method is not thread safe.
    *
    * @param operand the quantity to add, not null
    * @return the value associated with this instance immediately before the operand was added
    * @since 3.5
    */
  def getAndAdd(operand: Double): Double = {
    val last = value
    this.value += operand
    last
  }

  /**
    * Increments this instance's value by {@code operand}; this method returns the value associated with the instance
    * immediately prior to the addition operation. This method is not thread safe.
    *
    * @param operand the quantity to add, not null
    * @throws NullPointerException if {@code operand} is null
    * @return the value associated with this instance immediately before the operand was added
    * @since 3.5
    */
  def getAndAdd(operand: Number): Double = {
    val last = value
    this.value += operand.doubleValue
    last
  }

  /**
    * Returns the value of this MutableJavaDouble as an int.
    *
    * @return the numeric value represented by this object after conversion to type int.
    */
  override def intValue: Int = value.toInt

  /**
    * Returns the value of this MutableJavaDouble as a long.
    *
    * @return the numeric value represented by this object after conversion to type long.
    */
  override def longValue: Long = value.toLong

  /**
    * Returns the value of this MutableJavaDouble as a float.
    *
    * @return the numeric value represented by this object after conversion to type float.
    */
  override def floatValue: Float = value.toFloat

  /**
    * Returns the value of this MutableJavaDouble as a double.
    *
    * @return the numeric value represented by this object after conversion to type double.
    */
  override def doubleValue: Double = value

  /**
    * Gets this mutable as an instance of JavaDouble.
    *
    * @return a JavaDouble instance containing the value from this mutable, never null
    */
  def toJavaDouble: JavaDouble = JavaDouble.valueOf(doubleValue)

  /**
    * Compares this object against the specified object. The result is {@code true} if and only if the argument
    * is not {@code null} and is a {@code JavaDouble} object that represents a double that has the identical
    * bit pattern to the bit pattern of the double represented by this object. For this purpose, two
    * {@code double} values are considered to be the same if and only if the method
    * {@link JavaDouble# doubleToLongBits ( double )}returns the same long value when applied to each.
    * <p>
    * Note that in most cases, for two instances of class {@code JavaDouble},{@code d1} and {@code d2},
    * the value of {@code d1.equals(d2)} is {@code true} if and only if <blockquote>
    *
    * <pre>
    * d1.doubleValue()&nbsp;== d2.doubleValue()
    * </pre>
    *
    * </blockquote>
    * <p>
    * also has the value {@code true}. However, there are two exceptions:
    * <ul>
    * <li>If {@code d1} and {@code d2} both represent {@code JavaDouble.NaN}, then the
    * {@code equals} method returns {@code true}, even though {@code JavaDouble.NaN==JavaDouble.NaN} has
    * the value {@code false}.
    * <li>If {@code d1} represents {@code +0.0} while {@code d2} represents {@code -0.0},
    * or vice versa, the {@code equal} test has the value {@code false}, even though
    * {@code +0.0==-0.0} has the value {@code true}. This allows hashtables to operate properly.
    * </ul>
    *
    * @param obj the object to compare with, null returns false
    * @return {@code true} if the objects are the same; {@code false} otherwise.
    */
  override def equals(obj: Any): Boolean = obj.isInstanceOf[MutableJavaDouble] &&
    JavaDouble.doubleToLongBits(obj.asInstanceOf[MutableJavaDouble].value) == JavaDouble.doubleToLongBits(value)

  /**
    * Returns a suitable hash code for this mutable.
    *
    * @return a suitable hash code
    */
  override def hashCode: Int = {
    val bits = JavaDouble.doubleToLongBits(value)
    (bits ^ bits >>> 32).toInt
  }

  /**
    * Compares this mutable to another in ascending order.
    *
    * @param other the other mutable to compare to, not null
    * @return negative if this is less, zero if equal, positive if greater
    */
  override def compareTo(other: MutableJavaDouble): Int = JavaDouble.compare(this.value, other.value)

  /**
    * Returns the String value of this mutable.
    *
    * @return the mutable value as a string
    */
  override def toString: String = String.valueOf(value)
}