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

import org.apache.commons.lang3.math.NumberUtils
import java.lang.{Long => JavaLong}

/**
  * A mutable {@code long} wrapper.
  * <p>
  * Note that as MutableJavaLong does not extend JavaLong, it is not treated by String.format as a JavaLong parameter.
  *
  * @see Long
  * @since 2.1
  */
@SerialVersionUID(62986528375L)
class MutableJavaLong()

/**
  * Constructs a new MutableJavaLong with the default value of zero.
  */
  extends Number with Comparable[MutableJavaLong] with Mutable[Number] {
  /** The mutable value. */
  private var value: Long = 0L

  /**
    * Constructs a new MutableJavaLong with the specified value.
    *
    * @param value the initial value to store
    */
  def this(value: Long) = {
    this()
    this.value = value
  }

  /**
    * Constructs a new MutableJavaLong with the specified value.
    *
    * @param value the initial value to store, not null
    * @throws NullPointerException if the object is null
    */
  def this(value: Number) = {
    this()
    this.value = value.longValue
  }

  /**
    * Constructs a new MutableJavaLong parsing the given string.
    *
    * @param value the string to parse, not null
    * @throws NumberFormatException if the string cannot be parsed into a long
    * @since 2.5
    */
  def this(value: String) = {
    this()
    this.value = JavaLong.parseLong(value)
  }

  /**
    * Gets the value as a JavaLong instance.
    *
    * @return the value as a JavaLong, never null
    */
  override def getValue: JavaLong = JavaLong.valueOf(this.value)

  /**
    * Sets the value.
    *
    * @param value the value to set
    */
  def setValue(value: JavaLong): Unit = {
    this.value = value
  }

  /**
    * Sets the value from any Number instance.
    *
    * @param value the value to set, not null
    * @throws NullPointerException if the object is null
    */
  override def setValue(value: Number): Unit = {
    this.value = value.longValue
  }

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
  def getAndIncrement: Long = {
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
  def incrementAndGet: Long = {
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
  def getAndDecrement: Long = {
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
  def decrementAndGet: Long = {
    value -= 1
    value
  }

  /**
    * Adds a value to the value of this instance.
    *
    * @param operand the value to add, not null
    * @since 2.2
    */
  def add(operand: Long): Unit = {
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
    this.value += operand.longValue
  }

  /**
    * Subtracts a value from the value of this instance.
    *
    * @param operand the value to subtract, not null
    * @since 2.2
    */
  def subtract(operand: Long): Unit = {
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
    this.value -= operand.longValue
  }

  /**
    * Increments this instance's value by {@code operand}; this method returns the value associated with the instance
    * immediately after the addition operation. This method is not thread safe.
    *
    * @param operand the quantity to add, not null
    * @return the value associated with this instance after adding the operand
    * @since 3.5
    */
  def addAndGet(operand: Long): Long = {
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
  def addAndGet(operand: Number): Long = {
    this.value += operand.longValue
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
  def getAndAdd(operand: Long): Long = {
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
  def getAndAdd(operand: Number): Long = {
    val last = value
    this.value += operand.longValue
    last
  }

  /**
    * Returns the value of this MutableJavaLong as an int.
    *
    * @return the numeric value represented by this object after conversion to type int.
    */
  override def intValue: Int = value.toInt

  /**
    * Returns the value of this MutableJavaLong as a long.
    *
    * @return the numeric value represented by this object after conversion to type long.
    */
  override def longValue: Long = value.toLong

  /**
    * Returns the value of this MutableJavaLong as a float.
    *
    * @return the numeric value represented by this object after conversion to type float.
    */
  override def floatValue: Float = value.toFloat


  /**
    * Returns the value of this MutableJavaLong as a double.
    *
    * @return the numeric value represented by this object after conversion to type double.
    */
  override def doubleValue: Double = value.toDouble

  /**
    * Gets this mutable as an instance of JavaLong.
    *
    * @return a JavaLong instance containing the value from this mutable, never null
    */
  def toLong: JavaLong = JavaLong.valueOf(longValue)

  /**
    * Compares this object to the specified object. The result is {@code true} if and only if the argument
    * is not {@code null} and is a {@code MutableJavaLong} object that contains the same {@code long}
    * value as this object.
    *
    * @param obj the object to compare with, null returns false
    * @return {@code true} if the objects are the same; {@code false} otherwise.
    */
  override def equals(obj: Any): Boolean = {
    if (obj.isInstanceOf[MutableJavaLong]) return value == obj.asInstanceOf[MutableJavaLong].longValue
    false
  }

  /**
    * Returns a suitable hash code for this mutable.
    *
    * @return a suitable hash code
    */
  override def hashCode: Int = (value ^ (value >>> 32)).toInt

  /**
    * Compares this mutable to another in ascending order.
    *
    * @param other the other mutable to compare to, not null
    * @return negative if this is less, zero if equal, positive if greater
    */
  override def compareTo(other: MutableJavaLong): Int = NumberUtils.compare(this.value, other.value)

  /**
    * Returns the String value of this mutable.
    *
    * @return the mutable value as a string
    */
  override def toString: String = String.valueOf(value)
}