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
import java.lang.{Short => JavaShort}

object MutableShort {
  private val ONE: Short = 1.toShort
  private val ZERO: Short = 0.toShort
}
/**
  * A mutable {@code short} wrapper.
  * <p>
  * Note that as MutableShort does not extend Short, it is not treated by String.format as a Short parameter.
  *
  * @see Short
  * @since 2.1
  */
@SerialVersionUID(-2135791679L)
class MutableShort()

/**
  * Constructs a new MutableShort with the default value of zero.
  */
  extends Number with Comparable[MutableShort] with Mutable[Number] {
  import MutableShort.{ONE, ZERO}

  /** The mutable value. */
  private var value: Short = ZERO

  /**
    * Constructs a new MutableShort with the specified value.
    *
    * @param value the initial value to store
    */
  def this(value: Short) = {
    this()
    this.value = value
  }

  /**
    * Constructs a new MutableShort with the specified value.
    *
    * @param value the initial value to store, not null
    * @throws NullPointerException if the object is null
    */
  def this(value: Number) = {
    this()
    this.value = value.shortValue
  }

  /**
    * Constructs a new MutableShort parsing the given string.
    *
    * @param value the string to parse, not null
    * @throws NumberFormatException if the string cannot be parsed into a short
    * @since 2.5
    */
  def this(value: String) = {
    this()
    this.value = JavaShort.parseShort(value)
  }

  /**
    * Gets the value as a Short instance.
    *
    * @return the value as a Short, never null
    */
  override def getValue: JavaShort = JavaShort.valueOf(this.value)

  /**
    * Sets the value.
    *
    * @param value the value to set
    */
  def setValue(value: Short): Unit = {
    this.value = value
  }

  /**
    * Sets the value from any Number instance.
    *
    * @param value the value to set, not null
    * @throws NullPointerException if the object is null
    */
  override def setValue(value: Number): Unit = {
    this.value = value.shortValue
  }

  /**
    * Increments the value.
    *
    * @since 2.2
    */
  def increment(): Unit = {
    this.value = (this.value + ONE).toShort
  }

  /**
    * Increments this instance's value by 1; this method returns the value associated with the instance
    * immediately prior to the increment operation. This method is not thread safe.
    *
    * @return the value associated with the instance before it was incremented
    * @since 3.5
    */
  def getAndIncrement: Short = {
    val last = this.value
    this.value = (this.value + ONE).toShort
    last
  }

  /**
    * Increments this instance's value by 1; this method returns the value associated with the instance
    * immediately after the increment operation. This method is not thread safe.
    *
    * @return the value associated with the instance after it is incremented
    * @since 3.5
    */
  def incrementAndGet: Short = {
    this.value = (this.value + ONE).toShort
    this.value
  }

  /**
    * Decrements the value.
    *
    * @since 2.2
    */
  def decrement(): Unit = {
    this.value = (this.value - ONE).toShort
  }

  /**
    * Decrements this instance's value by 1; this method returns the value associated with the instance
    * immediately prior to the decrement operation. This method is not thread safe.
    *
    * @return the value associated with the instance before it was decremented
    * @since 3.5
    */
  def getAndDecrement: Short = {
    val last = this.value
    this.value = (this.value - ONE).toShort
    last
  }

  /**
    * Decrements this instance's value by 1; this method returns the value associated with the instance
    * immediately after the decrement operation. This method is not thread safe.
    *
    * @return the value associated with the instance after it is decremented
    * @since 3.5
    */
  def decrementAndGet: Short = {
    this.value = (this.value - ONE).toShort
    this.value
  }

  /**
    * Adds a value to the value of this instance.
    *
    * @param operand the value to add, not null
    * @since 2.2
    */
  def add(operand: Short): Unit = {
    this.value = (this.value + operand).toShort
  }

  /**
    * Adds a value to the value of this instance.
    *
    * @param operand the value to add, not null
    * @throws NullPointerException if the object is null
    * @since 2.2
    */
  def add(operand: Number): Unit = {
    this.value = (this.value + operand.shortValue).toShort
  }

  /**
    * Subtracts a value from the value of this instance.
    *
    * @param operand the value to subtract, not null
    * @since 2.2
    */
  def subtract(operand: Short): Unit = {
    this.value = (this.value - operand).toShort
  }

  /**
    * Subtracts a value from the value of this instance.
    *
    * @param operand the value to subtract, not null
    * @throws NullPointerException if the object is null
    * @since 2.2
    */
  def subtract(operand: Number): Unit = {
    this.value = (this.value - operand.shortValue).toShort
  }

  /**
    * Increments this instance's value by {@code operand}; this method returns the value associated with the instance
    * immediately after the addition operation. This method is not thread safe.
    *
    * @param operand the quantity to add, not null
    * @return the value associated with this instance after adding the operand
    * @since 3.5
    */
  def addAndGet(operand: Short): Short = {
    this.value = (this.value + operand).toShort
    this.value
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
  def addAndGet(operand: Number): Short = {
    this.value = (this.value + operand.shortValue).toShort
    this.value
  }

  /**
    * Increments this instance's value by {@code operand}; this method returns the value associated with the instance
    * immediately prior to the addition operation. This method is not thread safe.
    *
    * @param operand the quantity to add, not null
    * @return the value associated with this instance immediately before the operand was added
    * @since 3.5
    */
  def getAndAdd(operand: Short): Short = {
    val last = this.value
    this.value = (this.value + operand).toShort
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
  def getAndAdd(operand: Number): Short = {
    val last = this.value
    this.value = (this.value + operand.shortValue).toShort
    last
  }

  /**
    * Returns the value of this MutableShort as a short.
    *
    * @return the numeric value represented by this object after conversion to type short.
    */
  override def shortValue: Short = value

  /**
    * Returns the value of this MutableShort as an int.
    *
    * @return the numeric value represented by this object after conversion to type int.
    */
  override def intValue: Int = value.toInt

  /**
    * Returns the value of this MutableShort as a long.
    *
    * @return the numeric value represented by this object after conversion to type long.
    */
  override def longValue: Long = value.toLong

  /**
    * Returns the value of this MutableShort as a float.
    *
    * @return the numeric value represented by this object after conversion to type float.
    */
  override def floatValue: Float = value.toFloat

  /**
    * Returns the value of this MutableShort as a double.
    *
    * @return the numeric value represented by this object after conversion to type double.
    */
  override def doubleValue: Double = value.toDouble

  /**
    * Gets this mutable as an instance of Short.
    *
    * @return a Short instance containing the value from this mutable, never null
    */
  def toJavaShort: JavaShort = JavaShort.valueOf(shortValue)

  /**
    * Compares this object to the specified object. The result is {@code true} if and only if the argument
    * is not {@code null} and is a {@code MutableShort} object that contains the same {@code short}
    * value as this object.
    *
    * @param obj the object to compare with, null returns false
    * @return {@code true} if the objects are the same; {@code false} otherwise.
    */
  override def equals(obj: Any): Boolean = {
    if (obj.isInstanceOf[MutableShort]) value == obj.asInstanceOf[MutableShort].shortValue
    else false
  }

  /**
    * Returns a suitable hash code for this mutable.
    *
    * @return a suitable hash code
    */
  override def hashCode: Int = value.toInt

  /**
    * Compares this mutable to another in ascending order.
    *
    * @param other the other mutable to compare to, not null
    * @return negative if this is less, zero if equal, positive if greater
    */
  override def compareTo(other: MutableShort): Int = NumberUtils.compare(this.value, other.value)

  /**
    * Returns the String value of this mutable.
    *
    * @return the mutable value as a string
    */
  override def toString: String = String.valueOf(value.toInt)
}