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

import java.lang.{Float => JavaFloat}

/**
  * A mutable {@code float} wrapper.
  * <p>
  * Note that as MutableJavaFloat does not extend JavaFloat, it is not treated by String.format as a JavaFloat parameter.
  *
  * @see JavaFloat
  * @since 2.1
  */
@SerialVersionUID(5787169186L)
class MutableJavaFloat()

/**
  * Constructs a new MutableJavaFloat with the default value of zero.
  */
  extends Number with Comparable[MutableJavaFloat] with Mutable[Number] {
  /** The mutable value. */
  private var value: JavaFloat = 0.0f

  /**
    * Constructs a new MutableJavaFloat with the specified value.
    *
    * @param value the initial value to store
    */
  def this(value: JavaFloat) = {
    this()
    this.value = value
  }

  /**
    * Constructs a new MutableJavaFloat with the specified value.
    *
    * @param value the initial value to store, not null
    * @throws NullPointerException if the object is null
    */
  def this(value: Number) = {
    this()
    this.value = value.floatValue
  }

  /**
    * Constructs a new MutableJavaFloat parsing the given string.
    *
    * @param value the string to parse, not null
    * @throws NumberFormatException if the string cannot be parsed into a float
    * @since 2.5
    */
  def this(value: String) = {
    this()
    this.value = JavaFloat.parseFloat(value)
  }

  /**
    * Gets the value as a JavaFloat instance.
    *
    * @return the value as a JavaFloat, never null
    */
  override def getValue: JavaFloat = JavaFloat.valueOf(this.value)

  /**
    * Sets the value.
    *
    * @param value the value to set
    */
  def setValue(value: JavaFloat): Unit = {
    this.value = value
  }

  /**
    * Sets the value from any Number instance.
    *
    * @param value the value to set, not null
    * @throws NullPointerException if the object is null
    */
  override def setValue(value: Number): Unit = {
    this.value = value.floatValue
  }

  /**
    * Checks whether the float value is the special NaN value.
    *
    * @return true if NaN
    */
  def isNaN: Boolean = JavaFloat.isNaN(value)

  /**
    * Checks whether the float value is infinite.
    *
    * @return true if infinite
    */
  def isInfinite: Boolean = JavaFloat.isInfinite(value)

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
  def getAndIncrement: JavaFloat = {
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
  def incrementAndGet: JavaFloat = {
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
  def getAndDecrement: JavaFloat = {
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
  def decrementAndGet: JavaFloat = {
    value -= 1
    value
  }

  /**
    * Adds a value to the value of this instance.
    *
    * @param operand the value to add, not null
    * @since 2.2
    */
  def add(operand: JavaFloat): Unit = {
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
    this.value += operand.floatValue
  }

  /**
    * Subtracts a value from the value of this instance.
    *
    * @param operand the value to subtract
    * @since 2.2
    */
  def subtract(operand: JavaFloat): Unit = {
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
    this.value -= operand.floatValue
  }

  /**
    * Increments this instance's value by {@code operand}; this method returns the value associated with the instance
    * immediately after the addition operation. This method is not thread safe.
    *
    * @param operand the quantity to add, not null
    * @return the value associated with this instance after adding the operand
    * @since 3.5
    */
  def addAndGet(operand: JavaFloat): JavaFloat = {
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
  def addAndGet(operand: Number): JavaFloat = {
    this.value += operand.floatValue
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
  def getAndAdd(operand: JavaFloat): JavaFloat = {
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
  def getAndAdd(operand: Number): JavaFloat = {
    val last = value
    this.value += operand.floatValue
    last
  }

  /**
    * Returns the value of this MutableJavaFloat as an int.
    *
    * @return the numeric value represented by this object after conversion to type int.
    */
  override def intValue: Int = value.toInt

  /**
    * Returns the value of this MutableJavaFloat as a long.
    *
    * @return the numeric value represented by this object after conversion to type long.
    */
  override def longValue: Long = value.toLong

  /**
    * Returns the value of this MutableJavaFloat as a float.
    *
    * @return the numeric value represented by this object after conversion to type float.
    */
  override def floatValue: Float = value

  /**
    * Returns the value of this MutableJavaFloat as a double.
    *
    * @return the numeric value represented by this object after conversion to type double.
    */
  override def doubleValue: Double = value.toDouble

  /**
    * Gets this mutable as an instance of JavaFloat.
    *
    * @return a JavaFloat instance containing the value from this mutable, never null
    */
  def toJavaFloat: JavaFloat = JavaFloat.valueOf(floatValue)

  /**
    * Compares this object against some other object. The result is {@code true} if and only if the argument is
    * not {@code null} and is a {@code JavaFloat} object that represents a {@code float} that has the
    * identical bit pattern to the bit pattern of the {@code float} represented by this object. For this
    * purpose, two float values are considered to be the same if and only if the method
    * {@link JavaFloat# floatToIntBits ( float )}returns the same int value when applied to each.
    * <p>
    * Note that in most cases, for two instances of class {@code JavaFloat},{@code f1} and {@code f2},
    * the value of {@code f1.equals(f2)} is {@code true} if and only if <blockquote>
    *
    * <pre>
    * f1.floatValue() == f2.floatValue()
    * </pre>
    *
    * </blockquote>
    * <p>
    * also has the value {@code true}. However, there are two exceptions:
    * <ul>
    * <li>If {@code f1} and {@code f2} both represent {@code JavaFloat.NaN}, then the
    * {@code equals} method returns {@code true}, even though {@code JavaFloat.NaN==JavaFloat.NaN} has
    * the value {@code false}.
    * <li>If {@code f1} represents {@code +0.0f} while {@code f2} represents {@code -0.0f},
    * or vice versa, the {@code equal} test has the value {@code false}, even though
    * {@code 0.0f==-0.0f} has the value {@code true}.
    * </ul>
    * This definition allows hashtables to operate properly.
    *
    * @param obj the object to compare with, null returns false
    * @return {@code true} if the objects are the same; {@code false} otherwise.
    * @see java.lang.JavaFloat#floatToIntBits(float)
    */
  override def equals(obj: Any): Boolean =
    obj.isInstanceOf[MutableJavaFloat] &&
      JavaFloat.floatToIntBits(obj.asInstanceOf[MutableJavaFloat].value) == JavaFloat.floatToIntBits(value)

  /**
    * Returns a suitable hash code for this mutable.
    *
    * @return a suitable hash code
    */
  override def hashCode: Int = JavaFloat.floatToIntBits(value)

  /**
    * Compares this mutable to another in ascending order.
    *
    * @param other the other mutable to compare to, not null
    * @return negative if this is less, zero if equal, positive if greater
    */
  override def compareTo(other: MutableJavaFloat): Int = JavaFloat.compare(this.value, other.value)

  /**
    * Returns the String value of this mutable.
    *
    * @return the mutable value as a string
    */
  override def toString: String = String.valueOf(value)
}
