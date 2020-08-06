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

import java.io.Serializable
import org.apache.commons.lang3.BooleanUtils
import java.lang.{Boolean => JavaBoolean}

/**
  * A mutable {@code boolean} wrapper.
  * <p>
  * Note that as MutableJavaBoolean does not extend JavaBoolean, it is not treated by String.format as a JavaBoolean parameter.
  *
  * @see JavaBoolean
  * @since 2.2
  */
@SerialVersionUID(-4830728138360036487L)
class MutableBoolean()

/**
  * Constructs a new MutableJavaBoolean with the default value of false.
  */
  extends Mutable[JavaBoolean] with Serializable with Comparable[MutableBoolean] {
  /** The mutable value. */
  private var value: Boolean = false

  /**
    * Constructs a new MutableJavaBoolean with the specified value.
    *
    * @param value the initial value to store
    */
  def this(value: Boolean) = {
    this()
    this.value = value
  }

  /**
    * Constructs a new MutableJavaBoolean with the specified value.
    *
    * @param value the initial value to store, not null
    * @throws NullPointerException if the object is null
    */
  def this(value: JavaBoolean) = {
    this()
    this.value = value.booleanValue
  }

  /**
    * Gets the value as a JavaBoolean instance.
    *
    * @return the value as a JavaBoolean, never null
    */
  override def getValue: JavaBoolean = JavaBoolean.valueOf(this.value)

  /**
    * Sets the value.
    *
    * @param value the value to set
    */
  def setValue(value: Boolean): Unit = {
    this.value = value
  }

  /**
    * Sets the value to false.
    *
    * @since 3.3
    */
  def setFalse(): Unit = {
    this.value = false
  }

  /**
    * Sets the value to true.
    *
    * @since 3.3
    */
  def setTrue(): Unit = {
    this.value = true
  }

  /**
    * Sets the value from any JavaBoolean instance.
    *
    * @param value the value to set, not null
    * @throws NullPointerException if the object is null
    */
  override def setValue(value: JavaBoolean): Unit = {
    this.value = value.booleanValue
  }

  /**
    * Checks if the current value is {@code true}.
    *
    * @return {@code true} if the current value is {@code true}
    * @since 2.5
    */
  def isTrue: JavaBoolean = value

  /**
    * Checks if the current value is {@code false}.
    *
    * @return {@code true} if the current value is {@code false}
    * @since 2.5
    */
  def isFalse: JavaBoolean = !value

  /**
    * Returns the value of this MutableJavaBoolean as a boolean.
    *
    * @return the boolean value represented by this object.
    */
  def booleanValue: JavaBoolean = value

  /**
    * Gets this mutable as an instance of JavaBoolean.
    *
    * @return a JavaBoolean instance containing the value from this mutable, never null
    * @since 2.5
    */
  def toBoolean: JavaBoolean = JavaBoolean.valueOf(booleanValue)

  /**
    * Compares this object to the specified object. The result is {@code true} if and only if the argument is
    * not {@code null} and is an {@code MutableJavaBoolean} object that contains the same
    * {@code boolean} value as this object.
    *
    * @param obj the object to compare with, null returns false
    * @return {@code true} if the objects are the same; {@code false} otherwise.
    */
  override def equals(obj: Any): JavaBoolean = {
    if (obj.isInstanceOf[MutableBoolean]) return value == obj.asInstanceOf[MutableBoolean].booleanValue
    false
  }

  /**
    * Returns a suitable hash code for this mutable.
    *
    * @return the hash code returned by {@code JavaBoolean.TRUE} or {@code JavaBoolean.FALSE}
    */
  override def hashCode: Int = if (value) JavaBoolean.TRUE.hashCode
  else JavaBoolean.FALSE.hashCode

  /**
    * Compares this mutable to another in ascending order.
    *
    * @param other the other mutable to compare to, not null
    * @return negative if this is less, zero if equal, positive if greater
    *         where false is less than true
    */
  override def compareTo(other: MutableBoolean): Int = BooleanUtils.compare(this.value, other.value)

  /**
    * Returns the String value of this mutable.
    *
    * @return the mutable value as a string
    */
  override def toString: String = String.valueOf(value)
}