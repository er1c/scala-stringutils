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

/**
  * A mutable {@code Object} wrapper.
  *
  * @param < T> the type to set and get
  * @since 2.1
  */
@SerialVersionUID(86241875189L)
class MutableObject[T <: AnyRef]()

/**
  * Constructs a new MutableObject with the default value of {@code null}.
  */
  extends Mutable[T] with Serializable {
  /** The mutable value. */
  private var value: T = null.asInstanceOf[T]

  /**
    * Constructs a new MutableObject with the specified value.
    *
    * @param value the initial value to store
    */
  def this(value: T) = {
    this()
    this.value = value
  }

  /**
    * Gets the value.
    *
    * @return the value, may be null
    */
  override def getValue: T = this.value

  /**
    * Sets the value.
    *
    * @param value the value to set
    */
  override def setValue(value: T): Unit = {
    this.value = value
  }

  /**
    * <p>
    * Compares this object against the specified object. The result is {@code true} if and only if the argument
    * is not {@code null} and is a {@code MutableObject} object that contains the same {@code T}
    * value as this object.
    * </p>
    *
    * @param obj the object to compare with, {@code null} returns {@code false}
    * @return {@code true} if the objects are the same;
    *         {@code true} if the objects have equivalent {@code value} fields;
    *         {@code false} otherwise.
    */
  override def equals(obj: Any): Boolean = {
    if (obj == null) false
    else if (obj.isInstanceOf[T @unchecked] && (this eq obj.asInstanceOf[T])) true
    else if (this.getClass eq obj.getClass) {
      val that = obj.asInstanceOf[MutableObject[_]]
      this.value == that.value
    } else false
  }

  /**
    * Returns the value's hash code or {@code 0} if the value is {@code null}.
    *
    * @return the value's hash code or {@code 0} if the value is {@code null}.
    */
  override def hashCode: Int =
    if (value == null) 0
    else value.hashCode

  /**
    * Returns the String value of this mutable.
    *
    * @return the mutable value as a string
    */
  override def toString: String =
    if (value == null) "null"
    else value.toString
}
