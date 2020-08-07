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

package org.apache.commons.lang3.builder

// adapted from org.apache.axis.utils.IDKey

/**
  * Wrap an identity key (System.identityHashCode())
  * so that an object can only be equal() to itself.
  *
  * This is necessary to disambiguate the occasional duplicate
  * identityHashCodes that can occur.
  */
final class IDKey private[builder] (val value: Any) {
  private val id: Int = System.identityHashCode(value)

  /**
    * returns hash code - i.e. the system identity hashcode.
    *
    * @return the hashcode
    */
  override def hashCode: Int = id

  /**
    * checks if instances are equal
    *
    * @param other The other object to compare to
    * @return if the instances are for the same object
    */
  override def equals(other: Any): Boolean = {
    if (!other.isInstanceOf[IDKey]) return false

    val idKey: IDKey = other.asInstanceOf[IDKey]
    if (id != idKey.id) return false

    // Note that identity equals is used.
    //value eq idKey.value
    ???
  }
}
