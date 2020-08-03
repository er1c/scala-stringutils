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

package org.apache.commons.lang3.tuple

import java.io.Serializable
import java.util
import java.util.Objects
import org.apache.commons.lang3.builder.CompareToBuilder

/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */


/**
  * <p>A pair consisting of two elements.</p>
  *
  * <p>This class is an abstract implementation defining the basic API.
  * It refers to the elements as 'left' and 'right'. It also implements the
  * {@code Map.Entry} interface where the key is 'left' and the value is 'right'.</p>
  *
  * <p>Subclass implementations may be mutable or immutable.
  * However, there is no restriction on the type of the stored objects that may be stored.
  * If mutable objects are stored in the pair, then the pair itself effectively becomes mutable.</p>
  *
  * @param < L> the left element type
  * @param < R> the right element type
  * @since 3.0
  */
@SerialVersionUID(4954918890077093841L)
object Pair {

  @SerialVersionUID(1L)
  final private class PairAdapter[L, R] extends Pair[L, R] {
    override def getLeft: L = null.asInstanceOf[L]

    override def getRight: R = null.asInstanceOf[R]

    override def setValue(value: R): R = null.asInstanceOf[R]
  }

  /**
    * An empty array.
    * <p>
    * Consider using {@link #emptyArray ( )} to avoid generics warnings.
    * </p>
    *
    * @since 3.10.
    */
  val EMPTY_ARRAY: Array[Pair[_, _]] = Array(new PairAdapter[Any, Any])

  /**
    * Returns the empty array singleton that can be assigned without compiler warning.
    *
    * @param < L> the left element type
    * @param < R> the right element type
    * @return the empty array singleton that can be assigned without compiler warning.
    * @since 3.10.
    */
  @SuppressWarnings(Array("unchecked")) def emptyArray[L, R]: Array[Pair[L, R]] = EMPTY_ARRAY.asInstanceOf[Array[Pair[L, R]]]

  /**
    * <p>Creates an immutable pair of two objects inferring the generic types.</p>
    *
    * <p>This factory allows the pair to be created using inference to
    * obtain the generic types.</p>
    *
    * @param <     L> the left element type
    * @param <     R> the right element type
    * @param left  the left element, may be null
    * @param right the right element, may be null
    * @return a pair formed from the two parameters, not null
    */
  def of[L, R](left: L, right: R): Pair[L, R] = ImmutablePair.of(left, right)

  /**
    * <p>Creates an immutable pair from an existing pair.</p>
    *
    * <p>This factory allows the pair to be created using inference to
    * obtain the generic types.</p>
    *
    * @param <    L> the left element type
    * @param <    R> the right element type
    * @param pair the existing pair.
    * @return a pair formed from the two parameters, not null
    * @since 3.10
    */
  def of[L, R](pair: util.Map.Entry[L, R]): Pair[L, R] = ImmutablePair.of(pair)
}

@SerialVersionUID(4954918890077093841L)
abstract class Pair[L, R] extends util.Map.Entry[L, R] with Comparable[Pair[L, R]] with Serializable {
  /**
    * <p>Compares the pair based on the left element followed by the right element.
    * The types must be {@code Comparable}.</p>
    *
    * @param other the other pair, not null
    * @return negative if this is less, zero if equal, positive if greater
    */
  override def compareTo(other: Pair[L, R]): Int = new CompareToBuilder().append(getLeft, other.getLeft).append(getRight, other.getRight).toComparison

  /**
    * <p>Compares this pair to another based on the two elements.</p>
    *
    * @param obj the object to compare to, null returns false
    * @return true if the elements of the pair are equal
    */
  override def equals(obj: Any): Boolean = {
    obj match {
      case m: util.Map.Entry[_, _] => Objects.equals(getKey, m.getKey) && Objects.equals(getValue, m.getValue)
      case that: AnyRef => that eq this
      case _ => false
    }
  }

  /**
    * <p>Gets the key from this pair.</p>
    *
    * <p>This method implements the {@code Map.Entry} interface returning the
    * left element as the key.</p>
    *
    * @return the left element as the key, may be null
    */
  override final def getKey: L = getLeft

  /**
    * <p>Gets the left element from this pair.</p>
    *
    * <p>When treated as a key-value pair, this is the key.</p>
    *
    * @return the left element, may be null
    */
  def getLeft: L

  /**
    * <p>Gets the right element from this pair.</p>
    *
    * <p>When treated as a key-value pair, this is the value.</p>
    *
    * @return the right element, may be null
    */
  def getRight: R

  /**
    * <p>Gets the value from this pair.</p>
    *
    * <p>This method implements the {@code Map.Entry} interface returning the
    * right element as the value.</p>
    *
    * @return the right element as the value, may be null
    */
  override def getValue: R = getRight

  /**
    * <p>Returns a suitable hash code.
    * The hash code follows the definition in {@code Map.Entry}.</p>
    *
    * @return the hash code
    */
  override def hashCode: Int = { // see Map.Entry API specification
    Objects.hashCode(getKey) ^ Objects.hashCode(getValue)
  }

  /**
    * <p>Returns a String representation of this pair using the format {@code ($left,$right)}.</p>
    *
    * @return a string describing this object, not null
    */
  override def toString: String = "(" + getLeft + ',' + getRight + ')'

  /**
    * <p>Formats the receiver using the given format.</p>
    *
    * <p>This uses {@link java.util.Formattable} to perform the formatting. Two variables may
    * be used to embed the left and right elements. Use {@code %1$s} for the left
    * element (key) and {@code %2$s} for the right element (value).
    * The default format used by {@code toString()} is {@code (%1$s,%2$s)}.</p>
    *
    * @param format the format string, optionally containing {@code %1$s} and {@code %2$s}, not null
    * @return the formatted string, not null
    */
  def toString(format: String): String = String.format(format, getLeft, getRight)
}