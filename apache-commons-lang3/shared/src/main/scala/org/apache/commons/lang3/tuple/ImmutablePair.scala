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

import java.util

/**
  * <p>An immutable pair consisting of two {@code Object} elements.</p>
  *
  * <p>Although the implementation is immutable, there is no restriction on the objects
  * that may be stored. If mutable objects are stored in the pair, then the pair
  * itself effectively becomes mutable. The class is also {@code final}, so a subclass
  * can not add undesirable behavior.</p>
  *
  * <p>#ThreadSafe# if both paired objects are thread-safe</p>
  *
  * @param < L> the left element type
  * @param < R> the right element type
  * @since 3.0
  */
@SerialVersionUID(4954918890077093841L)
object ImmutablePair {
  /**
    * An empty array.
    * <p>
    * Consider using {@link #emptyArray ( )} to avoid generics warnings.
    * </p>
    *
    * @since 3.10.
    */
  val EMPTY_ARRAY = new Array[ImmutablePair[_, _]](0)
  /**
    * An immutable pair of nulls.
    */
  // This is not defined with generics to avoid warnings in call sites.
  @SuppressWarnings(Array("rawtypes")) private val NULL: ImmutablePair[_, _] = of(null, null)

  /**
    * Returns the empty array singleton that can be assigned without compiler warning.
    *
    * @param < L> the left element type
    * @param < R> the right element type
    * @return the empty array singleton that can be assigned without compiler warning.
    * @since 3.10.
    */
  @SuppressWarnings(Array("unchecked")) def emptyArray[L, R]: Array[ImmutablePair[L, R]] =
    EMPTY_ARRAY.asInstanceOf[Array[ImmutablePair[L, R]]]

  /**
    * <p>Creates an immutable pair of two objects inferring the generic types.</p>
    *
    * <p>This factory allows the pair to be created using inference to
    * obtain the generic types.</p>
    *
    * @param <    L> the left element type
    * @param <    R> the right element type
    * @param left the left element, may be null
    * @return a pair formed from the two parameters, not null
    * @since 3.11
    */
  def left[L, R](left: L): Pair[L, R] = ImmutablePair.of(left, null.asInstanceOf[R])

  /**
    * Returns an immutable pair of nulls.
    *
    * @param < L> the left element of this pair. Value is {@code null}.
    * @param < R> the right element of this pair. Value is {@code null}.
    * @return an immutable pair of nulls.
    * @since 3.6
    */
  def nullPair[L, R]: ImmutablePair[L, R] = NULL.asInstanceOf[ImmutablePair[L, R]]

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
  def of[L, R](left: L, right: R) = new ImmutablePair[L, R](left, right)

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
  def of[L, R](pair: util.Map.Entry[L, R]): ImmutablePair[L, R] = {
    var left: L = null.asInstanceOf[L]
    var right: R = null.asInstanceOf[R]

    if (pair != null) {
      left = pair.getKey
      right = pair.getValue
    }

    new ImmutablePair[L, R](left, right)
  }

  /**
    * <p>Creates an immutable pair of two objects inferring the generic types.</p>
    *
    * <p>This factory allows the pair to be created using inference to
    * obtain the generic types.</p>
    *
    * @param <     L> the left element type
    * @param <     R> the right element type
    * @param right the right element, may be null
    * @return a pair formed from the two parameters, not null
    * @since 3.11
    */
  def right[L, R](right: R): Pair[L, R] = ImmutablePair.of(null.asInstanceOf[L], right)
}

@SerialVersionUID(4954918890077093841L)
final class ImmutablePair[L, R](
  /** Left object */
  val left: L,
  /** Right object */
  val right: R)

/**
  * Create a new pair instance.
  *
  * @param left  the left value, may be null
  * @param right the right value, may be null
  */
  extends Pair[L, R] {
  /**
    * {@inheritDoc }
    */
  override def getLeft: L = left

  override def getRight: R = right

  /**
    * <p>Throws {@code UnsupportedOperationException}.</p>
    *
    * <p>This pair is immutable, so this operation is not supported.</p>
    *
    * @param value the value to set
    * @return never
    * @throws UnsupportedOperationException as this operation is not supported
    */
  override def setValue(value: R) = throw new UnsupportedOperationException
}
