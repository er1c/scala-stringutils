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

package org.apache.commons.lang3

import java.lang.{Double => JavaDouble}
import java.util
import java.util.Objects
import java.util.regex.Pattern

/**
  * <p>This class assists in validating arguments. The validation methods are
  * based along the following principles:
  * <ul>
  * <li>An invalid {@code null} argument causes a {@link NullPointerException}.</li>
  * <li>A non-{@code null} argument causes an {@link IllegalArgumentException}.</li>
  * <li>An invalid index into an array/collection/map/string causes an {@link IndexOutOfBoundsException}.</li>
  * </ul>
  *
  * <p>All exceptions messages are
  * <a href="http://docs.oracle.com/javase/1.5.0/docs/api/java/util/Formatter.html#syntax">format strings</a>
  * as defined by the Java platform. For example:</p>
  *
  * <pre>
  * Validate.isTrue(i &gt; 0, "The value must be greater than zero: %d", i);
  * Validate.notNull(surname, "The surname must not be %s", null);
  * </pre>
  *
  * <p>#ThreadSafe#</p>
  *
  * @see java.lang.String#format(String, Object...)
  * @since 2.0
  */
object Validate {
  private val DEFAULT_NOT_NAN_EX_MESSAGE = "The validated value is not a number"
  private val DEFAULT_FINITE_EX_MESSAGE = "The value is invalid: %f"
  private val DEFAULT_EXCLUSIVE_BETWEEN_EX_MESSAGE = "The value %s is not in the specified exclusive range of %s to %s"
  private val DEFAULT_INCLUSIVE_BETWEEN_EX_MESSAGE = "The value %s is not in the specified inclusive range of %s to %s"
  private val DEFAULT_MATCHES_PATTERN_EX = "The string %s does not match the pattern %s"
  private val DEFAULT_IS_NULL_EX_MESSAGE = "The validated object is null"
  private val DEFAULT_IS_TRUE_EX_MESSAGE = "The validated expression is false"
  private val DEFAULT_NO_NULL_ELEMENTS_ARRAY_EX_MESSAGE = "The validated array contains null element at index: %d"
  private val DEFAULT_NO_NULL_ELEMENTS_COLLECTION_EX_MESSAGE = "The validated collection contains null element at index: %d"
  private val DEFAULT_NOT_BLANK_EX_MESSAGE = "The validated character sequence is blank"
  private val DEFAULT_NOT_EMPTY_ARRAY_EX_MESSAGE = "The validated array is empty"
  private val DEFAULT_NOT_EMPTY_CHAR_SEQUENCE_EX_MESSAGE = "The validated character sequence is empty"
  private val DEFAULT_NOT_EMPTY_COLLECTION_EX_MESSAGE = "The validated collection is empty"
  private val DEFAULT_NOT_EMPTY_MAP_EX_MESSAGE = "The validated map is empty"
  private val DEFAULT_VALID_INDEX_ARRAY_EX_MESSAGE = "The validated array index is invalid: %d"
  private val DEFAULT_VALID_INDEX_CHAR_SEQUENCE_EX_MESSAGE = "The validated character sequence index is invalid: %d"
  private val DEFAULT_VALID_INDEX_COLLECTION_EX_MESSAGE = "The validated collection index is invalid: %d"
  private val DEFAULT_VALID_STATE_EX_MESSAGE = "The validated state is false"
  private val DEFAULT_IS_ASSIGNABLE_EX_MESSAGE = "Cannot assign a %s to a %s"
  private val DEFAULT_IS_INSTANCE_OF_EX_MESSAGE = "Expected type: %s, actual: %s"

  /**
    * <p>Validate that the argument condition is {@code true}; otherwise
    * throwing an exception with the specified message. This method is useful when
    * validating according to an arbitrary boolean expression, such as validating a
    * primitive number or using your own custom validation expression.</p>
    *
    * <pre>Validate.isTrue(i &gt; 0.0, "The value must be greater than zero: &#37;d", i);</pre>
    *
    * <p>For performance reasons, the long value is passed as a separate parameter and
    * appended to the exception message only in the case of an error.</p>
    *
    * @param expression the boolean expression to check
    * @param message    the {@link String# format ( String, Object...)} exception message if invalid, not null
    * @param value      the value to append to the message when invalid
    * @throws IllegalArgumentException if expression is {@code false}
    * @see #isTrue(boolean)
    * @see #isTrue(boolean, String, double)
    * @see #isTrue(boolean, String, Object...)
    */
  def isTrue(expression: Boolean, message: String, value: Long): Unit = {
    if (!expression) throw new IllegalArgumentException(message.format(value))
  }

  /**
    * <p>Validate that the argument condition is {@code true}; otherwise
    * throwing an exception with the specified message. This method is useful when
    * validating according to an arbitrary boolean expression, such as validating a
    * primitive number or using your own custom validation expression.</p>
    *
    * <pre>Validate.isTrue(d &gt; 0.0, "The value must be greater than zero: &#37;s", d);</pre>
    *
    * <p>For performance reasons, the double value is passed as a separate parameter and
    * appended to the exception message only in the case of an error.</p>
    *
    * @param expression the boolean expression to check
    * @param message    the {@link String# format ( String, Object...)} exception message if invalid, not null
    * @param value      the value to append to the message when invalid
    * @throws IllegalArgumentException if expression is {@code false}
    * @see #isTrue(boolean)
    * @see #isTrue(boolean, String, long)
    * @see #isTrue(boolean, String, Object...)
    */
  def isTrue(expression: Boolean, message: String, value: Double): Unit = {
    if (!expression) throw new IllegalArgumentException(message.format(value))
  }

  /**
    * <p>Validate that the argument condition is {@code true}; otherwise
    * throwing an exception with the specified message. This method is useful when
    * validating according to an arbitrary boolean expression, such as validating a
    * primitive number or using your own custom validation expression.</p>
    *
    * <pre>
    * Validate.isTrue(i &gt;= min &amp;&amp; i &lt;= max, "The value must be between &#37;d and &#37;d", min, max);
    * Validate.isTrue(myObject.isOk(), "The object is not okay");</pre>
    *
    * @param expression the boolean expression to check
    * @param message    the {@link String# format ( String, Object...)} exception message if invalid, not null
    * @param values     the optional values for the formatted exception message, null array not recommended
    * @throws IllegalArgumentException if expression is {@code false}
    * @see #isTrue(boolean)
    * @see #isTrue(boolean, String, long)
    * @see #isTrue(boolean, String, double)
    */
  def isTrue(expression: Boolean, message: String, values: Any*): Unit = {
    if (!expression) throw new IllegalArgumentException(String.format(message, values))
  }

  /**
    * <p>Validate that the argument condition is {@code true}; otherwise
    * throwing an exception. This method is useful when validating according
    * to an arbitrary boolean expression, such as validating a
    * primitive number or using your own custom validation expression.</p>
    *
    * <pre>
    * Validate.isTrue(i &gt; 0);
    * Validate.isTrue(myObject.isOk());</pre>
    *
    * <p>The message of the exception is &quot;The validated expression is
    * false&quot;.</p>
    *
    * @param expression the boolean expression to check
    * @throws IllegalArgumentException if expression is {@code false}
    * @see #isTrue(boolean, String, long)
    * @see #isTrue(boolean, String, double)
    * @see #isTrue(boolean, String, Object...)
    */
  def isTrue(expression: Boolean): Unit = {
    if (!expression) throw new IllegalArgumentException(DEFAULT_IS_TRUE_EX_MESSAGE)
  }

  /**
    * <p>Validate that the specified argument is not {@code null};
    * otherwise throwing an exception.
    *
    * <pre>Validate.notNull(myObject, "The object must not be null");</pre>
    *
    * <p>The message of the exception is &quot;The validated object is
    * null&quot;.</p>
    *
    * @tparam T     the object type
    * @param object the object to check
    * @return the validated object (never {@code null} for method chaining)
    * @throws NullPointerException if the object is {@code null}
    * @see #notNull(Object, String, Object...)
    */
  def notNull[T](`object`: T): T = notNull(`object`, DEFAULT_IS_NULL_EX_MESSAGE)

  /**
    * <p>Validate that the specified argument is not {@code null};
    * otherwise throwing an exception with the specified message.
    *
    * <pre>Validate.notNull(myObject, "The object must not be null");</pre>
    *
    * @tparam T      the object type
    * @param object  the object to check
    * @param message the {@link String# format ( String, Object...)} exception message if invalid, not null
    * @param values  the optional values for the formatted exception message
    * @return the validated object (never {@code null} for method chaining)
    * @throws NullPointerException if the object is {@code null}
    * @see #notNull(Object)
    */
  def notNull[T](`object`: T, message: String, values: Any*): T = {
    @inline def msg: String = message.format(values)
    Objects.requireNonNull(`object`, msg)
  }

  /**
    * <p>Validate that the specified argument array is neither {@code null}
    * nor a length of zero (no elements); otherwise throwing an exception
    * with the specified message.
    *
    * <pre>Validate.notEmpty(myArray, "The array must not be empty");</pre>
    *
    * @tparam T      the array type
    * @param array   the array to check, validated not null by this method
    * @param message the {@link String# format ( String, Object...)} exception message if invalid, not null
    * @param values  the optional values for the formatted exception message, null array not recommended
    * @return the validated array (never {@code null} method for chaining)
    * @throws NullPointerException     if the array is {@code null}
    * @throws IllegalArgumentException if the array is empty
    * @see #notEmpty(Object[])
    */
  def notEmpty[T](array: Array[T], message: String, values: Any*): Array[T] = {
    @inline def msg: String = message.format(values)
    Objects.requireNonNull(array, msg)
    if (array.length == 0) throw new IllegalArgumentException(msg)
    array
  }

  /**
    * <p>Validate that the specified argument array is neither {@code null}
    * nor a length of zero (no elements); otherwise throwing an exception.
    *
    * <pre>Validate.notEmpty(myArray);</pre>
    *
    * <p>The message in the exception is &quot;The validated array is
    * empty&quot;.
    *
    * @tparam T    the array type
    * @param array the array to check, validated not null by this method
    * @return the validated array (never {@code null} method for chaining)
    * @throws NullPointerException     if the array is {@code null}
    * @throws IllegalArgumentException if the array is empty
    * @see #notEmpty(Object[], String, Object...)
    */
  def notEmpty[T](array: Array[T]): Array[T] =
    notEmpty(array, DEFAULT_NOT_EMPTY_ARRAY_EX_MESSAGE)

  /**
    * <p>Validate that the specified argument collection is neither {@code null}
    * nor a size of zero (no elements); otherwise throwing an exception
    * with the specified message.
    *
    * <pre>Validate.notEmpty(myCollection, "The collection must not be empty");</pre>
    *
    * @tparam T         the collection type
    * @param collection the collection to check, validated not null by this method
    * @param message    the {@link String# format ( String, Object...)} exception message if invalid, not null
    * @param values     the optional values for the formatted exception message, null array not recommended
    * @return the validated collection (never {@code null} method for chaining)
    * @throws NullPointerException     if the collection is {@code null}
    * @throws IllegalArgumentException if the collection is empty
    * @see #notEmpty(Object[])
    */
  def notEmpty[T <: util.Collection[_]](collection: T, message: String, values: Any*): T = {
    @inline def msg: String = message.format(values)
    Objects.requireNonNull(collection, msg)
    if (collection.isEmpty) throw new IllegalArgumentException(msg)
    collection
  }

  /**
    * <p>Validate that the specified argument collection is neither {@code null}
    * nor a size of zero (no elements); otherwise throwing an exception.
    *
    * <pre>Validate.notEmpty(myCollection);</pre>
    *
    * <p>The message in the exception is &quot;The validated collection is
    * empty&quot;.</p>
    *
    * @tparam T         the collection type
    * @param collection the collection to check, validated not null by this method
    * @return the validated collection (never {@code null} method for chaining)
    * @throws NullPointerException     if the collection is {@code null}
    * @throws IllegalArgumentException if the collection is empty
    * @see #notEmpty(Collection, String, Object...)
    */
  def notEmpty[T <: util.Collection[Any]](collection: T): T =
    notEmpty(collection, DEFAULT_NOT_EMPTY_COLLECTION_EX_MESSAGE)

  /**
    * <p>Validate that the specified argument map is neither {@code null}
    * nor a size of zero (no elements); otherwise throwing an exception
    * with the specified message.
    *
    * <pre>Validate.notEmpty(myMap, "The map must not be empty");</pre>
    *
    * @tparam T      the map type
    * @param map     the map to check, validated not null by this method
    * @param message the {@link String# format ( String, Object...)} exception message if invalid, not null
    * @param values  the optional values for the formatted exception message, null array not recommended
    * @return the validated map (never {@code null} method for chaining)
    * @throws NullPointerException     if the map is {@code null}
    * @throws IllegalArgumentException if the map is empty
    * @see #notEmpty(Object[])
    */
  def notEmpty[T <: util.Map[_, _]](map: T, message: String, values: Any*)(implicit ev: DummyImplicit): T = {
    @inline def msg: String = message.format(values)
    Objects.requireNonNull(map, msg)
    if (map.isEmpty) throw new IllegalArgumentException(msg)
    map
  }

  /**
    * <p>Validate that the specified argument map is neither {@code null}
    * nor a size of zero (no elements); otherwise throwing an exception.
    *
    * <pre>Validate.notEmpty(myMap);</pre>
    *
    * <p>The message in the exception is &quot;The validated map is
    * empty&quot;.</p>
    *
    * @tparam T  the map type
    * @param map the map to check, validated not null by this method
    * @return the validated map (never {@code null} method for chaining)
    * @throws NullPointerException     if the map is {@code null}
    * @throws IllegalArgumentException if the map is empty
    * @see #notEmpty(Map, String, Object...)
    */
  def notEmpty[T <: util.Map[_, _]](map: T)(implicit ev: DummyImplicit, ev2: DummyImplicit): T =
    notEmpty(map, DEFAULT_NOT_EMPTY_MAP_EX_MESSAGE)(ev)

  /**
    * <p>Validate that the specified argument character sequence is
    * neither {@code null} nor a length of zero (no characters);
    * otherwise throwing an exception with the specified message.
    *
    * <pre>Validate.notEmpty(myString, "The string must not be empty");</pre>
    *
    * @tparam T      the character sequence type
    * @param chars   the character sequence to check, validated not null by this method
    * @param message the {@link String# format ( String, Object...)} exception message if invalid, not null
    * @param values  the optional values for the formatted exception message, null array not recommended
    * @return the validated character sequence (never {@code null} method for chaining)
    * @throws NullPointerException     if the character sequence is {@code null}
    * @throws IllegalArgumentException if the character sequence is empty
    * @see #notEmpty(CharSequence)
    */
  def notEmpty[T <: CharSequence](chars: T, message: String, values: AnyRef*): T = {
    @inline def msg: String = message.format(values)
    Objects.requireNonNull(chars, msg)
    if (chars.length == 0) throw new IllegalArgumentException(msg)
    chars
  }

  /**
    * <p>Validate that the specified argument character sequence is
    * neither {@code null} nor a length of zero (no characters);
    * otherwise throwing an exception with the specified message.
    *
    * <pre>Validate.notEmpty(myString);</pre>
    *
    * <p>The message in the exception is &quot;The validated
    * character sequence is empty&quot;.</p>
    *
    * @tparam T    the character sequence type
    * @param chars the character sequence to check, validated not null by this method
    * @return the validated character sequence (never {@code null} method for chaining)
    * @throws NullPointerException     if the character sequence is {@code null}
    * @throws IllegalArgumentException if the character sequence is empty
    * @see #notEmpty(CharSequence, String, Object...)
    */
  def notEmpty[T <: CharSequence](chars: T)(implicit ev: DummyImplicit): T =
    notEmpty(chars, DEFAULT_NOT_EMPTY_CHAR_SEQUENCE_EX_MESSAGE)

  /**
    * <p>Validate that the specified argument character sequence is
    * neither {@code null}, a length of zero (no characters), empty
    * nor whitespace; otherwise throwing an exception with the specified
    * message.
    *
    * <pre>Validate.notBlank(myString, "The string must not be blank");</pre>
    *
    * @tparam T      the character sequence type
    * @param chars   the character sequence to check, validated not null by this method
    * @param message the {@link String# format ( String, Object...)} exception message if invalid, not null
    * @param values  the optional values for the formatted exception message, null array not recommended
    * @return the validated character sequence (never {@code null} method for chaining)
    * @throws NullPointerException     if the character sequence is {@code null}
    * @throws IllegalArgumentException if the character sequence is blank
    * @see #notBlank(CharSequence)
    * @since 3.0
    */
  def notBlank[T <: CharSequence](chars: T, message: String, values: Any*): T = {
    @inline def msg: String = message.format(values)
    Objects.requireNonNull(chars, msg)
    if (StringUtils.isBlank(chars)) throw new IllegalArgumentException(msg)
    chars
  }

  /**
    * <p>Validate that the specified argument character sequence is
    * neither {@code null}, a length of zero (no characters), empty
    * nor whitespace; otherwise throwing an exception.
    *
    * <pre>Validate.notBlank(myString);</pre>
    *
    * <p>The message in the exception is &quot;The validated character
    * sequence is blank&quot;.</p>
    *
    * @tparam T    the character sequence type
    * @param chars the character sequence to check, validated not null by this method
    * @return the validated character sequence (never {@code null} method for chaining)
    * @throws NullPointerException     if the character sequence is {@code null}
    * @throws IllegalArgumentException if the character sequence is blank
    * @see #notBlank(CharSequence, String, Object...)
    * @since 3.0
    */
  def notBlank[T <: CharSequence](chars: T): T = notBlank(chars, DEFAULT_NOT_BLANK_EX_MESSAGE)

  /**
    * <p>Validate that the specified argument array is neither
    * {@code null} nor contains any elements that are {@code null};
    * otherwise throwing an exception with the specified message.
    *
    * <pre>Validate.noNullElements(myArray, "The array contain null at position %d");</pre>
    *
    * <p>If the array is {@code null}, then the message in the exception
    * is &quot;The validated object is null&quot;.</p>
    *
    * <p>If the array has a {@code null} element, then the iteration
    * index of the invalid element is appended to the {@code values}
    * argument.</p>
    *
    * @tparam T      the array type
    * @param array   the array to check, validated not null by this method
    * @param message the {@link String# format ( String, Object...)} exception message if invalid, not null
    * @param values  the optional values for the formatted exception message, null array not recommended
    * @return the validated array (never {@code null} method for chaining)
    * @throws NullPointerException     if the array is {@code null}
    * @throws IllegalArgumentException if an element is {@code null}
    * @see #noNullElements(Object[])
    */
  def noNullElements[T](array: Array[T], message: String, values: Any*): Array[T] = {
    notNull(array)
    for (i <- 0 until array.length) {
      if (array(i) == null) {
        val values2 = ArrayUtils.add(values.toArray, Integer.valueOf(i))
        throw new IllegalArgumentException(String.format(message, values2))
      }
    }
    array
  }

  /**
    * <p>Validate that the specified argument array is neither
    * {@code null} nor contains any elements that are {@code null};
    * otherwise throwing an exception.</p>
    *
    * <pre>Validate.noNullElements(myArray);</pre>
    *
    * <p>If the array is {@code null}, then the message in the exception
    * is &quot;The validated object is null&quot;.</p>
    *
    * <p>If the array has a {@code null} element, then the message in the
    * exception is &quot;The validated array contains null element at index:
    * &quot; followed by the index.</p>
    *
    * @tparam T    the array type
    * @param array the array to check, validated not null by this method
    * @return the validated array (never {@code null} method for chaining)
    * @throws NullPointerException     if the array is {@code null}
    * @throws IllegalArgumentException if an element is {@code null}
    * @see #noNullElements(Object[], String, Object...)
    */
  def noNullElements[T](array: Array[T]): Array[T] = noNullElements(array, DEFAULT_NO_NULL_ELEMENTS_ARRAY_EX_MESSAGE)

  /**
    * <p>Validate that the specified argument iterable is neither
    * {@code null} nor contains any elements that are {@code null};
    * otherwise throwing an exception with the specified message.
    *
    * <pre>Validate.noNullElements(myCollection, "The collection contains null at position %d");</pre>
    *
    * <p>If the iterable is {@code null}, then the message in the exception
    * is &quot;The validated object is null&quot;.</p>
    *
    * <p>If the iterable has a {@code null} element, then the iteration
    * index of the invalid element is appended to the {@code values}
    * argument.</p>
    *
    * @tparam T       the iterable type
    * @param iterable the iterable to check, validated not null by this method
    * @param message  the {@link String# format ( String, Object...)} exception message if invalid, not null
    * @param values   the optional values for the formatted exception message, null array not recommended
    * @return the validated iterable (never {@code null} method for chaining)
    * @throws NullPointerException     if the array is {@code null}
    * @throws IllegalArgumentException if an element is {@code null}
    * @see #noNullElements(Iterable)
    */
  def noNullElements[T <: Iterable[Any]](iterable: T, message: String, values: Any*): T = {
    notNull(iterable)
    var i = 0
    val it = iterable.iterator
    while ( {
      it.hasNext
    }) {
      if (it.next() == null) {
        val values2 = ArrayUtils.addAll(values.toArray, Integer.valueOf(i))
        throw new IllegalArgumentException(String.format(message, values2))
      }

      i += 1
    }
    iterable
  }

  /**
    * <p>Validate that the specified argument iterable is neither
    * {@code null} nor contains any elements that are {@code null};
    * otherwise throwing an exception.
    *
    * <pre>Validate.noNullElements(myCollection);</pre>
    *
    * <p>If the iterable is {@code null}, then the message in the exception
    * is &quot;The validated object is null&quot;.</p>
    *
    * <p>If the array has a {@code null} element, then the message in the
    * exception is &quot;The validated iterable contains null element at index:
    * &quot; followed by the index.</p>
    *
    * @tparam T       the iterable type
    * @param iterable the iterable to check, validated not null by this method
    * @return the validated iterable (never {@code null} method for chaining)
    * @throws NullPointerException     if the array is {@code null}
    * @throws IllegalArgumentException if an element is {@code null}
    * @see #noNullElements(Iterable, String, Object...)
    */
  def noNullElements[T <: Iterable[Any]](iterable: T): T = noNullElements(iterable, DEFAULT_NO_NULL_ELEMENTS_COLLECTION_EX_MESSAGE)

  /**
    * <p>Validates that the index is within the bounds of the argument
    * array; otherwise throwing an exception with the specified message.</p>
    *
    * <pre>Validate.validIndex(myArray, 2, "The array index is invalid: ");</pre>
    *
    * <p>If the array is {@code null}, then the message of the exception
    * is &quot;The validated object is null&quot;.</p>
    *
    * @tparam T      the array type
    * @param array   the array to check, validated not null by this method
    * @param index   the index to check
    * @param message the {@link String# format ( String, Object...)} exception message if invalid, not null
    * @param values  the optional values for the formatted exception message, null array not recommended
    * @return the validated array (never {@code null} for method chaining)
    * @throws NullPointerException      if the array is {@code null}
    * @throws IndexOutOfBoundsException if the index is invalid
    * @see #validIndex(Object[], int)
    * @since 3.0
    */
  def validIndex[T <: AnyRef](array: Array[T], index: Int, message: String, values: AnyRef*): Array[T] = {
    notNull(array)
    if (index < 0 || index >= array.length) throw new IndexOutOfBoundsException(String.format(message, values))
    array
  }

  /**
    * <p>Validates that the index is within the bounds of the argument
    * array; otherwise throwing an exception.</p>
    *
    * <pre>Validate.validIndex(myArray, 2);</pre>
    *
    * <p>If the array is {@code null}, then the message of the exception
    * is &quot;The validated object is null&quot;.</p>
    *
    * <p>If the index is invalid, then the message of the exception is
    * &quot;The validated array index is invalid: &quot; followed by the
    * index.</p>
    *
    * @tparam T    the array type
    * @param array the array to check, validated not null by this method
    * @param index the index to check
    * @return the validated array (never {@code null} for method chaining)
    * @throws NullPointerException      if the array is {@code null}
    * @throws IndexOutOfBoundsException if the index is invalid
    * @see #validIndex(Object[], int, String, Object...)
    * @since 3.0
    */
  def validIndex[T <: AnyRef](array: Array[T], index: Int): Array[T] = validIndex(array, index, DEFAULT_VALID_INDEX_ARRAY_EX_MESSAGE, Integer.valueOf(index))

  /**
    * <p>Validates that the index is within the bounds of the argument
    * collection; otherwise throwing an exception with the specified message.</p>
    *
    * <pre>Validate.validIndex(myCollection, 2, "The collection index is invalid: ");</pre>
    *
    * <p>If the collection is {@code null}, then the message of the
    * exception is &quot;The validated object is null&quot;.</p>
    *
    * @tparam T         the collection type
    * @param collection the collection to check, validated not null by this method
    * @param index      the index to check
    * @param message    the {@link String# format ( String, Object...)} exception message if invalid, not null
    * @param values     the optional values for the formatted exception message, null array not recommended
    * @return the validated collection (never {@code null} for chaining)
    * @throws NullPointerException      if the collection is {@code null}
    * @throws IndexOutOfBoundsException if the index is invalid
    * @see #validIndex(Collection, int)
    * @since 3.0
    */
  def validIndex[T <: util.Collection[_]](collection: T, index: Int, message: String, values: Any*): T = {
    notNull(collection)
    if (index < 0 || index >= collection.size) throw new IndexOutOfBoundsException(String.format(message, values))
    collection
  }

  /**
    * <p>Validates that the index is within the bounds of the argument
    * collection; otherwise throwing an exception.</p>
    *
    * <pre>Validate.validIndex(myCollection, 2);</pre>
    *
    * <p>If the index is invalid, then the message of the exception
    * is &quot;The validated collection index is invalid: &quot;
    * followed by the index.</p>
    *
    * @tparam T         the collection type
    * @param collection the collection to check, validated not null by this method
    * @param index      the index to check
    * @return the validated collection (never {@code null} for method chaining)
    * @throws NullPointerException      if the collection is {@code null}
    * @throws IndexOutOfBoundsException if the index is invalid
    * @see #validIndex(Collection, int, String, Object...)
    * @since 3.0
    */
  def validIndex[T <: util.Collection[_]](collection: T, index: Int): T =
    validIndex(collection, index, DEFAULT_VALID_INDEX_COLLECTION_EX_MESSAGE, Integer.valueOf(index))

  /**
    * <p>Validates that the index is within the bounds of the argument
    * character sequence; otherwise throwing an exception with the
    * specified message.</p>
    *
    * <pre>Validate.validIndex(myStr, 2, "The string index is invalid: ");</pre>
    *
    * <p>If the character sequence is {@code null}, then the message
    * of the exception is &quot;The validated object is null&quot;.</p>
    *
    * @tparam T      the character sequence type
    * @param chars   the character sequence to check, validated not null by this method
    * @param index   the index to check
    * @param message the {@link String# format ( String, Object...)} exception message if invalid, not null
    * @param values  the optional values for the formatted exception message, null array not recommended
    * @return the validated character sequence (never {@code null} for method chaining)
    * @throws NullPointerException      if the character sequence is {@code null}
    * @throws IndexOutOfBoundsException if the index is invalid
    * @see #validIndex(CharSequence, int)
    * @since 3.0
    */
  def validIndex[T <: CharSequence](chars: T, index: Int, message: String, values: Any*)(implicit ev: DummyImplicit): T = {
    notNull(chars)
    if (index < 0 || index >= chars.length) throw new IndexOutOfBoundsException(String.format(message, values))
    chars
  }

  /**
    * <p>Validates that the index is within the bounds of the argument
    * character sequence; otherwise throwing an exception.</p>
    *
    * <pre>Validate.validIndex(myStr, 2);</pre>
    *
    * <p>If the character sequence is {@code null}, then the message
    * of the exception is &quot;The validated object is
    * null&quot;.</p>
    *
    * <p>If the index is invalid, then the message of the exception
    * is &quot;The validated character sequence index is invalid: &quot;
    * followed by the index.</p>
    *
    * @tparam T    the character sequence type
    * @param chars the character sequence to check, validated not null by this method
    * @param index the index to check
    * @return the validated character sequence (never {@code null} for method chaining)
    * @throws NullPointerException      if the character sequence is {@code null}
    * @throws IndexOutOfBoundsException if the index is invalid
    * @see #validIndex(CharSequence, int, String, Object...)
    * @since 3.0
    */
  def validIndex[T <: CharSequence](chars: T, index: Int)(implicit ev: DummyImplicit): T =
    validIndex(chars, index, DEFAULT_VALID_INDEX_CHAR_SEQUENCE_EX_MESSAGE, Integer.valueOf(index))

  /**
    * <p>Validate that the stateful condition is {@code true}; otherwise
    * throwing an exception. This method is useful when validating according
    * to an arbitrary boolean expression, such as validating a
    * primitive number or using your own custom validation expression.</p>
    *
    * <pre>
    * Validate.validState(field &gt; 0);
    * Validate.validState(this.isOk());</pre>
    *
    * <p>The message of the exception is &quot;The validated state is
    * false&quot;.</p>
    *
    * @param expression the boolean expression to check
    * @throws IllegalStateException if expression is {@code false}
    * @see #validState(boolean, String, Object...)
    * @since 3.0
    */
  def validState(expression: Boolean): Unit = {
    if (!expression) throw new IllegalStateException(DEFAULT_VALID_STATE_EX_MESSAGE)
  }

  /**
    * <p>Validate that the stateful condition is {@code true}; otherwise
    * throwing an exception with the specified message. This method is useful when
    * validating according to an arbitrary boolean expression, such as validating a
    * primitive number or using your own custom validation expression.</p>
    *
    * <pre>Validate.validState(this.isOk(), "The state is not OK: %s", myObject);</pre>
    *
    * @param expression the boolean expression to check
    * @param message    the {@link String# format ( String, Object...)} exception message if invalid, not null
    * @param values     the optional values for the formatted exception message, null array not recommended
    * @throws IllegalStateException if expression is {@code false}
    * @see #validState(boolean)
    * @since 3.0
    */
  def validState(expression: Boolean, message: String, values: Any*): Unit = {
    if (!expression) throw new IllegalStateException(String.format(message, values))
  }

  /**
    * <p>Validate that the specified argument character sequence matches the specified regular
    * expression pattern; otherwise throwing an exception.</p>
    *
    * <pre>Validate.matchesPattern("hi", "[a-z]*");</pre>
    *
    * <p>The syntax of the pattern is the one used in the {@link Pattern} class.</p>
    *
    * @param input   the character sequence to validate, not null
    * @param pattern the regular expression pattern, not null
    * @throws IllegalArgumentException if the character sequence does not match the pattern
    * @see #matchesPattern(CharSequence, String, String, Object...)
    * @since 3.0
    */
  def matchesPattern(input: CharSequence, pattern: String): Unit = { // TODO when breaking BC, consider returning input
    if (!Pattern.matches(pattern, input)) throw new IllegalArgumentException(String.format(DEFAULT_MATCHES_PATTERN_EX, input, pattern))
  }

  /**
    * <p>Validate that the specified argument character sequence matches the specified regular
    * expression pattern; otherwise throwing an exception with the specified message.</p>
    *
    * <pre>Validate.matchesPattern("hi", "[a-z]*", "%s does not match %s", "hi" "[a-z]*");</pre>
    *
    * <p>The syntax of the pattern is the one used in the {@link Pattern} class.</p>
    *
    * @param input   the character sequence to validate, not null
    * @param pattern the regular expression pattern, not null
    * @param message the {@link String# format ( String, Object...)} exception message if invalid, not null
    * @param values  the optional values for the formatted exception message, null array not recommended
    * @throws IllegalArgumentException if the character sequence does not match the pattern
    * @see #matchesPattern(CharSequence, String)
    * @since 3.0
    */
  def matchesPattern(input: CharSequence, pattern: String, message: String, values: Any*): Unit = {
    if (!Pattern.matches(pattern, input)) throw new IllegalArgumentException(String.format(message, values))
  }

  /**
    * <p>Validates that the specified argument is not {@code NaN}; otherwise
    * throwing an exception.</p>
    *
    * <pre>Validate.notNaN(myDouble);</pre>
    *
    * <p>The message of the exception is &quot;The validated value is not a
    * number&quot;.</p>
    *
    * @param value the value to validate
    * @throws IllegalArgumentException if the value is not a number
    * @see #notNaN(double, java.lang.String, java.lang.Object...)
    * @since 3.5
    */
  def notNaN(value: Double): Unit = {
    notNaN(value, DEFAULT_NOT_NAN_EX_MESSAGE)
  }

  /**
    * <p>Validates that the specified argument is not {@code NaN}; otherwise
    * throwing an exception with the specified message.</p>
    *
    * <pre>Validate.notNaN(myDouble, "The value must be a number");</pre>
    *
    * @param value   the value to validate
    * @param message the {@link String# format ( String, Object...)} exception message if invalid, not null
    * @param values  the optional values for the formatted exception message
    * @throws IllegalArgumentException if the value is not a number
    * @see #notNaN(double)
    * @since 3.5
    */
  def notNaN(value: Double, message: String, values: Any*): Unit = {
    if (JavaDouble.isNaN(value)) throw new IllegalArgumentException(String.format(message, values))
  }

  /**
    * <p>Validates that the specified argument is not infinite or {@code NaN};
    * otherwise throwing an exception.</p>
    *
    * <pre>Validate.finite(myDouble);</pre>
    *
    * <p>The message of the exception is &quot;The value is invalid: %f&quot;.</p>
    *
    * @param value the value to validate
    * @throws IllegalArgumentException if the value is infinite or {@code NaN}
    * @see #finite(double, java.lang.String, java.lang.Object...)
    * @since 3.5
    */
  def finite(value: Double): Unit = {
    finite(value, DEFAULT_FINITE_EX_MESSAGE, value)
  }

  /**
    * <p>Validates that the specified argument is not infinite or {@code NaN};
    * otherwise throwing an exception with the specified message.</p>
    *
    * <pre>Validate.finite(myDouble, "The argument must contain a numeric value");</pre>
    *
    * @param value   the value to validate
    * @param message the {@link String# format ( String, Object...)} exception message if invalid, not null
    * @param values  the optional values for the formatted exception message
    * @throws IllegalArgumentException if the value is infinite or {@code NaN}
    * @see #finite(double)
    * @since 3.5
    */
  def finite(value: Double, message: String, values: Any*): Unit = {
    if (JavaDouble.isNaN(value) || JavaDouble.isInfinite(value)) throw new IllegalArgumentException(String.format(message, values))
  }

  /**
    * <p>Validate that the specified argument object fall between the two
    * inclusive values specified; otherwise, throws an exception.</p>
    *
    * <pre>Validate.inclusiveBetween(0, 2, 1);</pre>
    *
    * @tparam T    the type of the argument object
    * @param start the inclusive start value, not null
    * @param end   the inclusive end value, not null
    * @param value the object to validate, not null
    * @throws IllegalArgumentException if the value falls outside the boundaries
    * @see #inclusiveBetween(Object, Object, Comparable, String, Object...)
    * @since 3.0
    */
  def inclusiveBetween[T](start: T, `end`: T, value: Comparable[T]): Unit = { // TODO when breaking BC, consider returning value
    if (value.compareTo(start) < 0 || value.compareTo(`end`) > 0)
      throw new IllegalArgumentException(DEFAULT_INCLUSIVE_BETWEEN_EX_MESSAGE.format(value, start, `end`))
  }

  /**
    * <p>Validate that the specified argument object fall between the two
    * inclusive values specified; otherwise, throws an exception with the
    * specified message.</p>
    *
    * <pre>Validate.inclusiveBetween(0, 2, 1, "Not in boundaries");</pre>
    *
    * @tparam T      the type of the argument object
    * @param start   the inclusive start value, not null
    * @param end     the inclusive end value, not null
    * @param value   the object to validate, not null
    * @param message the {@link String# format ( String, Object...)} exception message if invalid, not null
    * @param values  the optional values for the formatted exception message, null array not recommended
    * @throws IllegalArgumentException if the value falls outside the boundaries
    * @see #inclusiveBetween(Object, Object, Comparable)
    * @since 3.0
    */
  def inclusiveBetween[T](start: T, `end`: T, value: Comparable[T], message: String, values: Any*): Unit = {
    if (value.compareTo(start) < 0 || value.compareTo(`end`) > 0) throw new IllegalArgumentException(String.format(message, values))
  }

  /**
    * Validate that the specified primitive value falls between the two
    * inclusive values specified; otherwise, throws an exception.
    *
    * <pre>Validate.inclusiveBetween(0, 2, 1);</pre>
    *
    * @param start the inclusive start value
    * @param end   the inclusive end value
    * @param value the value to validate
    * @throws IllegalArgumentException if the value falls outside the boundaries (inclusive)
    * @since 3.3
    */
  @SuppressWarnings(Array("boxing")) def inclusiveBetween(start: Long, `end`: Long, value: Long): Unit = {
    if (value < start || value > `end`)
      throw new IllegalArgumentException(DEFAULT_INCLUSIVE_BETWEEN_EX_MESSAGE.format(value, start, `end`))
  }

  /**
    * Validate that the specified primitive value falls between the two
    * inclusive values specified; otherwise, throws an exception with the
    * specified message.
    *
    * <pre>Validate.inclusiveBetween(0, 2, 1, "Not in range");</pre>
    *
    * @param start   the inclusive start value
    * @param end     the inclusive end value
    * @param value   the value to validate
    * @param message the exception message if invalid, not null
    * @throws IllegalArgumentException if the value falls outside the boundaries
    * @since 3.3
    */
  def inclusiveBetween(start: Long, `end`: Long, value: Long, message: String): Unit = {
    if (value < start || value > `end`) throw new IllegalArgumentException(message)
  }

  /**
    * Validate that the specified primitive value falls between the two
    * inclusive values specified; otherwise, throws an exception.
    *
    * <pre>Validate.inclusiveBetween(0.1, 2.1, 1.1);</pre>
    *
    * @param start the inclusive start value
    * @param end   the inclusive end value
    * @param value the value to validate
    * @throws IllegalArgumentException if the value falls outside the boundaries (inclusive)
    * @since 3.3
    */
  @SuppressWarnings(Array("boxing")) def inclusiveBetween(start: Double, `end`: Double, value: Double): Unit = {
    if (value < start || value > `end`)
      throw new IllegalArgumentException(DEFAULT_INCLUSIVE_BETWEEN_EX_MESSAGE.format(value, start, `end`))
  }

  /**
    * Validate that the specified primitive value falls between the two
    * inclusive values specified; otherwise, throws an exception with the
    * specified message.
    *
    * <pre>Validate.inclusiveBetween(0.1, 2.1, 1.1, "Not in range");</pre>
    *
    * @param start   the inclusive start value
    * @param end     the inclusive end value
    * @param value   the value to validate
    * @param message the exception message if invalid, not null
    * @throws IllegalArgumentException if the value falls outside the boundaries
    * @since 3.3
    */
  def inclusiveBetween(start: Double, `end`: Double, value: Double, message: String): Unit = {
    if (value < start || value > `end`) throw new IllegalArgumentException(message)
  }

  /**
    * <p>Validate that the specified argument object fall between the two
    * exclusive values specified; otherwise, throws an exception.</p>
    *
    * <pre>Validate.exclusiveBetween(0, 2, 1);</pre>
    *
    * @tparam T    the type of the argument object
    * @param start the exclusive start value, not null
    * @param end   the exclusive end value, not null
    * @param value the object to validate, not null
    * @throws IllegalArgumentException if the value falls outside the boundaries
    * @see #exclusiveBetween(Object, Object, Comparable, String, Object...)
    * @since 3.0
    */
  def exclusiveBetween[T](start: T, `end`: T, value: Comparable[T]): Unit = {
    if (value.compareTo(start) <= 0 || value.compareTo(`end`) >= 0)
      throw new IllegalArgumentException(DEFAULT_EXCLUSIVE_BETWEEN_EX_MESSAGE.format(value, start, `end`))
  }

  /**
    * <p>Validate that the specified argument object fall between the two
    * exclusive values specified; otherwise, throws an exception with the
    * specified message.</p>
    *
    * <pre>Validate.exclusiveBetween(0, 2, 1, "Not in boundaries");</pre>
    *
    * @tparam T      the type of the argument object
    * @param start   the exclusive start value, not null
    * @param end     the exclusive end value, not null
    * @param value   the object to validate, not null
    * @param message the {@link String# format ( String, Object...)} exception message if invalid, not null
    * @param values  the optional values for the formatted exception message, null array not recommended
    * @throws IllegalArgumentException if the value falls outside the boundaries
    * @see #exclusiveBetween(Object, Object, Comparable)
    * @since 3.0
    */
  def exclusiveBetween[T](start: T, `end`: T, value: Comparable[T], message: String, values: Any*): Unit = {
    if (value.compareTo(start) <= 0 || value.compareTo(`end`) >= 0) throw new IllegalArgumentException(String.format(message, values))
  }

  /**
    * Validate that the specified primitive value falls between the two
    * exclusive values specified; otherwise, throws an exception.
    *
    * <pre>Validate.exclusiveBetween(0, 2, 1);</pre>
    *
    * @param start the exclusive start value
    * @param end   the exclusive end value
    * @param value the value to validate
    * @throws IllegalArgumentException if the value falls out of the boundaries
    * @since 3.3
    */
  @SuppressWarnings(Array("boxing")) def exclusiveBetween(start: Long, `end`: Long, value: Long): Unit = {
    if (value <= start || value >= `end`)
      throw new IllegalArgumentException(DEFAULT_EXCLUSIVE_BETWEEN_EX_MESSAGE.format(value, start, `end`))
  }

  /**
    * Validate that the specified primitive value falls between the two
    * exclusive values specified; otherwise, throws an exception with the
    * specified message.
    *
    * <pre>Validate.exclusiveBetween(0, 2, 1, "Not in range");</pre>
    *
    * @param start   the exclusive start value
    * @param end     the exclusive end value
    * @param value   the value to validate
    * @param message the exception message if invalid, not null
    * @throws IllegalArgumentException if the value falls outside the boundaries
    * @since 3.3
    */
  def exclusiveBetween(start: Long, `end`: Long, value: Long, message: String): Unit = {
    if (value <= start || value >= `end`) throw new IllegalArgumentException(message)
  }

  /**
    * Validate that the specified primitive value falls between the two
    * exclusive values specified; otherwise, throws an exception.
    *
    * <pre>Validate.exclusiveBetween(0.1, 2.1, 1.1);</pre>
    *
    * @param start the exclusive start value
    * @param end   the exclusive end value
    * @param value the value to validate
    * @throws IllegalArgumentException if the value falls out of the boundaries
    * @since 3.3
    */
  @SuppressWarnings(Array("boxing")) def exclusiveBetween(start: Double, `end`: Double, value: Double): Unit = {
    if (value <= start || value >= `end`)
      throw new IllegalArgumentException(DEFAULT_EXCLUSIVE_BETWEEN_EX_MESSAGE.format(value, start, `end`))
  }

  /**
    * Validate that the specified primitive value falls between the two
    * exclusive values specified; otherwise, throws an exception with the
    * specified message.
    *
    * <pre>Validate.exclusiveBetween(0.1, 2.1, 1.1, "Not in range");</pre>
    *
    * @param start   the exclusive start value
    * @param end     the exclusive end value
    * @param value   the value to validate
    * @param message the exception message if invalid, not null
    * @throws IllegalArgumentException if the value falls outside the boundaries
    * @since 3.3
    */
  def exclusiveBetween(start: Double, `end`: Double, value: Double, message: String): Unit = {
    if (value <= start || value >= `end`) throw new IllegalArgumentException(message)
  }

  /**
    * Validates that the argument is an instance of the specified class, if not throws an exception.
    *
    * <p>This method is useful when validating according to an arbitrary class</p>
    *
    * <pre>Validate.isInstanceOf(OkClass.class, object);</pre>
    *
    * <p>The message of the exception is &quot;Expected type: {type}, actual: {obj_type}&quot;</p>
    *
    * @param type the class the object must be validated against, not null
    * @param obj  the object to check, null throws an exception
    * @throws IllegalArgumentException if argument is not of specified class
    * @see #isInstanceOf(Class, Object, String, Object...)
    * @since 3.0
    */
  def isInstanceOf(`type`: Class[_], obj: Any): Unit = { // TODO when breaking BC, consider returning obj
    if (!`type`.isInstance(obj)) throw new IllegalArgumentException(String.format(DEFAULT_IS_INSTANCE_OF_EX_MESSAGE, `type`.getName, if (obj == null) "null"
    else obj.getClass.getName))
  }

  /**
    * <p>Validate that the argument is an instance of the specified class; otherwise
    * throwing an exception with the specified message. This method is useful when
    * validating according to an arbitrary class</p>
    *
    * <pre>Validate.isInstanceOf(OkClass.class, object, "Wrong class, object is of class %s",
    * object.getClass().getName());</pre>
    *
    * @param type    the class the object must be validated against, not null
    * @param obj     the object to check, null throws an exception
    * @param message the {@link String# format ( String, Object...)} exception message if invalid, not null
    * @param values  the optional values for the formatted exception message, null array not recommended
    * @throws IllegalArgumentException if argument is not of specified class
    * @see #isInstanceOf(Class, Object)
    * @since 3.0
    */
  def isInstanceOf(`type`: Class[_], obj: Any, message: String, values: Any*): Unit = {
    if (!`type`.isInstance(obj)) throw new IllegalArgumentException(String.format(message, values))
  }

  /**
    * Validates that the argument can be converted to the specified class, if not, throws an exception.
    *
    * <p>This method is useful when validating that there will be no casting errors.</p>
    *
    * <pre>Validate.isAssignableFrom(SuperClass.class, object.getClass());</pre>
    *
    * <p>The message format of the exception is &quot;Cannot assign {type} to {superType}&quot;</p>
    *
    * @param superType the class the class must be validated against, not null
    * @param type      the class to check, not null
    * @throws IllegalArgumentException if type argument is not assignable to the specified superType
    * @see #isAssignableFrom(Class, Class, String, Object...)
    * @since 3.0
    */
  def isAssignableFrom(superType: Class[_], `type`: Class[_]): Unit = { // TODO when breaking BC, consider returning type
    if (!superType.isAssignableFrom(`type`)) throw new IllegalArgumentException(String.format(DEFAULT_IS_ASSIGNABLE_EX_MESSAGE, if (`type` == null) "null"
    else `type`.getName, superType.getName))
  }

  /**
    * Validates that the argument can be converted to the specified class, if not throws an exception.
    *
    * <p>This method is useful when validating if there will be no casting errors.</p>
    *
    * <pre>Validate.isAssignableFrom(SuperClass.class, object.getClass());</pre>
    *
    * <p>The message of the exception is &quot;The validated object can not be converted to the&quot;
    * followed by the name of the class and &quot;class&quot;</p>
    *
    * @param superType the class the class must be validated against, not null
    * @param type      the class to check, not null
    * @param message   the {@link String# format ( String, Object...)} exception message if invalid, not null
    * @param values    the optional values for the formatted exception message, null array not recommended
    * @throws IllegalArgumentException if argument can not be converted to the specified class
    * @see #isAssignableFrom(Class, Class)
    */
  def isAssignableFrom(superType: Class[_], `type`: Class[_], message: String, values: Any*): Unit = {
    if (!superType.isAssignableFrom(`type`)) throw new IllegalArgumentException(String.format(message, values))
  }
}