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

import java.lang.reflect.{Array => ReflectArray}
import java.lang.reflect.Field
import java.lang.reflect.Method
import java.lang.reflect.Type
import java.lang.{Boolean => JavaBoolean, Byte => JavaByte, Double => JavaDouble, Float => JavaFloat, Long => JavaLong, Short => JavaShort}
import java.{util => ju}
import scala.reflect.ClassTag
//import java.util.Comparator
//import java.util.Random
//import org.apache.commons.lang3.builder.EqualsBuilder
import org.apache.commons.lang3.builder.HashCodeBuilder
import org.apache.commons.lang3.builder.ToStringBuilder
import org.apache.commons.lang3.builder.ToStringStyle
//import org.apache.commons.lang3.math.NumberUtils
//import org.apache.commons.lang3.mutable.MutableInt
import scala.util.control.Breaks
//
//
/**
  * <p>Operations on arrays, primitive arrays (like {@code int[]}) and
  * primitive wrapper arrays (like {@code Integer[]}).
  *
  * <p>This class tries to handle {@code null} input gracefully.
  * An exception will not be thrown for a {@code null}
  * array input. However, an Object array that contains a {@code null}
  * element may throw an exception. Each method documents its behavior.
  *
  * <p>#ThreadSafe#
  *
  * @since 2.0
  */
object ArrayUtils {
  /**
    * Our own copy of breaks to avoid conflicts with any other breaks:
    * "Calls to break from one instantiation of Breaks will never target breakable objects of some other instantiation."
    */
  private val breaks: Breaks = new Breaks
  import breaks._

  /**
    * An empty immutable {@code boolean} array.
    */
  val EMPTY_BOOLEAN_ARRAY = new Array[Boolean](0)
  /**
    * An empty immutable {@code Boolean} array.
    */
  val EMPTY_BOOLEAN_OBJECT_ARRAY = new Array[Boolean](0)
  /**
    * An empty immutable {@code byte} array.
    */
  val EMPTY_BYTE_ARRAY = new Array[Byte](0)
  /**
    * An empty immutable {@code Byte} array.
    */
  val EMPTY_BYTE_OBJECT_ARRAY = new Array[Byte](0)
  /**
    * An empty immutable {@code char} array.
    */
  val EMPTY_CHAR_ARRAY = new Array[Char](0)
  /**
    * An empty immutable {@code Character} array.
    */
  val EMPTY_CHARACTER_OBJECT_ARRAY = new Array[Character](0)
  /**
    * An empty immutable {@code Class} array.
    */
  val EMPTY_CLASS_ARRAY = new Array[Class[_]](0)
  /**
    * An empty immutable {@code double} array.
    */
  val EMPTY_DOUBLE_ARRAY = new Array[Double](0)
  /**
    * An empty immutable {@code Double} array.
    */
  val EMPTY_DOUBLE_OBJECT_ARRAY = new Array[Double](0)
  /**
    * An empty immutable {@code Field} array.
    *
    * @since 3.10
    */
  val EMPTY_FIELD_ARRAY = new Array[Field](0)
  /**
    * An empty immutable {@code float} array.
    */
  val EMPTY_FLOAT_ARRAY = new Array[Float](0)
  /**
    * An empty immutable {@code Float} array.
    */
  val EMPTY_FLOAT_OBJECT_ARRAY = new Array[Float](0)
  /**
    * An empty immutable {@code int} array.
    */
  val EMPTY_INT_ARRAY = new Array[Int](0)
  /**
    * An empty immutable {@code Integer} array.
    */
  val EMPTY_INTEGER_OBJECT_ARRAY = new Array[Integer](0)
  /**
    * An empty immutable {@code long} array.
    */
  val EMPTY_LONG_ARRAY = new Array[Long](0)
  /**
    * An empty immutable {@code Long} array.
    */
  val EMPTY_LONG_OBJECT_ARRAY = new Array[Long](0)
  /**
    * An empty immutable {@code Method} array.
    *
    * @since 3.10
    */
  val EMPTY_METHOD_ARRAY: Array[Method] = new Array[Method](0)
  /**
    * An empty immutable {@code Object} array.
    */
  val EMPTY_OBJECT_ARRAY: Array[AnyRef]  = new Array[AnyRef](0)
  /**
    * An empty immutable {@code short} array.
    */
  val EMPTY_SHORT_ARRAY: Array[Short] = new Array[Short](0)
  /**
    * An empty immutable {@code Short} array.
    */
  val EMPTY_SHORT_OBJECT_ARRAY: Array[Short]  = new Array[Short](0)
  /**
    * An empty immutable {@code String} array.
    */
  val EMPTY_STRING_ARRAY: Array[String] = new Array[String](0)
  /**
    * An empty immutable {@code Throwable} array.
    *
    * @since 3.10
    */
  val EMPTY_THROWABLE_ARRAY: Array[Throwable] = new Array[Throwable](0)
  /**
    * An empty immutable {@code Type} array.
    *
    * @since 3.10
    */
  val EMPTY_TYPE_ARRAY: Array[Type] = new Array[Type](0)
  /**
    * The index value when an element is not found in a list or array: {@code -1}.
    * This value is returned by methods in this class and can also be used in comparisons with values returned by
    * various method from {@link java.ju.List}.
    */
  val INDEX_NOT_FOUND: Int = -1

  /**
    * <p>Copies the given array and adds the given element at the end of the new array.
    *
    * <p>The new array contains the same elements of the input
    * array plus the given element in the last position. The component type of
    * the new array is the same as that of the input array.
    *
    * <p>If the input array is {@code null}, a new one element array is returned
    * whose component type is the same as the element.
    *
    * <pre>
    * ArrayUtils.add(null, true)          = [true]
    * ArrayUtils.add([true], false)       = [true, false]
    * ArrayUtils.add([true, false], true) = [true, false, true]
    * </pre>
    *
    * @param array   the array to copy and add the element to, may be {@code null}
    * @param element the object to add at the last index of the new array
    * @return A new array containing the existing elements plus the new element
    * @since 2.1
    */
  def add(array: Array[Boolean], element: Boolean): Array[Boolean] = {
    val newArray = copyArrayGrow1(array, JavaBoolean.TYPE).asInstanceOf[Array[Boolean]]
    newArray(newArray.length - 1) = element
    newArray
  }

  /**
    * <p>Inserts the specified element at the specified position in the array.
    * Shifts the element currently at that position (if any) and any subsequent
    * elements to the right (adds one to their indices).
    *
    * <p>This method returns a new array with the same elements of the input
    * array plus the given element on the specified position. The component
    * type of the returned array is always the same as that of the input
    * array.
    *
    * <p>If the input array is {@code null}, a new one element array is returned
    * whose component type is the same as the element.
    *
    * <pre>
    * ArrayUtils.add(null, 0, true)          = [true]
    * ArrayUtils.add([true], 0, false)       = [false, true]
    * ArrayUtils.add([false], 1, true)       = [false, true]
    * ArrayUtils.add([true, false], 1, true) = [true, true, false]
    * </pre>
    *
    * @param array   the array to add the element to, may be {@code null}
    * @param index   the position of the new object
    * @param element the object to add
    * @return A new array containing the existing elements and the new element
    * @throws IndexOutOfBoundsException if the index is out of range (index &lt; 0 || index &gt; array.length).
    * @deprecated this method has been superseded by {@link #insert ( int, boolean[], boolean...)} and
    *             may be removed in a future release. Please note the handling of {@code null} input arrays differs
    *             in the new method: inserting {@code X} into a {@code null} array results in {@code null} not {@code X}.
    */
  @deprecated def add(array: Array[Boolean], index: Int, element: Boolean): Array[Boolean] =
    add(array, index, JavaBoolean.valueOf(element), JavaBoolean.TYPE).asInstanceOf[Array[Boolean]]

  /**
    * <p>Copies the given array and adds the given element at the end of the new array.
    *
    * <p>The new array contains the same elements of the input
    * array plus the given element in the last position. The component type of
    * the new array is the same as that of the input array.
    *
    * <p>If the input array is {@code null}, a new one element array is returned
    * whose component type is the same as the element.
    *
    * <pre>
    * ArrayUtils.add(null, 0)   = [0]
    * ArrayUtils.add([1], 0)    = [1, 0]
    * ArrayUtils.add([1, 0], 1) = [1, 0, 1]
    * </pre>
    *
    * @param array   the array to copy and add the element to, may be {@code null}
    * @param element the object to add at the last index of the new array
    * @return A new array containing the existing elements plus the new element
    * @since 2.1
    */
  def add(array: Array[Byte], element: Byte): Array[Byte] = {
    val newArray = copyArrayGrow1(array, JavaByte.TYPE).asInstanceOf[Array[Byte]]
    newArray(newArray.length - 1) = element
    newArray
  }

  /**
    * <p>Inserts the specified element at the specified position in the array.
    * Shifts the element currently at that position (if any) and any subsequent
    * elements to the right (adds one to their indices).
    *
    * <p>This method returns a new array with the same elements of the input
    * array plus the given element on the specified position. The component
    * type of the returned array is always the same as that of the input
    * array.
    *
    * <p>If the input array is {@code null}, a new one element array is returned
    * whose component type is the same as the element.
    *
    * <pre>
    * ArrayUtils.add([1], 0, 2)         = [2, 1]
    * ArrayUtils.add([2, 6], 2, 3)      = [2, 6, 3]
    * ArrayUtils.add([2, 6], 0, 1)      = [1, 2, 6]
    * ArrayUtils.add([2, 6, 3], 2, 1)   = [2, 6, 1, 3]
    * </pre>
    *
    * @param array   the array to add the element to, may be {@code null}
    * @param index   the position of the new object
    * @param element the object to add
    * @return A new array containing the existing elements and the new element
    * @throws IndexOutOfBoundsException if the index is out of range
    *                                   (index &lt; 0 || index &gt; array.length).
    * @deprecated this method has been superseded by {@link #insert ( int, byte[], byte...)} and
    *             may be removed in a future release. Please note the handling of {@code null} input arrays differs
    *             in the new method: inserting {@code X} into a {@code null} array results in {@code null} not {@code X}.
    */
  @deprecated def add(array: Array[Byte], index: Int, element: Byte): Array[Byte] =
    add(array, index, JavaByte.valueOf(element), JavaByte.TYPE).asInstanceOf[Array[Byte]]

  /**
    * <p>Copies the given array and adds the given element at the end of the new array.
    *
    * <p>The new array contains the same elements of the input
    * array plus the given element in the last position. The component type of
    * the new array is the same as that of the input array.
    *
    * <p>If the input array is {@code null}, a new one element array is returned
    * whose component type is the same as the element.
    *
    * <pre>
    * ArrayUtils.add(null, '0')       = ['0']
    * ArrayUtils.add(['1'], '0')      = ['1', '0']
    * ArrayUtils.add(['1', '0'], '1') = ['1', '0', '1']
    * </pre>
    *
    * @param array   the array to copy and add the element to, may be {@code null}
    * @param element the object to add at the last index of the new array
    * @return A new array containing the existing elements plus the new element
    * @since 2.1
    */
  def add(array: Array[Char], element: Char): Array[Char] = {
    val newArray = copyArrayGrow1(array, Character.TYPE).asInstanceOf[Array[Char]]
    newArray(newArray.length - 1) = element
    newArray
  }

  /**
    * <p>Inserts the specified element at the specified position in the array.
    * Shifts the element currently at that position (if any) and any subsequent
    * elements to the right (adds one to their indices).
    *
    * <p>This method returns a new array with the same elements of the input
    * array plus the given element on the specified position. The component
    * type of the returned array is always the same as that of the input
    * array.
    *
    * <p>If the input array is {@code null}, a new one element array is returned
    * whose component type is the same as the element.
    *
    * <pre>
    * ArrayUtils.add(null, 0, 'a')            = ['a']
    * ArrayUtils.add(['a'], 0, 'b')           = ['b', 'a']
    * ArrayUtils.add(['a', 'b'], 0, 'c')      = ['c', 'a', 'b']
    * ArrayUtils.add(['a', 'b'], 1, 'k')      = ['a', 'k', 'b']
    * ArrayUtils.add(['a', 'b', 'c'], 1, 't') = ['a', 't', 'b', 'c']
    * </pre>
    *
    * @param array   the array to add the element to, may be {@code null}
    * @param index   the position of the new object
    * @param element the object to add
    * @return A new array containing the existing elements and the new element
    * @throws IndexOutOfBoundsException if the index is out of range
    *                                   (index &lt; 0 || index &gt; array.length).
    * @deprecated this method has been superseded by {@link #insert ( int, char[], char...)} and
    *             may be removed in a future release. Please note the handling of {@code null} input arrays differs
    *             in the new method: inserting {@code X} into a {@code null} array results in {@code null} not {@code X}.
    */
  @deprecated def add(array: Array[Char], index: Int, element: Char): Array[Char] =
    add(array, index, Character.valueOf(element), Character.TYPE).asInstanceOf[Array[Char]]

  def add(array: Array[Double], element: Double): Array[Double] = {
    val newArray = copyArrayGrow1(array, JavaDouble.TYPE).asInstanceOf[Array[Double]]
    newArray(newArray.length - 1) = element
    newArray
  }

  /**
    * <p>Inserts the specified element at the specified position in the array.
    * Shifts the element currently at that position (if any) and any subsequent
    * elements to the right (adds one to their indices).
    *
    * <p>This method returns a new array with the same elements of the input
    * array plus the given element on the specified position. The component
    * type of the returned array is always the same as that of the input
    * array.
    *
    * <p>If the input array is {@code null}, a new one element array is returned
    * whose component type is the same as the element.
    *
    * <pre>
    * ArrayUtils.add([1.1], 0, 2.2)              = [2.2, 1.1]
    * ArrayUtils.add([2.3, 6.4], 2, 10.5)        = [2.3, 6.4, 10.5]
    * ArrayUtils.add([2.6, 6.7], 0, -4.8)        = [-4.8, 2.6, 6.7]
    * ArrayUtils.add([2.9, 6.0, 0.3], 2, 1.0)    = [2.9, 6.0, 1.0, 0.3]
    * </pre>
    *
    * @param array   the array to add the element to, may be {@code null}
    * @param index   the position of the new object
    * @param element the object to add
    * @return A new array containing the existing elements and the new element
    * @throws IndexOutOfBoundsException if the index is out of range
    *                                   (index &lt; 0 || index &gt; array.length).
    * @deprecated this method has been superseded by {@link #insert ( int, double[], double...)} and
    *             may be removed in a future release. Please note the handling of {@code null} input arrays differs
    *             in the new method: inserting {@code X} into a {@code null} array results in {@code null} not {@code X}.
    */
  @deprecated def add(array: Array[Double], index: Int, element: Double): Array[Double] =
    add(array, index, JavaDouble.valueOf(element), JavaDouble.TYPE).asInstanceOf[Array[Double]]

  def add(array: Array[Float], element: Float): Array[Float] = {
    val newArray = copyArrayGrow1(array, JavaFloat.TYPE).asInstanceOf[Array[Float]]
    newArray(newArray.length - 1) = element
    newArray
  }

  /**
    * <p>Inserts the specified element at the specified position in the array.
    * Shifts the element currently at that position (if any) and any subsequent
    * elements to the right (adds one to their indices).
    *
    * <p>This method returns a new array with the same elements of the input
    * array plus the given element on the specified position. The component
    * type of the returned array is always the same as that of the input
    * array.
    *
    * <p>If the input array is {@code null}, a new one element array is returned
    * whose component type is the same as the element.
    *
    * <pre>
    * ArrayUtils.add([1.1f], 0, 2.2f)               = [2.2f, 1.1f]
    * ArrayUtils.add([2.3f, 6.4f], 2, 10.5f)        = [2.3f, 6.4f, 10.5f]
    * ArrayUtils.add([2.6f, 6.7f], 0, -4.8f)        = [-4.8f, 2.6f, 6.7f]
    * ArrayUtils.add([2.9f, 6.0f, 0.3f], 2, 1.0f)   = [2.9f, 6.0f, 1.0f, 0.3f]
    * </pre>
    *
    * @param array   the array to add the element to, may be {@code null}
    * @param index   the position of the new object
    * @param element the object to add
    * @return A new array containing the existing elements and the new element
    * @throws IndexOutOfBoundsException if the index is out of range
    *                                   (index &lt; 0 || index &gt; array.length).
    * @deprecated this method has been superseded by {@link #insert ( int, float[], float...)} and
    *             may be removed in a future release. Please note the handling of {@code null} input arrays differs
    *             in the new method: inserting {@code X} into a {@code null} array results in {@code null} not {@code X}.
    */
  @deprecated def add(array: Array[Float], index: Int, element: Float): Array[Float] =
    add(array, index, JavaFloat.valueOf(element), JavaFloat.TYPE).asInstanceOf[Array[Float]]

  def add(array: Array[Int], element: Int): Array[Int] = {
    val newArray = copyArrayGrow1(array, Integer.TYPE).asInstanceOf[Array[Int]]
    newArray(newArray.length - 1) = element
    newArray
  }

  /**
    * <p>Inserts the specified element at the specified position in the array.
    * Shifts the element currently at that position (if any) and any subsequent
    * elements to the right (adds one to their indices).
    *
    * <p>This method returns a new array with the same elements of the input
    * array plus the given element on the specified position. The component
    * type of the returned array is always the same as that of the input
    * array.
    *
    * <p>If the input array is {@code null}, a new one element array is returned
    * whose component type is the same as the element.
    *
    * <pre>
    * ArrayUtils.add([1], 0, 2)         = [2, 1]
    * ArrayUtils.add([2, 6], 2, 10)     = [2, 6, 10]
    * ArrayUtils.add([2, 6], 0, -4)     = [-4, 2, 6]
    * ArrayUtils.add([2, 6, 3], 2, 1)   = [2, 6, 1, 3]
    * </pre>
    *
    * @param array   the array to add the element to, may be {@code null}
    * @param index   the position of the new object
    * @param element the object to add
    * @return A new array containing the existing elements and the new element
    * @throws IndexOutOfBoundsException if the index is out of range
    *                                   (index &lt; 0 || index &gt; array.length).
    * @deprecated this method has been superseded by {@link #insert ( int, int[], int...)} and
    *             may be removed in a future release. Please note the handling of {@code null} input arrays differs
    *             in the new method: inserting {@code X} into a {@code null} array results in {@code null} not {@code X}.
    */
  @deprecated def add(array: Array[Int], index: Int, element: Int): Array[Int] =
    add(array, index, Integer.valueOf(element), Integer.TYPE).asInstanceOf[Array[Int]]

  /**
    * <p>Inserts the specified element at the specified position in the array.
    * Shifts the element currently at that position (if any) and any subsequent
    * elements to the right (adds one to their indices).
    *
    * <p>This method returns a new array with the same elements of the input
    * array plus the given element on the specified position. The component
    * type of the returned array is always the same as that of the input
    * array.
    *
    * <p>If the input array is {@code null}, a new one element array is returned
    * whose component type is the same as the element.
    *
    * <pre>
    * ArrayUtils.add([1L], 0, 2L)           = [2L, 1L]
    * ArrayUtils.add([2L, 6L], 2, 10L)      = [2L, 6L, 10L]
    * ArrayUtils.add([2L, 6L], 0, -4L)      = [-4L, 2L, 6L]
    * ArrayUtils.add([2L, 6L, 3L], 2, 1L)   = [2L, 6L, 1L, 3L]
    * </pre>
    *
    * @param array   the array to add the element to, may be {@code null}
    * @param index   the position of the new object
    * @param element the object to add
    * @return A new array containing the existing elements and the new element
    * @throws IndexOutOfBoundsException if the index is out of range
    *                                   (index &lt; 0 || index &gt; array.length).
    * @deprecated this method has been superseded by {@link #insert ( int, long[], long...)} and
    *             may be removed in a future release. Please note the handling of {@code null} input arrays differs
    *             in the new method: inserting {@code X} into a {@code null} array results in {@code null} not {@code X}.
    */
  @deprecated def add(array: Array[Long], index: Int, element: Long): Array[Long] =
    add(array, index, JavaLong.valueOf(element), JavaLong.TYPE).asInstanceOf[Array[Long]]

  def add(array: Array[Long], element: Long): Array[Long] = {
    val newArray = copyArrayGrow1(array, JavaLong.TYPE).asInstanceOf[Array[Long]]
    newArray(newArray.length - 1) = element
    newArray
  }

  /**
    * Underlying implementation of add(array, index, element) methods.
    * The last parameter is the class, which may not equal element.getClass
    * for primitives.
    *
    * @param array   the array to add the element to, may be {@code null}
    * @param index   the position of the new object
    * @param element the object to add
    * @param clss    the type of the element being added
    * @return A new array containing the existing elements and the new element
    */
  private def add(array: Any, index: Int, element: Any, clss: Class[_]): Any = {
    if (array == null) {
      if (index != 0) throw new IndexOutOfBoundsException("Index: " + index + ", Length: 0")
      val joinedArray = ReflectArray.newInstance(clss, 1)
      ReflectArray.set(joinedArray, 0, element)
      return joinedArray
    }

    val length = ReflectArray.getLength(array)
    if (index > length || index < 0) throw new IndexOutOfBoundsException("Index: " + index + ", Length: " + length)
    val result = ReflectArray.newInstance(clss, length + 1)
    System.arraycopy(array, 0, result, 0, index)
    ReflectArray.set(result, index, element)

    if (index < length) System.arraycopy(array, index, result, index + 1, length - index)
    result
  }

  /**
    * <p>Inserts the specified element at the specified position in the array.
    * Shifts the element currently at that position (if any) and any subsequent
    * elements to the right (adds one to their indices).
    *
    * <p>This method returns a new array with the same elements of the input
    * array plus the given element on the specified position. The component
    * type of the returned array is always the same as that of the input
    * array.
    *
    * <p>If the input array is {@code null}, a new one element array is returned
    * whose component type is the same as the element.
    *
    * <pre>
    * ArrayUtils.add([1], 0, 2)         = [2, 1]
    * ArrayUtils.add([2, 6], 2, 10)     = [2, 6, 10]
    * ArrayUtils.add([2, 6], 0, -4)     = [-4, 2, 6]
    * ArrayUtils.add([2, 6, 3], 2, 1)   = [2, 6, 1, 3]
    * </pre>
    *
    * @param array   the array to add the element to, may be {@code null}
    * @param index   the position of the new object
    * @param element the object to add
    * @return A new array containing the existing elements and the new element
    * @throws IndexOutOfBoundsException if the index is out of range
    *                                   (index &lt; 0 || index &gt; array.length).
    * @deprecated this method has been superseded by {@link #insert ( int, short[], short...)} and
    *             may be removed in a future release. Please note the handling of {@code null} input arrays differs
    *             in the new method: inserting {@code X} into a {@code null} array results in {@code null} not {@code X}.
    */
  @deprecated def add(array: Array[Short], index: Int, element: Short): Array[Short] =
    add(array, index, JavaShort.valueOf(element), JavaShort.TYPE).asInstanceOf[Array[Short]]

  def add(array: Array[Short], element: Short): Array[Short] = {
    val newArray = copyArrayGrow1(array, JavaShort.TYPE).asInstanceOf[Array[Short]]
    newArray(newArray.length - 1) = element
    newArray
  }

  /**
    * <p>Inserts the specified element at the specified position in the array.
    * Shifts the element currently at that position (if any) and any subsequent
    * elements to the right (adds one to their indices).
    *
    * <p>This method returns a new array with the same elements of the input
    * array plus the given element on the specified position. The component
    * type of the returned array is always the same as that of the input
    * array.
    *
    * <p>If the input array is {@code null}, a new one element array is returned
    * whose component type is the same as the element.
    *
    * <pre>
    * ArrayUtils.add(null, 0, null)      = IllegalArgumentException
    * ArrayUtils.add(null, 0, "a")       = ["a"]
    * ArrayUtils.add(["a"], 1, null)     = ["a", null]
    * ArrayUtils.add(["a"], 1, "b")      = ["a", "b"]
    * ArrayUtils.add(["a", "b"], 3, "c") = ["a", "b", "c"]
    * </pre>
    *
    * @tparam T      the component type of the array
    * @param array   the array to add the element to, may be {@code null}
    * @param index   the position of the new object
    * @param element the object to add
    * @return A new array containing the existing elements and the new element
    * @throws IndexOutOfBoundsException if the index is out of range (index &lt; 0 || index &gt; array.length).
    * @throws IllegalArgumentException  if both array and element are null
    * @deprecated this method has been superseded by {@link #insert ( int, Object[], Object...) insert(int, T[], T...)} and
    *             may be removed in a future release. Please note the handling of {@code null} input arrays differs
    *             in the new method: inserting {@code X} into a {@code null} array results in {@code null} not {@code X}.
    */
  @deprecated def add[T](array: Array[T], index: Int, element: T): Array[T] = {
    var clss: Class[_] = null
    if (array != null) clss = array.getClass.getComponentType
    else if (element != null) clss = element.getClass
    else throw new IllegalArgumentException("Array and element cannot both be null")

    @SuppressWarnings(Array("unchecked")) // the add method creates an array of type clss, which is type T
    val newArray: Array[T] = add(array, index, element, clss).asInstanceOf[Array[T]]
    newArray
  }

  /**
    * <p>Copies the given array and adds the given element at the end of the new array.
    *
    * <p>The new array contains the same elements of the input
    * array plus the given element in the last position. The component type of
    * the new array is the same as that of the input array.
    *
    * <p>If the input array is {@code null}, a new one element array is returned
    * whose component type is the same as the element, unless the element itself is null,
    * in which case the return type is Object[]
    *
    * <pre>
    * ArrayUtils.add(null, null)      = IllegalArgumentException
    * ArrayUtils.add(null, "a")       = ["a"]
    * ArrayUtils.add(["a"], null)     = ["a", null]
    * ArrayUtils.add(["a"], "b")      = ["a", "b"]
    * ArrayUtils.add(["a", "b"], "c") = ["a", "b", "c"]
    * </pre>
    *
    * @tparam T      the component type of the array
    * @param array   the array to "add" the element to, may be {@code null}
    * @param element the object to add, may be {@code null}
    * @return A new array containing the existing elements plus the new element
    *         The returned array type will be that of the input array (unless null),
    *         in which case it will have the same type as the element.
    *         If both are null, an IllegalArgumentException is thrown
    * @since 2.1
    * @throws IllegalArgumentException if both arguments are null
    */
  def add[T](array: Array[T], element: T): Array[T] = {
    var `type`: Class[_] = null
    if (array != null) `type` = array.getClass.getComponentType
    else if (element != null) `type` = element.getClass
    else throw new IllegalArgumentException("Arguments cannot both be null")
    @SuppressWarnings(Array("unchecked")) // type must be T
    val newArray: Array[T] = copyArrayGrow1(array, `type`).asInstanceOf[Array[T]]
    newArray(newArray.length - 1) = element
    newArray
  }

  /**
    * <p>Adds all the elements of the given arrays into a new array.
    * <p>The new array contains all of the element of {@code array1} followed
    * by all of the elements {@code array2}. When an array is returned, it is always
    * a new array.
    *
    * <pre>
    * ArrayUtils.addAll(array1, null)   = cloned copy of array1
    * ArrayUtils.addAll(null, array2)   = cloned copy of array2
    * ArrayUtils.addAll([], [])         = []
    * </pre>
    *
    * @param array1 the first array whose elements are added to the new array.
    * @param array2 the second array whose elements are added to the new array.
    * @return The new boolean[] array.
    * @since 2.1
    */
  def addAll(array1: Array[Boolean], array2: Boolean*): Array[Boolean] = {
    if (isEmpty(array1)) array2.toArray
    else if (array2.isEmpty) array1.clone()
    else {
      val joinedArray = new Array[Boolean](array1.length + array2.length)
      System.arraycopy(array1, 0, joinedArray, 0, array1.length)
      System.arraycopy(array2, 0, joinedArray, array1.length, array2.length)
      joinedArray
    }
  }

  /**
    * <p>Adds all the elements of the given arrays into a new array.
    * <p>The new array contains all of the element of {@code array1} followed
    * by all of the elements {@code array2}. When an array is returned, it is always
    * a new array.
    *
    * <pre>
    * ArrayUtils.addAll(array1, null)   = cloned copy of array1
    * ArrayUtils.addAll(null, array2)   = cloned copy of array2
    * ArrayUtils.addAll([], [])         = []
    * </pre>
    *
    * @param array1 the first array whose elements are added to the new array.
    * @param array2 the second array whose elements are added to the new array.
    * @return The new byte[] array.
    * @since 2.1
    */
  def addAll(array1: Array[Byte], array2: Byte*): Array[Byte] = {
    if (isEmpty(array1)) array2.toArray
    else if (array2.isEmpty) array1.clone
    else {
      val joinedArray = new Array[Byte](array1.length + array2.length)
      System.arraycopy(array1, 0, joinedArray, 0, array1.length)
      System.arraycopy(array2, 0, joinedArray, array1.length, array2.length)
      joinedArray
    }
  }

  /**
    * <p>Adds all the elements of the given arrays into a new array.
    * <p>The new array contains all of the element of {@code array1} followed
    * by all of the elements {@code array2}. When an array is returned, it is always
    * a new array.
    *
    * <pre>
    * ArrayUtils.addAll(array1, null)   = cloned copy of array1
    * ArrayUtils.addAll(null, array2)   = cloned copy of array2
    * ArrayUtils.addAll([], [])         = []
    * </pre>
    *
    * @param array1 the first array whose elements are added to the new array.
    * @param array2 the second array whose elements are added to the new array.
    * @return The new char[] array.
    * @since 2.1
    */
  def addAll(array1: Array[Char], array2: Char*): Array[Char] = {
    if (isEmpty(array1)) array2.toArray
    else if (array2.isEmpty) return array1.clone()
    else {
      val joinedArray = new Array[Char](array1.length + array2.length)
      System.arraycopy(array1, 0, joinedArray, 0, array1.length)
      System.arraycopy(array2, 0, joinedArray, array1.length, array2.length)
      joinedArray
    }
  }

  /**
    * <p>Adds all the elements of the given arrays into a new array.
    * <p>The new array contains all of the element of {@code array1} followed
    * by all of the elements {@code array2}. When an array is returned, it is always
    * a new array.
    *
    * <pre>
    * ArrayUtils.addAll(array1, null)   = cloned copy of array1
    * ArrayUtils.addAll(null, array2)   = cloned copy of array2
    * ArrayUtils.addAll([], [])         = []
    * </pre>
    *
    * @param array1 the first array whose elements are added to the new array.
    * @param array2 the second array whose elements are added to the new array.
    * @return The new double[] array.
    * @since 2.1
    */
  def addAll(array1: Array[Double], array2: Double*): Array[Double] = {
    if (isEmpty(array1)) array2.toArray
    else if (array2.isEmpty) array1.clone()
    else {
      val joinedArray = new Array[Double](array1.length + array2.length)
      System.arraycopy(array1, 0, joinedArray, 0, array1.length)
      System.arraycopy(array2, 0, joinedArray, array1.length, array2.length)
      joinedArray
    }
  }

  /**
    * <p>Adds all the elements of the given arrays into a new array.
    * <p>The new array contains all of the element of {@code array1} followed
    * by all of the elements {@code array2}. When an array is returned, it is always
    * a new array.
    *
    * <pre>
    * ArrayUtils.addAll(array1, null)   = cloned copy of array1
    * ArrayUtils.addAll(null, array2)   = cloned copy of array2
    * ArrayUtils.addAll([], [])         = []
    * </pre>
    *
    * @param array1 the first array whose elements are added to the new array.
    * @param array2 the second array whose elements are added to the new array.
    * @return The new float[] array.
    * @since 2.1
    */
  def addAll(array1: Array[Float], array2: Float*): Array[Float] = {
    if (isEmpty(array1)) array2.toArray
    else if (array2.isEmpty) array1.clone()
    else {
      val joinedArray = new Array[Float](array1.length + array2.length)
      System.arraycopy(array1, 0, joinedArray, 0, array1.length)
      System.arraycopy(array2, 0, joinedArray, array1.length, array2.length)
      joinedArray
    }
  }

  /**
    * <p>Adds all the elements of the given arrays into a new array.
    * <p>The new array contains all of the element of {@code array1} followed
    * by all of the elements {@code array2}. When an array is returned, it is always
    * a new array.
    *
    * <pre>
    * ArrayUtils.addAll(array1, null)   = cloned copy of array1
    * ArrayUtils.addAll(null, array2)   = cloned copy of array2
    * ArrayUtils.addAll([], [])         = []
    * </pre>
    *
    * @param array1 the first array whose elements are added to the new array.
    * @param array2 the second array whose elements are added to the new array.
    * @return The new int[] array.
    * @since 2.1
    */
  def addAll(array1: Array[Int], array2: Int*): Array[Int] = {
    if (isEmpty(array1)) array2.toArray
    else if (array2.isEmpty) array1.clone()
    else {
      val joinedArray = new Array[Int](array1.length + array2.length)
      System.arraycopy(array1, 0, joinedArray, 0, array1.length)
      System.arraycopy(array2, 0, joinedArray, array1.length, array2.length)
      joinedArray
    }
  }

  /**
    * <p>Adds all the elements of the given arrays into a new array.
    * <p>The new array contains all of the element of {@code array1} followed
    * by all of the elements {@code array2}. When an array is returned, it is always
    * a new array.
    *
    * <pre>
    * ArrayUtils.addAll(array1, null)   = cloned copy of array1
    * ArrayUtils.addAll(null, array2)   = cloned copy of array2
    * ArrayUtils.addAll([], [])         = []
    * </pre>
    *
    * @param array1 the first array whose elements are added to the new array.
    * @param array2 the second array whose elements are added to the new array.
    * @return The new long[] array.
    * @since 2.1
    */
  def addAll(array1: Array[Long], array2: Long*): Array[Long] = {
    if (isEmpty(array1)) array2.toArray
    else if (array2.isEmpty) array1.clone()
    else {
      val joinedArray = new Array[Long](array1.length + array2.length)
      System.arraycopy(array1, 0, joinedArray, 0, array1.length)
      System.arraycopy(array2, 0, joinedArray, array1.length, array2.length)
      joinedArray
    }
  }

  /**
    * <p>Adds all the elements of the given arrays into a new array.
    * <p>The new array contains all of the element of {@code array1} followed
    * by all of the elements {@code array2}. When an array is returned, it is always
    * a new array.
    *
    * <pre>
    * ArrayUtils.addAll(array1, null)   = cloned copy of array1
    * ArrayUtils.addAll(null, array2)   = cloned copy of array2
    * ArrayUtils.addAll([], [])         = []
    * </pre>
    *
    * @param array1 the first array whose elements are added to the new array.
    * @param array2 the second array whose elements are added to the new array.
    * @return The new short[] array.
    * @since 2.1
    */
  def addAll(array1: Array[Short], array2: Short*): Array[Short] = {
    if (array1 == null) return array2.toArray.clone()
    if (array2 == null || array2.isEmpty) return array1.clone()

    val joinedArray = new Array[Short](array1.length + array2.length)
    System.arraycopy(array1, 0, joinedArray, 0, array1.length)
    System.arraycopy(array2, 0, joinedArray, array1.length, array2.length)
    joinedArray
  }

  /**
    * <p>Adds all the elements of the given arrays into a new array.
    * <p>The new array contains all of the element of {@code array1} followed
    * by all of the elements {@code array2}. When an array is returned, it is always
    * a new array.
    *
    * <pre>
    * ArrayUtils.addAll(null, null)     = null
    * ArrayUtils.addAll(array1, null)   = cloned copy of array1
    * ArrayUtils.addAll(null, array2)   = cloned copy of array2
    * ArrayUtils.addAll([], [])         = []
    * ArrayUtils.addAll([null], [null]) = [null, null]
    * ArrayUtils.addAll(["a", "b", "c"], ["1", "2", "3"]) = ["a", "b", "c", "1", "2", "3"]
    * </pre>
    *
    * @tparam T     the component type of the array
    * @param array1 the first array whose elements are added to the new array, may be {@code null}
    * @param array2 the second array whose elements are added to the new array, may be {@code null}
    * @return The new array, {@code null} if both arrays are {@code null}.
    *         The type of the new array is the type of the first array,
    *         unless the first array is null, in which case the type is the same as the second array.
    * @since 2.1
    * @throws IllegalArgumentException if the array types are incompatible
    */
  def addAll[T: ClassTag](array1: Array[T], @SuppressWarnings(Array("unchecked")) array2: T*): Array[T] = {
    if (null == array1 || array1.isEmpty) return array2.toArray
    if (array2.isEmpty) return array1.clone()

    val type1 = array1.getClass.getComponentType
    @SuppressWarnings(Array("unchecked")) // OK, because array is of type T
    val joinedArray: Array[T] = ReflectArray.newInstance(type1, array1.length + array2.length).asInstanceOf[Array[T]]
    System.arraycopy(array1, 0, joinedArray, 0, array1.length)

    try {
      System.arraycopy(array2, 0, joinedArray, array1.length, array2.length)
    } catch {
      case ase: ArrayStoreException =>
        // Check if problem was due to incompatible types
        /*
         * We do this here, rather than before the copy because:
         * - it would be a wasted check most of the time
         * - safer, in case check turns out to be too strict
         */
        val type2 = array2.getClass.getComponentType
        if (!type1.isAssignableFrom(type2)) throw new IllegalArgumentException("Cannot store " + type2.getName + " in an array of " + type1.getName, ase)
        throw ase // No, so rethrow original

    }
    joinedArray
  }
  //
  //  /**
  //    * Copies the given array and adds the given element at the beginning of the new array.
  //    *
  //    * <p>
  //    * The new array contains the same elements of the input array plus the given element in the first position. The
  //    * component type of the new array is the same as that of the input array.
  //    * </p>
  //    *
  //    * <p>
  //    * If the input array is {@code null}, a new one element array is returned whose component type is the same as the
  //    * element.
  //    * </p>
  //    *
  //    * <pre>
  //    * ArrayUtils.add(null, true)          = [true]
  //    * ArrayUtils.add([true], false)       = [false, true]
  //    * ArrayUtils.add([true, false], true) = [true, true, false]
  //    * </pre>
  //    *
  //    * @param array   the array to "add" the element to, may be {@code null}.
  //    * @param element the object to add.
  //    * @return A new array containing the existing elements plus the new element The returned array type will be that of
  //    *         the input array (unless null), in which case it will have the same type as the element.
  //    * @since 3.10
  //    */
  //  def addFirst(array: Array[Boolean], element: Boolean): Array[Boolean] = if (array == null) add(array, element)
  //  else insert(0, array, element)
  //
  //  /**
  //    * Copies the given array and adds the given element at the beginning of the new array.
  //    *
  //    * <p>
  //    * The new array contains the same elements of the input array plus the given element in the first position. The
  //    * component type of the new array is the same as that of the input array.
  //    * </p>
  //    *
  //    * <p>
  //    * If the input array is {@code null}, a new one element array is returned whose component type is the same as the
  //    * element.
  //    * </p>
  //    *
  //    * <pre>
  //    * ArrayUtils.add(null, 1)   = [1]
  //    * ArrayUtils.add([1], 0)    = [0, 1]
  //    * ArrayUtils.add([1, 0], 1) = [1, 1, 0]
  //    * </pre>
  //    *
  //    * @param array   the array to "add" the element to, may be {@code null}.
  //    * @param element the object to add.
  //    * @return A new array containing the existing elements plus the new element The returned array type will be that of
  //    *         the input array (unless null), in which case it will have the same type as the element.
  //    * @since 3.10
  //    */
  //  def addFirst(array: Array[Byte], element: Byte): Array[Byte] = if (array == null) add(array, element)
  //  else insert(0, array, element)
  //
  //  /**
  //    * Copies the given array and adds the given element at the beginning of the new array.
  //    *
  //    * <p>
  //    * The new array contains the same elements of the input array plus the given element in the first position. The
  //    * component type of the new array is the same as that of the input array.
  //    * </p>
  //    *
  //    * <p>
  //    * If the input array is {@code null}, a new one element array is returned whose component type is the same as the
  //    * element.
  //    * </p>
  //    *
  //    * <pre>
  //    * ArrayUtils.add(null, '1')       = ['1']
  //    * ArrayUtils.add(['1'], '0')      = ['0', '1']
  //    * ArrayUtils.add(['1', '0'], '1') = ['1', '1', '0']
  //    * </pre>
  //    *
  //    * @param array   the array to "add" the element to, may be {@code null}.
  //    * @param element the object to add.
  //    * @return A new array containing the existing elements plus the new element The returned array type will be that of
  //    *         the input array (unless null), in which case it will have the same type as the element.
  //    * @since 3.10
  //    */
  //  def addFirst(array: Array[Char], element: Char): Array[Char] = if (array == null) add(array, element)
  //  else insert(0, array, element)
  //
  //  def addFirst(array: Array[Double], element: Double): Array[Double] = if (array == null) add(array, element)
  //  else insert(0, array, element)
  //
  //  def addFirst(array: Array[Float], element: Float): Array[Float] = if (array == null) add(array, element)
  //  else insert(0, array, element)
  //
  //  def addFirst(array: Array[Int], element: Int): Array[Int] = if (array == null) add(array, element)
  //  else insert(0, array, element)
  //
  //  def addFirst(array: Array[Long], element: Long): Array[Long] = if (array == null) add(array, element)
  //  else insert(0, array, element)
  //
  //  def addFirst(array: Array[Short], element: Short): Array[Short] = if (array == null) add(array, element)
  //  else insert(0, array, element)
  //
  //  /**
  //    * Copies the given array and adds the given element at the beginning of the new array.
  //    *
  //    * <p>
  //    * The new array contains the same elements of the input array plus the given element in the first positioaddFirstaddFirstaddFirstn. The
  //    * component type of the new array is the same as that of the input array.
  //    * </p>
  //    *
  //    * <p>
  //    * If the input array is {@code null}, a new one element array is returned whose component type is the same as the
  //    * element, unless the element itself is null, in which case the return type is Object[]
  //    * </p>
  //    *
  //    * <pre>
  //    * ArrayUtils.add(null, null)      = IllegalArgumentException
  //    * ArrayUtils.add(null, "a")       = ["a"]
  //    * ArrayUtils.add(["a"], null)     = [null, "a"]
  //    * ArrayUtils.add(["a"], "b")      = ["b", "a"]
  //    * ArrayUtils.add(["a", "b"], "c") = ["c", "a", "b"]
  //    * </pre>
  //    *
  //    * @tparam T      the component type of the array
  //    * @param array   the array to "add" the element to, may be {@code null}
  //    * @param element the object to add, may be {@code null}
  //    * @return A new array containing the existing elements plus the new element The returned array type will be that of
  //    *         the input array (unless null), in which case it will have the same type as the element. If both are null,
  //    *         an IllegalArgumentException is thrown
  //    * @since 3.10
  //    * @throws IllegalArgumentException if both arguments are null
  //    */
  //  def addFirst[T](array: Array[T], element: T): Array[T] = if (array == null) add(array, element)
  //  else insert(0, array, element)
  //
  /**
    * <p>Clones an array returning a typecast result and handling
    * {@code null}.
    *
    * <p>This method returns {@code null} for a {@code null} input array.
    *
    * @param array the array to clone, may be {@code null}
    * @return the cloned array, {@code null} if {@code null} input
    */
  def clone(array: Array[Boolean]): Array[Boolean] = {
    if (array == null) return null
    array.clone
  }

  def clone(array: Array[Byte]): Array[Byte] = {
    if (array == null) return null
    array.clone
  }

  def clone(array: Array[Char]): Array[Char] = {
    if (array == null) return null
    array.clone
  }

  def clone(array: Array[Double]): Array[Double] = {
    if (array == null) return null
    array.clone
  }

  def clone(array: Array[Float]): Array[Float] = {
    if (array == null) return null
    array.clone
  }

  def clone(array: Array[Int]): Array[Int] = {
    if (array == null) return null
    array.clone
  }

  def clone(array: Array[Long]): Array[Long] = {
    if (array == null) return null
    array.clone
  }

  def clone(array: Array[Short]): Array[Short] = {
    if (array == null) return null
    array.clone
  }

  /**
    * <p>Shallow clones an array returning a typecast result and handling
    * {@code null}.
    *
    * <p>The objects in the array are not cloned, thus there is no special
    * handling for multi-dimensional arrays.
    *
    * <p>This method returns {@code null} for a {@code null} input array.
    *
    * @tparam T    the component type of the array
    * @param array the array to shallow clone, may be {@code null}
    * @return the cloned array, {@code null} if {@code null} input
    */
  def clone[T](array: Array[T]): Array[T] = {
    if (array == null) return null
    array.clone
  }

  /**
    * <p>Checks if the value is in the given array.
    *
    * <p>The method returns {@code false} if a {@code null} array is passed in.
    *
    * @param array       the array to search through
    * @param valueToFind the value to find
    * @return {@code true} if the array contains the object
    */
  def contains(array: Array[Boolean], valueToFind: Boolean): Boolean = indexOf(array, valueToFind) != INDEX_NOT_FOUND

  def contains(array: Array[Byte], valueToFind: Byte): Boolean = indexOf(array, valueToFind) != INDEX_NOT_FOUND

  /**
    * <p>Checks if the value is in the given array.
    *
    * <p>The method returns {@code false} if a {@code null} array is passed in.
    *
    * @param array       the array to search through
    * @param valueToFind the value to find
    * @return {@code true} if the array contains the object
    * @since 2.1
    */
  def contains(array: Array[Char], valueToFind: Char): Boolean = indexOf(array, valueToFind) != INDEX_NOT_FOUND

  def contains(array: Array[Double], valueToFind: Double): Boolean = indexOf(array, valueToFind) != INDEX_NOT_FOUND

  /**
    * <p>Checks if a value falling within the given tolerance is in the
    * given array.  If the array contains a value within the inclusive range
    * defined by (value - tolerance) to (value + tolerance).
    *
    * <p>The method returns {@code false} if a {@code null} array
    * is passed in.
    *
    * @param array       the array to search
    * @param valueToFind the value to find
    * @param tolerance   the array contains the tolerance of the search
    * @return true if value falling within tolerance is in array
    */
  def contains(array: Array[Double], valueToFind: Double, tolerance: Double): Boolean = indexOf(array, valueToFind, 0, tolerance) != INDEX_NOT_FOUND

  def contains(array: Array[Float], valueToFind: Float): Boolean = indexOf(array, valueToFind) != INDEX_NOT_FOUND

  def contains(array: Array[Int], valueToFind: Int): Boolean = indexOf(array, valueToFind) != INDEX_NOT_FOUND

  def contains(array: Array[Long], valueToFind: Long): Boolean = indexOf(array, valueToFind) != INDEX_NOT_FOUND

  /**
    * <p>Checks if the object is in the given array.
    *
    * <p>The method returns {@code false} if a {@code null} array is passed in.
    *
    * @param array        the array to search through
    * @param objectToFind the object to find
    * @return {@code true} if the array contains the object
    */
  def contains[T](array: Array[_ <: T], objectToFind: T): Boolean = indexOf(array, objectToFind) != INDEX_NOT_FOUND

  def contains(array: Array[Short], valueToFind: Short): Boolean = indexOf(array, valueToFind) != INDEX_NOT_FOUND

  /**
    * Returns a copy of the given array of size 1 greater than the argument.
    * The last value of the array is left to the default value.
    *
    * @param array                 The array to copy, must not be {@code null}.
    * @param newArrayComponentType If {@code array} is {@code null}, create a
    *                              size 1 array of this type.
    * @return A new copy of the array of size 1 greater than the input.
    */
  private def copyArrayGrow1(array: Any, newArrayComponentType: Class[_]): Any = {
    if (array != null) {
      val arrayLength = ReflectArray.getLength(array)
      val newArray = ReflectArray.newInstance(array.getClass.getComponentType, arrayLength + 1)
      System.arraycopy(array, 0, newArray, 0, arrayLength)
      return newArray
    }

    ReflectArray.newInstance(newArrayComponentType, 1)
  }

  /**
    * Gets the nTh element of an array or null if the index is out of bounds or the array is null.
    *
    * @tparam T    The type of array elements.
    * @param array The array to index.
    * @param index The index
    * @return the nTh element of an array or null if the index is out of bounds or the array is null.
    * @since 3.11
    */
  def get[T](array: Array[T], index: Int): T =
    get(array, index, null.asInstanceOf[T])

  /**
    * Gets the nTh element of an array or a default value if the index is out of bounds.
    *
    * @tparam T           The type of array elements.
    * @param array        The array to index.
    * @param index        The index
    * @param defaultValue The return value of the given index is out of bounds.
    * @return the nTh element of an array or a default value if the index is out of bounds.
    * @since 3.11
    */
  def get[T](array: Array[T], index: Int, defaultValue: T): T =
    if (isArrayIndexValid(array, index)) array(index)
    else defaultValue

  /**
    * <p>Returns the length of the specified array.
    * This method can deal with {@code Object} arrays and with primitive arrays.
    *
    * <p>If the input array is {@code null}, {@code 0} is returned.
    *
    * <pre>
    * ArrayUtils.getLength(null)            = 0
    * ArrayUtils.getLength([])              = 0
    * ArrayUtils.getLength([null])          = 1
    * ArrayUtils.getLength([true, false])   = 2
    * ArrayUtils.getLength([1, 2, 3])       = 3
    * ArrayUtils.getLength(["a", "b", "c"]) = 3
    * </pre>
    *
    * @param array the array to retrieve the length from, may be null
    * @return The length of the array, or {@code 0} if the array is {@code null}
    * @throws IllegalArgumentException if the object argument is not an array.
    * @since 2.1
    */
  def getLength(array: Any): Int = {
    if (array == null) return 0
    ReflectArray.getLength(array)
  }

  /**
    * <p>Get a hash code for an array handling multi-dimensional arrays correctly.
    *
    * <p>Multi-dimensional primitive arrays are also handled correctly by this method.
    *
    * @param array the array to get a hash code for, {@code null} returns zero
    * @return a hash code for the array
    */
  def hashCode(array: Any): Int = new HashCodeBuilder().append(array).toHashCode

  /**
    * Finds the indices of the given value in the array.
    *
    * <p>This method returns an empty BitSet for a {@code null} input array.</p>
    *
    * @param array       the array to search through for the object, may be {@code null}
    * @param valueToFind the value to find
    * @return a BitSet of all the the indices of the value within the array,
    *         an empty BitSet if not found or {@code null} array input
    * @since 3.10
    */
  def indexesOf(array: Array[Boolean], valueToFind: Boolean): ju.BitSet = indexesOf(array, valueToFind, 0)

  /**
    * Finds the indices of the given value in the array starting at the given index.
    *
    * <p>This method returns an empty BitSet for a {@code null} input array.</p>
    *
    * <p>A negative startIndex is treated as zero. A startIndex larger than the array
    * length will return an empty BitSet ({@code -1}).</p>
    *
    * @param array       the array to search through for the object, may be {@code null}
    * @param valueToFind the value to find
    * @param startIndex  the index to start searching at
    * @return a BitSet of all the indices of the value within the array,
    *         an empty BitSet if not found or {@code null}
    *         array input
    * @since 3.10
    */
  def indexesOf(array: Array[Boolean], valueToFind: Boolean, startIndex: Int): ju.BitSet = {
    val bitSet = new ju.BitSet
    if (array == null) return bitSet

    var startIdx: Int = startIndex
    breakable {
      while (startIdx < array.length) {
        startIdx = indexOf(array, valueToFind, startIdx)
        if (startIdx == INDEX_NOT_FOUND) break()
        bitSet.set(startIdx)
        startIdx += 1
      }
    }

    bitSet
  }

  /**
    * Finds the indices of the given value in the array.
    *
    * <p>This method returns an empty BitSet for a {@code null} input array.</p>
    *
    * @param array       the array to search through for the object, may be {@code null}
    * @param valueToFind the value to find
    * @return a BitSet of all the indices of the value within the array,
    *         an empty BitSet if not found or {@code null} array input
    * @since 3.10
    */
  def indexesOf(array: Array[Byte], valueToFind: Byte): ju.BitSet = indexesOf(array, valueToFind, 0)

  /**
    * Finds the indices of the given value in the array starting at the given index.
    *
    * <p>This method returns an empty BitSet for a {@code null} input array.</p>
    *
    * <p>A negative startIndex is treated as zero. A startIndex larger than the array
    * length will return an empty BitSet.</p>
    *
    * @param array       the array to search through for the object, may be {@code null}
    * @param valueToFind the value to find
    * @param startIndex  the index to start searching at
    * @return a BitSet of all the indices of the value within the array,
    *         an empty BitSet if not found or {@code null} array input
    * @since 3.10
    */
  def indexesOf(array: Array[Byte], valueToFind: Byte, startIndex: Int): ju.BitSet = {
    val bitSet = new ju.BitSet
    if (array == null) return bitSet

    var startIdx: Int = startIndex
    breakable {
      while (startIdx < array.length) {
        startIdx = indexOf(array, valueToFind, startIdx)
        if (startIdx == INDEX_NOT_FOUND) break()
        bitSet.set(startIdx)
        startIdx += 1
      }
    }

    bitSet
  }

  def indexesOf(array: Array[Char], valueToFind: Char): ju.BitSet = indexesOf(array, valueToFind, 0)

  def indexesOf(array: Array[Char], valueToFind: Char, startIndex: Int): ju.BitSet = {
    val bitSet = new ju.BitSet
    if (array == null) return bitSet

    var startIdx: Int = startIndex
    breakable {
      while (startIdx < array.length) {
        startIdx = indexOf(array, valueToFind, startIdx)
        if (startIdx == INDEX_NOT_FOUND) break()
        bitSet.set(startIdx)
        startIdx += 1
      }
    }

    bitSet
  }

  /**
    * Finds the indices of the given value in the array.
    *
    * <p>This method returns empty BitSet for a {@code null} input array.</p>
    *
    * @param array       the array to search through for the object, may be {@code null}
    * @param valueToFind the value to find
    * @return a BitSet of all the indices of the value within the array,
    *         an empty BitSet if not found or {@code null} array input
    * @since 3.10
    */
  def indexesOf(array: Array[Double], valueToFind: Double): ju.BitSet = indexesOf(array, valueToFind, 0)

  /**
    * Finds the indices of the given value within a given tolerance in the array.
    *
    * <p>
    * This method will return all the indices of the value which fall between the region
    * defined by valueToFind - tolerance and valueToFind + tolerance, each time between the nearest integers.
    * </p>
    *
    * <p>This method returns an empty BitSet for a {@code null} input array.</p>
    *
    * @param array       the array to search through for the object, may be {@code null}
    * @param valueToFind the value to find
    * @param tolerance   tolerance of the search
    * @return a BitSet of all the indices of the value within the array,
    *         an empty BitSet if not found or {@code null} array input
    * @since 3.10
    */
  def indexesOf(array: Array[Double], valueToFind: Double, tolerance: Double): ju.BitSet =
    indexesOf(array, valueToFind, 0, tolerance)

  /**
    * Finds the indices of the given value in the array starting at the given index.
    *
    * <p>This method returns an empty BitSet for a {@code null} input array.</p>
    *
    * <p>A negative startIndex is treated as zero. A startIndex larger than the array
    * length will return an empty BitSet.</p>
    *
    * @param array       the array to search through for the object, may be {@code null}
    * @param valueToFind the value to find
    * @param startIndex  the index to start searching at
    * @return a BitSet of the indices of the value within the array,
    *         an empty BitSet if not found or {@code null} array input
    * @since 3.10
    */
  def indexesOf(array: Array[Double], valueToFind: Double, startIndex: Int): ju.BitSet = {
    val bitSet = new ju.BitSet
    if (array == null) return bitSet

    var startIdx: Int = startIndex
    breakable {
      while (startIdx < array.length) {
        startIdx = indexOf(array, valueToFind, startIdx)
        if (startIdx == INDEX_NOT_FOUND) break()
        bitSet.set(startIdx)
        startIdx += 1
      }
    }

    bitSet
  }

  /**
    * Finds the indices of the given value in the array starting at the given index.
    *
    * <p>
    * This method will return the indices of the values which fall between the region
    * defined by valueToFind - tolerance and valueToFind + tolerance, between the nearest integers.
    * </p>
    *
    * <p>This method returns an empty BitSet for a {@code null} input array.</p>
    *
    * <p>A negative startIndex is treated as zero. A startIndex larger than the array
    * length will return an empty BitSet.</p>
    *
    * @param array       the array to search through for the object, may be {@code null}
    * @param valueToFind the value to find
    * @param startIndex  the index to start searching at
    * @param tolerance   tolerance of the search
    * @return a BitSet of the indices of the value within the array,
    *         an empty BitSet if not found or {@code null} array input
    * @since 3.10
    */
  def indexesOf(array: Array[Double], valueToFind: Double, startIndex: Int, tolerance: Double): ju.BitSet = {
    val bitSet = new ju.BitSet
    if (array == null) return bitSet

    var startIdx: Int = startIndex
    breakable {
      while (startIdx < array.length) {
        startIdx = indexOf(array, valueToFind, startIdx, tolerance)
        if (startIdx == INDEX_NOT_FOUND) break()
        bitSet.set(startIdx)
        startIdx += 1
      }
    }

    bitSet
  }

  def indexesOf(array: Array[Float], valueToFind: Float): ju.BitSet =
    indexesOf(array, valueToFind, 0)

  /**
    * Finds the indices of the given value in the array starting at the given index.
    *
    * <p>This method returns an empty BitSet for a {@code null} input array.</p>
    *
    * <p>A negative startIndex is treated as zero. A startIndex larger than the array
    * length will return empty BitSet.</p>
    *
    * @param array       the array to search through for the object, may be {@code null}
    * @param valueToFind the value to find
    * @param startIndex  the index to start searching at
    * @return a BitSet of all the indices of the value within the array,
    *         an empty BitSet if not found or {@code null} array input
    * @since 3.10
    */
  def indexesOf(array: Array[Float], valueToFind: Float, startIndex: Int): ju.BitSet = {
    val bitSet = new ju.BitSet
    if (array == null) return bitSet

    var startIdx: Int = startIndex
    breakable {
      while (startIdx < array.length) {
        startIdx = indexOf(array, valueToFind, startIdx)
        if (startIdx == INDEX_NOT_FOUND) break()
        bitSet.set(startIdx)
        startIdx += 1
      }
    }

    bitSet
  }

  def indexesOf(array: Array[Int], valueToFind: Int): ju.BitSet = indexesOf(array, valueToFind, 0)

  def indexesOf(array: Array[Int], valueToFind: Int, startIndex: Int): ju.BitSet = {
    val bitSet = new ju.BitSet
    if (array == null) return bitSet

    var startIdx: Int = startIndex
    breakable {
      while (startIdx < array.length) {
        startIdx = indexOf(array, valueToFind, startIdx)
        if (startIdx == INDEX_NOT_FOUND) break()
        bitSet.set(startIdx)
        startIdx += 1
      }
    }

    bitSet
  }

  def indexesOf(array: Array[Long], valueToFind: Long): ju.BitSet = indexesOf(array, valueToFind, 0)

  def indexesOf(array: Array[Long], valueToFind: Long, startIndex: Int): ju.BitSet = {
    val bitSet = new ju.BitSet
    if (array == null) return bitSet

    var startIdx: Int = startIndex
    breakable {
      while (startIdx < array.length) {
        startIdx = indexOf(array, valueToFind, startIdx)
        if (startIdx == INDEX_NOT_FOUND) break()
        bitSet.set(startIdx)
        startIdx += 1
      }
    }

    bitSet
  }

  /**
    * Finds the indices of the given object in the array.
    *
    * <p>This method returns an empty BitSet for a {@code null} input array.</p>
    *
    * @param array        the array to search through for the object, may be {@code null}
    * @param objectToFind the object to find, may be {@code null}
    * @return a BitSet of all the indices of the object within the array,
    *         an empty BitSet if not found or {@code null} array input
    * @since 3.10
    */
  def indexesOf(array: Array[AnyRef], objectToFind: Any): ju.BitSet = indexesOf(array, objectToFind, 0)

  /**
    * Finds the indices of the given object in the array starting at the given index.
    *
    * <p>This method returns an empty BitSet for a {@code null} input array.</p>
    *
    * <p>A negative startIndex is treated as zero. A startIndex larger than the array
    * length will return an empty BitSet.</p>
    *
    * @param array        the array to search through for the object, may be {@code null}
    * @param objectToFind the object to find, may be {@code null}
    * @param startIndex   the index to start searching at
    * @return a BitSet of all the indices of the object within the array starting at the index,
    *         an empty BitSet if not found or {@code null} array input
    * @since 3.10
    */
  def indexesOf(array: Array[_], objectToFind: Any, startIndex: Int): ju.BitSet = {
    val bitSet = new ju.BitSet
    if (array == null) return bitSet

    var startIdx: Int = startIndex
    breakable {
      while (startIdx < array.length) {
        startIdx = indexOf(array, objectToFind, startIdx)
        if (startIdx == INDEX_NOT_FOUND) break()
        bitSet.set(startIdx)
        startIdx += 1
      }
    }

    bitSet
  }

  def indexesOf(array: Array[Short], valueToFind: Short): ju.BitSet = indexesOf(array, valueToFind, 0)

  def indexesOf(array: Array[Short], valueToFind: Short, startIndex: Int): ju.BitSet = {
    val bitSet = new ju.BitSet
    if (array == null) return bitSet

    var startIdx: Int = startIndex
    breakable {
      while (startIdx < array.length) {
        startIdx = indexOf(array, valueToFind, startIdx)
        if (startIdx == INDEX_NOT_FOUND) break()
        bitSet.set(startIdx)
        startIdx += 1
      }
    }

    bitSet
  }

  /**
    * <p>Finds the index of the given value in the array.
    *
    * <p>This method returns {@link #INDEX_NOT_FOUND} ({@code -1}) for a {@code null} input array.
    *
    * @param array       the array to search through for the object, may be {@code null}
    * @param valueToFind the value to find
    * @return the index of the value within the array,
    *         {@link #INDEX_NOT_FOUND} ({@code -1}) if not found or {@code null} array input
    */
  def indexOf(array: Array[Boolean], valueToFind: Boolean): Int = indexOf(array, valueToFind, 0)

  /**
    * <p>Finds the index of the given value in the array starting at the given index.
    *
    * <p>This method returns {@link #INDEX_NOT_FOUND} ({@code -1}) for a {@code null} input array.
    *
    * <p>A negative startIndex is treated as zero. A startIndex larger than the array
    * length will return {@link #INDEX_NOT_FOUND} ({@code -1}).
    *
    * @param array       the array to search through for the object, may be {@code null}
    * @param valueToFind the value to find
    * @param startIndex  the index to start searching at
    * @return the index of the value within the array,
    *         {@link #INDEX_NOT_FOUND} ({@code -1}) if not found or {@code null}
    *         array input
    */
  def indexOf(array: Array[Boolean], valueToFind: Boolean, startIndex: Int): Int = {
    if (isEmpty(array)) return INDEX_NOT_FOUND
    @inline def start: Int = if (startIndex < 0) 0 else startIndex

    for (i <- start until array.length) {
      if (valueToFind == array(i)) return i
    }
    INDEX_NOT_FOUND
  }

  def indexOf(array: Array[Byte], valueToFind: Byte): Int = indexOf(array, valueToFind, 0)

  /**
    * <p>Finds the index of the given value in the array starting at the given index.
    *
    * <p>This method returns {@link #INDEX_NOT_FOUND} ({@code -1}) for a {@code null} input array.
    *
    * <p>A negative startIndex is treated as zero. A startIndex larger than the array
    * length will return {@link #INDEX_NOT_FOUND} ({@code -1}).
    *
    * @param array       the array to search through for the object, may be {@code null}
    * @param valueToFind the value to find
    * @param startIndex  the index to start searching at
    * @return the index of the value within the array,
    *         {@link #INDEX_NOT_FOUND} ({@code -1}) if not found or {@code null} array input
    */
  def indexOf(array: Array[Byte], valueToFind: Byte, startIndex: Int): Int = {
    if (array == null) return INDEX_NOT_FOUND
    @inline def start: Int = if (startIndex < 0) 0 else startIndex

    for (i <- start until array.length) {
      if (valueToFind == array(i)) return i
    }
    INDEX_NOT_FOUND
  }

  /**
    * <p>Finds the index of the given value in the array.
    *
    * <p>This method returns {@link #INDEX_NOT_FOUND} ({@code -1}) for a {@code null} input array.
    *
    * @param array       the array to search through for the object, may be {@code null}
    * @param valueToFind the value to find
    * @return the index of the value within the array,
    *         {@link #INDEX_NOT_FOUND} ({@code -1}) if not found or {@code null} array input
    * @since 2.1
    */
  def indexOf(array: Array[Char], valueToFind: Char): Int = indexOf(array, valueToFind, 0)

  /**
    * <p>Finds the index of the given value in the array starting at the given index.
    *
    * <p>This method returns {@link #INDEX_NOT_FOUND} ({@code -1}) for a {@code null} input array.
    *
    * <p>A negative startIndex is treated as zero. A startIndex larger than the array
    * length will return {@link #INDEX_NOT_FOUND} ({@code -1}).
    *
    * @param array       the array to search through for the object, may be {@code null}
    * @param valueToFind the value to find
    * @param startIndex  the index to start searching at
    * @return the index of the value within the array,
    *         {@link #INDEX_NOT_FOUND} ({@code -1}) if not found or {@code null} array input
    * @since 2.1
    */
  def indexOf(array: Array[Char], valueToFind: Char, startIndex: Int): Int = {
    if (array == null) return INDEX_NOT_FOUND
    @inline def start: Int = if (startIndex < 0) 0 else startIndex

    for (i <- start until array.length) {
      if (valueToFind == array(i)) return i
    }
    INDEX_NOT_FOUND
  }

  def indexOf(array: Array[Double], valueToFind: Double): Int = indexOf(array, valueToFind, 0)

  /**
    * <p>Finds the index of the given value within a given tolerance in the array.
    * This method will return the index of the first value which falls between the region
    * defined by valueToFind - tolerance and valueToFind + tolerance.
    *
    * <p>This method returns {@link #INDEX_NOT_FOUND} ({@code -1}) for a {@code null} input array.
    *
    * @param array       the array to search through for the object, may be {@code null}
    * @param valueToFind the value to find
    * @param tolerance   tolerance of the search
    * @return the index of the value within the array,
    *         {@link #INDEX_NOT_FOUND} ({@code -1}) if not found or {@code null} array input
    */
  def indexOf(array: Array[Double], valueToFind: Double, tolerance: Double): Int = indexOf(array, valueToFind, 0, tolerance)

  def indexOf(array: Array[Double], valueToFind: Double, startIndex: Int): Int = {
    if (isEmpty(array)) return INDEX_NOT_FOUND
    @inline def start: Int = if (startIndex < 0) 0 else startIndex

    for (i <- start until array.length) {
      if (valueToFind == array(i)) return i
    }
    INDEX_NOT_FOUND
  }

  /**
    * <p>Finds the index of the given value in the array starting at the given index.
    * This method will return the index of the first value which falls between the region
    * defined by valueToFind - tolerance and valueToFind + tolerance.
    *
    * <p>This method returns {@link #INDEX_NOT_FOUND} ({@code -1}) for a {@code null} input array.
    *
    * <p>A negative startIndex is treated as zero. A startIndex larger than the array
    * length will return {@link #INDEX_NOT_FOUND} ({@code -1}).
    *
    * @param array       the array to search through for the object, may be {@code null}
    * @param valueToFind the value to find
    * @param startIndex  the index to start searching at
    * @param tolerance   tolerance of the search
    * @return the index of the value within the array,
    *         {@link #INDEX_NOT_FOUND} ({@code -1}) if not found or {@code null} array input
    */
  def indexOf(array: Array[Double], valueToFind: Double, startIndex: Int, tolerance: Double): Int = {
    if (isEmpty(array)) return INDEX_NOT_FOUND
    @inline def start: Int = if (startIndex < 0) 0 else startIndex

    val min = valueToFind - tolerance
    val max = valueToFind + tolerance
    for (i <- start until array.length) {
      if (array(i) >= min && array(i) <= max) return i
    }
    INDEX_NOT_FOUND
  }

  def indexOf(array: Array[Float], valueToFind: Float): Int = indexOf(array, valueToFind, 0)

  def indexOf(array: Array[Float], valueToFind: Float, startIndex: Int): Int = {
    if (isEmpty(array)) return INDEX_NOT_FOUND

    @inline def start: Int = if (startIndex < 0) 0 else startIndex
    for (i <- start until array.length) {
      if (valueToFind == array(i)) return i
    }
    INDEX_NOT_FOUND
  }

  /**
    * <p>Finds the index of the given value in the array.
    *
    * <p>This method returns {@link #INDEX_NOT_FOUND} ({@code -1}) for a {@code null} input array.
    *
    * @param array       the array to search through for the object, may be {@code null}
    * @param valueToFind the value to find
    * @return the index of the value within the array,
    *         {@link #INDEX_NOT_FOUND} ({@code -1}) if not found or {@code null} array input
    */
  def indexOf(array: Array[Int], valueToFind: Int): Int = indexOf(array, valueToFind, 0)

  def indexOf(array: Array[Int], valueToFind: Int, startIndex: Int): Int = {
    if (array == null) return INDEX_NOT_FOUND
    @inline def start: Int = if (startIndex < 0) 0 else startIndex

    for (i <- start until array.length) {
      if (valueToFind == array(i)) return i
    }

    INDEX_NOT_FOUND
  }

  def indexOf(array: Array[Long], valueToFind: Long): Int = indexOf(array, valueToFind, 0)

  def indexOf(array: Array[Long], valueToFind: Long, startIndex: Int): Int = {
    if (array == null) return INDEX_NOT_FOUND
    def start = if (startIndex < 0) 0  else startIndex
    for (i <- start until array.length) {
      if (valueToFind == array(i)) return i
    }
    INDEX_NOT_FOUND
  }

  /**
    * <p>Finds the index of the given object in the array.
    *
    * <p>This method returns {@link #INDEX_NOT_FOUND} ({@code -1}) for a {@code null} input array.
    *
    * @param array        the array to search through for the object, may be {@code null}
    * @param objectToFind the object to find, may be {@code null}
    * @return the index of the object within the array,
    *         {@link #INDEX_NOT_FOUND} ({@code -1}) if not found or {@code null} array input
    */
  def indexOf(array: Array[_], objectToFind: Any): Int = indexOf(array, objectToFind, 0)

  /**
    * <p>Finds the index of the given object in the array starting at the given index.
    *
    * <p>This method returns {@link #INDEX_NOT_FOUND} ({@code -1}) for a {@code null} input array.
    *
    * <p>A negative startIndex is treated as zero. A startIndex larger than the array
    * length will return {@link #INDEX_NOT_FOUND} ({@code -1}).
    *
    * @param array        the array to search through for the object, may be {@code null}
    * @param objectToFind the object to find, may be {@code null}
    * @param startIndex   the index to start searching at
    * @return the index of the object within the array starting at the index,
    *         {@link #INDEX_NOT_FOUND} ({@code -1}) if not found or {@code null} array input
    */
  def indexOf(array: Array[_], objectToFind: Any, startIndex: Int): Int = {
    if (array == null) return INDEX_NOT_FOUND
    @inline def start: Int = if (startIndex < 0) 0 else startIndex

    if (objectToFind == null) for (i <- start until array.length) {
      if (array(i) == null) return i
    } else for (i <- start until array.length) {
      if (objectToFind == array(i)) return i
    }
    INDEX_NOT_FOUND
  }

  def indexOf(array: Array[Short], valueToFind: Short): Int = indexOf(array, valueToFind, 0)

  def indexOf(array: Array[Short], valueToFind: Short, startIndex: Int): Int = {
    if (array == null) return INDEX_NOT_FOUND
    @inline def start: Int = if (startIndex < 0) 0 else startIndex

    for (i <- start until array.length) {
      if (valueToFind == array(i)) return i
    }

    INDEX_NOT_FOUND
  }
  //
  //  /**
  //    * <p>Inserts elements into an array at the given index (starting from zero).</p>
  //    *
  //    * <p>When an array is returned, it is always a new array.</p>
  //    *
  //    * <pre>
  //    * ArrayUtils.insert(index, null, null)      = null
  //    * ArrayUtils.insert(index, array, null)     = cloned copy of 'array'
  //    * ArrayUtils.insert(index, null, values)    = null
  //    * </pre>
  //    *
  //    * @param index  the position within {@code array} to insert the new values
  //    * @param array  the array to insert the values into, may be {@code null}
  //    * @param values the new values to insert, may be {@code null}
  //    * @return The new array.
  //    * @throws IndexOutOfBoundsException if {@code array} is provided
  //    *                                   and either {@code index < 0} or {@code index > array.length}
  //    * @since 3.6
  //    */
  //  def insert(index: Int, array: Array[Boolean], values: Boolean*): Array[Boolean] = {
  //    if (array == null) return null
  //    if (ArrayUtils.isEmpty(values)) return clone(array)
  //    if (index < 0 || index > array.length) throw new IndexOutOfBoundsException("Index: " + index + ", Length: " + array.length)
  //    val result = new Array[Boolean](array.length + values.length)
  //    System.arraycopy(values, 0, result, index, values.length)
  //    if (index > 0) System.arraycopy(array, 0, result, 0, index)
  //    if (index < array.length) System.arraycopy(array, index, result, index + values.length, array.length - index)
  //    result
  //  }
  //
  //  def insert(index: Int, array: Array[Byte], values: Byte*): Array[Byte] = {
  //    if (array == null) return null
  //    if (ArrayUtils.isEmpty(values)) return clone(array)
  //    if (index < 0 || index > array.length) throw new IndexOutOfBoundsException("Index: " + index + ", Length: " + array.length)
  //    val result = new Array[Byte](array.length + values.length)
  //    System.arraycopy(values, 0, result, index, values.length)
  //    if (index > 0) System.arraycopy(array, 0, result, 0, index)
  //    if (index < array.length) System.arraycopy(array, index, result, index + values.length, array.length - index)
  //    result
  //  }
  //
  //  def insert(index: Int, array: Array[Char], values: Char*): Array[Char] = {
  //    if (array == null) return null
  //    if (ArrayUtils.isEmpty(values)) return clone(array)
  //    if (index < 0 || index > array.length) throw new IndexOutOfBoundsException("Index: " + index + ", Length: " + array.length)
  //    val result = new Array[Char](array.length + values.length)
  //    System.arraycopy(values, 0, result, index, values.length)
  //    if (index > 0) System.arraycopy(array, 0, result, 0, index)
  //    if (index < array.length) System.arraycopy(array, index, result, index + values.length, array.length - index)
  //    result
  //  }
  //
  //  def insert(index: Int, array: Array[Double], values: Double*): Array[Double] = {
  //    if (array == null) return null
  //    if (ArrayUtils.isEmpty(values)) return clone(array)
  //    if (index < 0 || index > array.length) throw new IndexOutOfBoundsException("Index: " + index + ", Length: " + array.length)
  //    val result = new Array[Double](array.length + values.length)
  //    System.arraycopy(values, 0, result, index, values.length)
  //    if (index > 0) System.arraycopy(array, 0, result, 0, index)
  //    if (index < array.length) System.arraycopy(array, index, result, index + values.length, array.length - index)
  //    result
  //  }
  //
  //  def insert(index: Int, array: Array[Float], values: Float*): Array[Float] = {
  //    if (array == null) return null
  //    if (ArrayUtils.isEmpty(values)) return clone(array)
  //    if (index < 0 || index > array.length) throw new IndexOutOfBoundsException("Index: " + index + ", Length: " + array.length)
  //    val result = new Array[Float](array.length + values.length)
  //    System.arraycopy(values, 0, result, index, values.length)
  //    if (index > 0) System.arraycopy(array, 0, result, 0, index)
  //    if (index < array.length) System.arraycopy(array, index, result, index + values.length, array.length - index)
  //    result
  //  }
  //
  //  def insert(index: Int, array: Array[Int], values: Int*): Array[Int] = {
  //    if (array == null) return null
  //    if (ArrayUtils.isEmpty(values)) return clone(array)
  //    if (index < 0 || index > array.length) throw new IndexOutOfBoundsException("Index: " + index + ", Length: " + array.length)
  //    val result = new Array[Int](array.length + values.length)
  //    System.arraycopy(values, 0, result, index, values.length)
  //    if (index > 0) System.arraycopy(array, 0, result, 0, index)
  //    if (index < array.length) System.arraycopy(array, index, result, index + values.length, array.length - index)
  //    result
  //  }
  //
  //  def insert(index: Int, array: Array[Long], values: Long*): Array[Long] = {
  //    if (array == null) return null
  //    if (ArrayUtils.isEmpty(values)) return clone(array)
  //    if (index < 0 || index > array.length) throw new IndexOutOfBoundsException("Index: " + index + ", Length: " + array.length)
  //    val result = new Array[Long](array.length + values.length)
  //    System.arraycopy(values, 0, result, index, values.length)
  //    if (index > 0) System.arraycopy(array, 0, result, 0, index)
  //    if (index < array.length) System.arraycopy(array, index, result, index + values.length, array.length - index)
  //    result
  //  }
  //
  //  def insert(index: Int, array: Array[Short], values: Short*): Array[Short] = {
  //    if (array == null) return null
  //    if (ArrayUtils.isEmpty(values)) return clone(array)
  //    if (index < 0 || index > array.length) throw new IndexOutOfBoundsException("Index: " + index + ", Length: " + array.length)
  //    val result = new Array[Short](array.length + values.length)
  //    System.arraycopy(values, 0, result, index, values.length)
  //    if (index > 0) System.arraycopy(array, 0, result, 0, index)
  //    if (index < array.length) System.arraycopy(array, index, result, index + values.length, array.length - index)
  //    result
  //  }
  //
  //  /**
  //    * <p>Inserts elements into an array at the given index (starting from zero).</p>
  //    *
  //    * <p>When an array is returned, it is always a new array.</p>
  //    *
  //    * <pre>
  //    * ArrayUtils.insert(index, null, null)      = null
  //    * ArrayUtils.insert(index, array, null)     = cloned copy of 'array'
  //    * ArrayUtils.insert(index, null, values)    = null
  //    * </pre>
  //    *
  //    * @tparam T     the type of elements in {@code array} and {@code values}
  //    * @param index  the position within {@code array} to insert the new values
  //    * @param array  the array to insert the values into, may be {@code null}
  //    * @param values the new values to insert, may be {@code null}
  //    * @return The new array.
  //    * @throws IndexOutOfBoundsException if {@code array} is provided
  //    *                                   and either {@code index < 0} or {@code index > array.length}
  //    * @since 3.6
  //    */
  //  @SafeVarargs def insert[T](index: Int, array: Array[T], values: T*): Array[T] = {
  //    /*
  //         * Note on use of @SafeVarargs:
  //         *
  //         * By returning null when 'array' is null, we avoid returning the vararg
  //         * array to the caller. We also avoid relying on the type of the vararg
  //         * array, by inspecting the component type of 'array'.
  //         */ if (array == null) return null
  //    if (ArrayUtils.isEmpty(values)) return clone(array)
  //    if (index < 0 || index > array.length) throw new IndexOutOfBoundsException("Index: " + index + ", Length: " + array.length)
  //    val `type` = array.getClass.getComponentType
  //    @SuppressWarnings(Array("unchecked")) // OK, because array and values are of type T
  //    val result: Array[T] = ReflectArray.newInstance(`type`, array.length + values.length).asInstanceOf[Array[T]]
  //
  //    System.arraycopy(values, 0, result, index, values.length)
  //    if (index > 0) System.arraycopy(array, 0, result, 0, index)
  //    if (index < array.length) System.arraycopy(array, index, result, index + values.length, array.length - index)
  //    result
  //  }
  //
  /**
    * Returns whether a given array can safely be accessed at the given index.
    *
    * <pre>
    * ArrayUtils.isArrayIndexValid(null, 0)       = false
    * ArrayUtils.isArrayIndexValid([], 0)         = false
    * ArrayUtils.isArrayIndexValid(["a"], 0)      = true
    * </pre>
    *
    * @tparam T    the component type of the array
    * @param array the array to inspect, may be null
    * @param index the index of the array to be inspected
    * @return Whether the given index is safely-accessible in the given array
    * @since 3.8
    */
  def isArrayIndexValid[T](array: Array[T], index: Int): Boolean = index >= 0 && getLength(array) > index

  /**
    * <p>Checks if an array of primitive booleans is empty or {@code null}.
    *
    * @param array the array to test
    * @return {@code true} if the array is empty or {@code null}
    * @since 2.1
    */
  def isEmpty(array: Array[Boolean]): Boolean = getLength(array) == 0

  /**
    * <p>Checks if an array of primitive bytes is empty or {@code null}.
    *
    * @param array the array to test
    * @return {@code true} if the array is empty or {@code null}
    * @since 2.1
    */
  def isEmpty(array: Array[Byte]): Boolean = getLength(array) == 0

  /**
    * <p>Checks if an array of primitive chars is empty or {@code null}.
    *
    * @param array the array to test
    * @return {@code true} if the array is empty or {@code null}
    * @since 2.1
    */
  def isEmpty(array: Array[Char]): Boolean = getLength(array) == 0

  /**
    * <p>Checks if an array of primitive doubles is empty or {@code null}.
    *
    * @param array the array to test
    * @return {@code true} if the array is empty or {@code null}
    * @since 2.1
    */
  def isEmpty(array: Array[Double]): Boolean = getLength(array) == 0

  /**
    * <p>Checks if an array of primitive floats is empty or {@code null}.
    *
    * @param array the array to test
    * @return {@code true} if the array is empty or {@code null}
    * @since 2.1
    */
  def isEmpty(array: Array[Float]): Boolean = getLength(array) == 0

  /**
    * <p>Checks if an array of primitive ints is empty or {@code null}.
    *
    * @param array the array to test
    * @return {@code true} if the array is empty or {@code null}
    * @since 2.1
    */
  def isEmpty(array: Array[Int]): Boolean = getLength(array) == 0

  /**
    * <p>Checks if an array of primitive longs is empty or {@code null}.
    *
    * @param array the array to test
    * @return {@code true} if the array is empty or {@code null}
    * @since 2.1
    */
  def isEmpty(array: Array[Long]): Boolean = getLength(array) == 0

  /**
    * <p>Checks if an array of Objects is empty or {@code null}.
    *
    * @param array the array to test
    * @return {@code true} if the array is empty or {@code null}
    * @since 2.1
    */
  def isEmpty(array: Array[AnyRef]): Boolean = getLength(array) == 0

  /**
    * <p>Checks if an array of primitive shorts is empty or {@code null}.
    *
    * @param array the array to test
    * @return {@code true} if the array is empty or {@code null}
    * @since 2.1
    */
  def isEmpty(array: Array[Short]): Boolean = getLength(array) == 0

  //  /**
  //    * <p>Compares two arrays, using equals(), handling multi-dimensional arrays
  //    * correctly.
  //    *
  //    * <p>Multi-dimensional primitive arrays are also handled correctly by this method.
  //    *
  //    * @param array1 the left hand array to compare, may be {@code null}
  //    * @param array2 the right hand array to compare, may be {@code null}
  //    * @return {@code true} if the arrays are equal
  //    * @deprecated this method has been replaced by {@code java.ju.Objects.deepEquals(Object, Object)} and will be
  //    *             removed from future releases.
  //    */
  //  @deprecated def isEquals(array1: Any, array2: Any): Boolean = new EqualsBuilder().append(array1, array2).isEquals
  //
  //  /**
  //    * <p>Checks if an array of primitive booleans is not empty and not {@code null}.
  //    *
  //    * @param array the array to test
  //    * @return {@code true} if the array is not empty and not {@code null}
  //    * @since 2.5
  //    */
  //  def isNotEmpty(array: Array[Boolean]): Boolean = !isEmpty(array)
  //
  //  /**
  //    * <p>Checks if an array of primitive bytes is not empty and not {@code null}.
  //    *
  //    * @param array the array to test
  //    * @return {@code true} if the array is not empty and not {@code null}
  //    * @since 2.5
  //    */
  //  def isNotEmpty(array: Array[Byte]): Boolean = !isEmpty(array)
  //
  //  /**
  //    * <p>Checks if an array of primitive chars is not empty and not {@code null}.
  //    *
  //    * @param array the array to test
  //    * @return {@code true} if the array is not empty and not {@code null}
  //    * @since 2.5
  //    */
  //  def isNotEmpty(array: Array[Char]): Boolean = !isEmpty(array)
  //
  //  /**
  //    * <p>Checks if an array of primitive doubles is not empty and not {@code null}.
  //    *
  //    * @param array the array to test
  //    * @return {@code true} if the array is not empty and not {@code null}
  //    * @since 2.5
  //    */
  //  def isNotEmpty(array: Array[Double]): Boolean = !isEmpty(array)
  //
  //  /**
  //    * <p>Checks if an array of primitive floats is not empty and not {@code null}.
  //    *
  //    * @param array the array to test
  //    * @return {@code true} if the array is not empty and not {@code null}
  //    * @since 2.5
  //    */
  //  def isNotEmpty(array: Array[Float]): Boolean = !isEmpty(array)
  //
  //  /**
  //    * <p>Checks if an array of primitive ints is not empty and not {@code null}.
  //    *
  //    * @param array the array to test
  //    * @return {@code true} if the array is not empty and not {@code null}
  //    * @since 2.5
  //    */
  //  def isNotEmpty(array: Array[Int]): Boolean = !isEmpty(array)
  //
  //  /**
  //    * <p>Checks if an array of primitive longs is not empty and not {@code null}.
  //    *
  //    * @param array the array to test
  //    * @return {@code true} if the array is not empty and not {@code null}
  //    * @since 2.5
  //    */
  //  def isNotEmpty(array: Array[Long]): Boolean = !isEmpty(array)
  //
  //  /**
  //    * <p>Checks if an array of primitive shorts is not empty and not {@code null}.
  //    *
  //    * @param array the array to test
  //    * @return {@code true} if the array is not empty and not {@code null}
  //    * @since 2.5
  //    */
  //  def isNotEmpty(array: Array[Short]): Boolean = !isEmpty(array)
  //
  //  /**
  //    * <p>Checks if an array of Objects is not empty and not {@code null}.
  //    *
  //    * @tparam T    the component type of the array
  //    * @param array the array to test
  //    * @return {@code true} if the array is not empty and not {@code null}
  //    * @since 2.5
  //    */
  //  def isNotEmpty[T](array: Array[T]): Boolean = !isEmpty(array)
  //
  //  /**
  //    * <p>Checks whether two arrays are the same length, treating
  //    * {@code null} arrays as length {@code 0}.
  //    *
  //    * @param array1 the first array, may be {@code null}
  //    * @param array2 the second array, may be {@code null}
  //    * @return {@code true} if length of arrays matches, treating
  //    *         {@code null} as an empty array
  //    */
  def isSameLength(array1: Array[Boolean], array2: Array[Boolean]): Boolean = getLength(array1) == getLength(array2)

  def isSameLength(array1: Array[Byte], array2: Array[Byte]): Boolean = getLength(array1) == getLength(array2)

  def isSameLength(array1: Array[Char], array2: Array[Char]): Boolean = getLength(array1) == getLength(array2)

  def isSameLength(array1: Array[Double], array2: Array[Double]): Boolean = getLength(array1) == getLength(array2)

  def isSameLength(array1: Array[Float], array2: Array[Float]): Boolean = getLength(array1) == getLength(array2)

  def isSameLength(array1: Array[Int], array2: Array[Int]): Boolean = getLength(array1) == getLength(array2)

  def isSameLength(array1: Array[Long], array2: Array[Long]): Boolean = getLength(array1) == getLength(array2)

  /**
    * <p>Checks whether two arrays are the same length, treating
    * {@code null} arrays as length {@code 0}.
    *
    * <p>Any multi-dimensional aspects of the arrays are ignored.
    *
    * @param array1 the first array, may be {@code null}
    * @param array2 the second array, may be {@code null}
    * @return {@code true} if length of arrays matches, treating
    *         {@code null} as an empty array
    * @since 3.11
    */
  def isSameLength(array1: Any, array2: Any): Boolean = getLength(array1) == getLength(array2)

  /**
    * <p>Checks whether two arrays are the same length, treating
    * {@code null} arrays as length {@code 0}.
    *
    * <p>Any multi-dimensional aspects of the arrays are ignored.
    *
    * @param array1 the first array, may be {@code null}
    * @param array2 the second array, may be {@code null}
    * @return {@code true} if length of arrays matches, treating
    *         {@code null} as an empty array
    */
  def isSameLength(array1: Array[AnyRef], array2: Array[AnyRef]): Boolean = getLength(array1) == getLength(array2)

  def isSameLength(array1: Array[Short], array2: Array[Short]): Boolean = getLength(array1) == getLength(array2)

  /**
    * <p>Checks whether two arrays are the same type taking into account
    * multi-dimensional arrays.
    *
    * @param array1 the first array, must not be {@code null}
    * @param array2 the second array, must not be {@code null}
    * @return {@code true} if type of arrays matches
    * @throws IllegalArgumentException if either array is {@code null}
    */
  def isSameType(array1: Any, array2: Any): Boolean = {
    if (array1 == null || array2 == null) throw new IllegalArgumentException("The Array must not be null")
    array1.getClass.getName == array2.getClass.getName
  }
  //
  //  /**
  //    * <p>This method checks whether the provided array is sorted according to natural ordering
  //    * ({@code false} before {@code true}).
  //    *
  //    * @param array the array to check
  //    * @return whether the array is sorted according to natural ordering
  //    * @since 3.4
  //    */
  //  def isSorted(array: Array[Boolean]): Boolean = {
  //    if (array == null || array.length < 2) return true
  //    var previous = array(0)
  //    val n = array.length
  //    for (i <- 1 until n) {
  //      val current = array(i)
  //      if (BooleanUtils.compare(previous, current) > 0) return false
  //      previous = current
  //    }
  //    true
  //  }
  //
  //  /**
  //    * <p>This method checks whether the provided array is sorted according to natural ordering.
  //    *
  //    * @param array the array to check
  //    * @return whether the array is sorted according to natural ordering
  //    * @since 3.4
  //    */
  //  def isSorted(array: Array[Byte]): Boolean = {
  //    if (array == null || array.length < 2) return true
  //    var previous = array(0)
  //    val n = array.length
  //    for (i <- 1 until n) {
  //      val current = array(i)
  //      if (NumberUtils.compare(previous, current) > 0) return false
  //      previous = current
  //    }
  //    true
  //  }
  //
  //  def isSorted(array: Array[Char]): Boolean = {
  //    if (array == null || array.length < 2) return true
  //    var previous = array(0)
  //    val n = array.length
  //    for (i <- 1 until n) {
  //      val current = array(i)
  //      if (CharUtils.compare(previous, current) > 0) return false
  //      previous = current
  //    }
  //    true
  //  }
  //
  //  def isSorted(array: Array[Double]): Boolean = {
  //    if (array == null || array.length < 2) return true
  //    var previous = array(0)
  //    val n = array.length
  //    for (i <- 1 until n) {
  //      val current = array(i)
  //      if (Double.compare(previous, current) > 0) return false
  //      previous = current
  //    }
  //    true
  //  }
  //
  //  def isSorted(array: Array[Float]): Boolean = {
  //    if (array == null || array.length < 2) return true
  //    var previous = array(0)
  //    val n = array.length
  //    for (i <- 1 until n) {
  //      val current = array(i)
  //      if (Float.compare(previous, current) > 0) return false
  //      previous = current
  //    }
  //    true
  //  }
  //
  //  def isSorted(array: Array[Int]): Boolean = {
  //    if (array == null || array.length < 2) return true
  //    var previous = array(0)
  //    val n = array.length
  //    for (i <- 1 until n) {
  //      val current = array(i)
  //      if (NumberUtils.compare(previous, current) > 0) return false
  //      previous = current
  //    }
  //    true
  //  }
  //
  //  def isSorted(array: Array[Long]): Boolean = {
  //    if (array == null || array.length < 2) return true
  //    var previous = array(0)
  //    val n = array.length
  //    for (i <- 1 until n) {
  //      val current = array(i)
  //      if (NumberUtils.compare(previous, current) > 0) return false
  //      previous = current
  //    }
  //    true
  //  }
  //
  //  def isSorted(array: Array[Short]): Boolean = {
  //    if (array == null || array.length < 2) return true
  //    var previous = array(0)
  //    val n = array.length
  //    for (i <- 1 until n) {
  //      val current = array(i)
  //      if (NumberUtils.compare(previous, current) > 0) return false
  //      previous = current
  //    }
  //    true
  //  }
  //
  //  /**
  //    * <p>This method checks whether the provided array is sorted according to the class's
  //    * {@code compareTo} method.
  //    *
  //    * @param array the array to check
  //    * @tparam T    the datatype of the array to check, it must implement {@code Comparable}
  //    * @return whether the array is sorted
  //    * @since 3.4
  //    */
  //  def isSorted[T <: Comparable[Any]](array: Array[T]): Boolean = isSorted(array, Comparable.compareTo)
  //
  //  /**
  //    * <p>This method checks whether the provided array is sorted according to the provided {@code Comparator}.
  //    *
  //    * @param array      the array to check
  //    * @param comparator the {@code Comparator} to compare over
  //    * @tparam T         the datatype of the array
  //    * @return whether the array is sorted
  //    * @since 3.4
  //    */
  //  def isSorted[T](array: Array[T], comparator: Comparator[T]): Boolean = {
  //    if (comparator == null) throw new IllegalArgumentException("Comparator should not be null.")
  //    if (array == null || array.length < 2) return true
  //    var previous = array(0)
  //    val n = array.length
  //    for (i <- 1 until n) {
  //      val current = array(i)
  //      if (comparator.compare(previous, current) > 0) return false
  //      previous = current
  //    }
  //    true
  //  }
  //
  //  /**
  //    * <p>Finds the last index of the given value within the array.
  //    *
  //    * <p>This method returns {@link #INDEX_NOT_FOUND} ({@code -1}) if
  //    * {@code null} array input.
  //    *
  //    * @param array       the array to traverse backwards looking for the object, may be {@code null}
  //    * @param valueToFind the object to find
  //    * @return the last index of the value within the array,
  //    *         {@link #INDEX_NOT_FOUND} ({@code -1}) if not found or {@code null} array input
  //    */
  //  def lastIndexOf(array: Array[Boolean], valueToFind: Boolean): Int = lastIndexOf(array, valueToFind, Integer.MAX_VALUE)
  //
  //  /**
  //    * <p>Finds the last index of the given value in the array starting at the given index.
  //    *
  //    * <p>This method returns {@link #INDEX_NOT_FOUND} ({@code -1}) for a {@code null} input array.
  //    *
  //    * <p>A negative startIndex will return {@link #INDEX_NOT_FOUND} ({@code -1}). A startIndex larger than
  //    * the array length will search from the end of the array.
  //    *
  //    * @param array       the array to traverse for looking for the object, may be {@code null}
  //    * @param valueToFind the value to find
  //    * @param startIndex  the start index to traverse backwards from
  //    * @return the last index of the value within the array,
  //    *         {@link #INDEX_NOT_FOUND} ({@code -1}) if not found or {@code null} array input
  //    */
  //  def lastIndexOf(array: Array[Boolean], valueToFind: Boolean, startIndex: Int): Int = {
  //    if (isEmpty(array)) return INDEX_NOT_FOUND
  //    if (startIndex < 0) return INDEX_NOT_FOUND
  //    else if (startIndex >= array.length) startIndex = array.length - 1
  //    for (i <- startIndex to 0 by -1) {
  //      if (valueToFind == array(i)) return i
  //    }
  //    INDEX_NOT_FOUND
  //  }
  //
  //  /**
  //    * <p>Finds the last index of the given value within the array.
  //    *
  //    * <p>This method returns {@link #INDEX_NOT_FOUND} ({@code -1}) for a {@code null} input array.
  //    *
  //    * @param array       the array to traverse backwards looking for the object, may be {@code null}
  //    * @param valueToFind the object to find
  //    * @return the last index of the value within the array,
  //    *         {@link #INDEX_NOT_FOUND} ({@code -1}) if not found or {@code null} array input
  //    */
  //  def lastIndexOf(array: Array[Byte], valueToFind: Byte): Int = lastIndexOf(array, valueToFind, Integer.MAX_VALUE)
  //
  //  /**
  //    * <p>Finds the last index of the given value in the array starting at the given index.
  //    *
  //    * <p>This method returns {@link #INDEX_NOT_FOUND} ({@code -1}) for a {@code null} input array.
  //    *
  //    * <p>A negative startIndex will return {@link #INDEX_NOT_FOUND} ({@code -1}). A startIndex larger than the
  //    * array length will search from the end of the array.
  //    *
  //    * @param array       the array to traverse for looking for the object, may be {@code null}
  //    * @param valueToFind the value to find
  //    * @param startIndex  the start index to traverse backwards from
  //    * @return the last index of the value within the array,
  //    *         {@link #INDEX_NOT_FOUND} ({@code -1}) if not found or {@code null} array input
  //    */
  //  def lastIndexOf(array: Array[Byte], valueToFind: Byte, startIndex: Int): Int = {
  //    if (array == null) return INDEX_NOT_FOUND
  //    if (startIndex < 0) return INDEX_NOT_FOUND
  //    else if (startIndex >= array.length) startIndex = array.length - 1
  //    for (i <- startIndex to 0 by -1) {
  //      if (valueToFind == array(i)) return i
  //    }
  //    INDEX_NOT_FOUND
  //  }
  //
  //  /**
  //    * <p>Finds the last index of the given value within the array.
  //    *
  //    * <p>This method returns {@link #INDEX_NOT_FOUND} ({@code -1}) for a {@code null} input array.
  //    *
  //    * @param array       the array to traverse backwards looking for the object, may be {@code null}
  //    * @param valueToFind the object to find
  //    * @return the last index of the value within the array,
  //    *         {@link #INDEX_NOT_FOUND} ({@code -1}) if not found or {@code null} array input
  //    * @since 2.1
  //    */
  //  def lastIndexOf(array: Array[Char], valueToFind: Char): Int = lastIndexOf(array, valueToFind, Integer.MAX_VALUE)
  //
  //  /**
  //    * <p>Finds the last index of the given value in the array starting at the given index.
  //    *
  //    * <p>This method returns {@link #INDEX_NOT_FOUND} ({@code -1}) for a {@code null} input array.
  //    *
  //    * <p>A negative startIndex will return {@link #INDEX_NOT_FOUND} ({@code -1}). A startIndex larger than the
  //    * array length will search from the end of the array.
  //    *
  //    * @param array       the array to traverse for looking for the object, may be {@code null}
  //    * @param valueToFind the value to find
  //    * @param startIndex  the start index to traverse backwards from
  //    * @return the last index of the value within the array,
  //    *         {@link #INDEX_NOT_FOUND} ({@code -1}) if not found or {@code null} array input
  //    * @since 2.1
  //    */
  //  def lastIndexOf(array: Array[Char], valueToFind: Char, startIndex: Int): Int = {
  //    if (array == null) return INDEX_NOT_FOUND
  //    if (startIndex < 0) return INDEX_NOT_FOUND
  //    else if (startIndex >= array.length) startIndex = array.length - 1
  //    for (i <- startIndex to 0 by -1) {
  //      if (valueToFind == array(i)) return i
  //    }
  //    INDEX_NOT_FOUND
  //  }
  //
  //  def lastIndexOf(array: Array[Double], valueToFind: Double): Int = lastIndexOf(array, valueToFind, Integer.MAX_VALUE)
  //
  //  /**
  //    * <p>Finds the last index of the given value within a given tolerance in the array.
  //    * This method will return the index of the last value which falls between the region
  //    * defined by valueToFind - tolerance and valueToFind + tolerance.
  //    *
  //    * <p>This method returns {@link #INDEX_NOT_FOUND} ({@code -1}) for a {@code null} input array.
  //    *
  //    * @param array       the array to search through for the object, may be {@code null}
  //    * @param valueToFind the value to find
  //    * @param tolerance   tolerance of the search
  //    * @return the index of the value within the array,
  //    *         {@link #INDEX_NOT_FOUND} ({@code -1}) if not found or {@code null} array input
  //    */
  //  def lastIndexOf(array: Array[Double], valueToFind: Double, tolerance: Double): Int = lastIndexOf(array, valueToFind, Integer.MAX_VALUE, tolerance)
  //
  //  def lastIndexOf(array: Array[Double], valueToFind: Double, startIndex: Int): Int = {
  //    if (isEmpty(array)) return INDEX_NOT_FOUND
  //    if (startIndex < 0) return INDEX_NOT_FOUND
  //    else if (startIndex >= array.length) startIndex = array.length - 1
  //    for (i <- startIndex to 0 by -1) {
  //      if (valueToFind == array(i)) return i
  //    }
  //    INDEX_NOT_FOUND
  //  }
  //
  //  /**
  //    * <p>Finds the last index of the given value in the array starting at the given index.
  //    * This method will return the index of the last value which falls between the region
  //    * defined by valueToFind - tolerance and valueToFind + tolerance.
  //    *
  //    * <p>This method returns {@link #INDEX_NOT_FOUND} ({@code -1}) for a {@code null} input array.
  //    *
  //    * <p>A negative startIndex will return {@link #INDEX_NOT_FOUND} ({@code -1}). A startIndex larger than the
  //    * array length will search from the end of the array.
  //    *
  //    * @param array       the array to traverse for looking for the object, may be {@code null}
  //    * @param valueToFind the value to find
  //    * @param startIndex  the start index to traverse backwards from
  //    * @param tolerance   search for value within plus/minus this amount
  //    * @return the last index of the value within the array,
  //    *         {@link #INDEX_NOT_FOUND} ({@code -1}) if not found or {@code null} array input
  //    */
  //  def lastIndexOf(array: Array[Double], valueToFind: Double, startIndex: Int, tolerance: Double): Int = {
  //    if (isEmpty(array)) return INDEX_NOT_FOUND
  //    if (startIndex < 0) return INDEX_NOT_FOUND
  //    else if (startIndex >= array.length) startIndex = array.length - 1
  //    val min = valueToFind - tolerance
  //    val max = valueToFind + tolerance
  //    for (i <- startIndex to 0 by -1) {
  //      if (array(i) >= min && array(i) <= max) return i
  //    }
  //    INDEX_NOT_FOUND
  //  }
  //
  //  def lastIndexOf(array: Array[Float], valueToFind: Float): Int = lastIndexOf(array, valueToFind, Integer.MAX_VALUE)
  //
  //  def lastIndexOf(array: Array[Float], valueToFind: Float, startIndex: Int): Int = {
  //    if (isEmpty(array)) return INDEX_NOT_FOUND
  //    if (startIndex < 0) return INDEX_NOT_FOUND
  //    else if (startIndex >= array.length) startIndex = array.length - 1
  //    for (i <- startIndex to 0 by -1) {
  //      if (valueToFind == array(i)) return i
  //    }
  //    INDEX_NOT_FOUND
  //  }
  //
  //  def lastIndexOf(array: Array[Int], valueToFind: Int): Int = lastIndexOf(array, valueToFind, Integer.MAX_VALUE)
  //
  //  def lastIndexOf(array: Array[Int], valueToFind: Int, startIndex: Int): Int = {
  //    if (array == null) return INDEX_NOT_FOUND
  //    if (startIndex < 0) return INDEX_NOT_FOUND
  //    else if (startIndex >= array.length) startIndex = array.length - 1
  //    for (i <- startIndex to 0 by -1) {
  //      if (valueToFind == array(i)) return i
  //    }
  //    INDEX_NOT_FOUND
  //  }
  //
  //  def lastIndexOf(array: Array[Long], valueToFind: Long): Int = lastIndexOf(array, valueToFind, Integer.MAX_VALUE)
  //
  //  def lastIndexOf(array: Array[Long], valueToFind: Long, startIndex: Int): Int = {
  //    if (array == null) return INDEX_NOT_FOUND
  //    if (startIndex < 0) return INDEX_NOT_FOUND
  //    else if (startIndex >= array.length) startIndex = array.length - 1
  //    for (i <- startIndex to 0 by -1) {
  //      if (valueToFind == array(i)) return i
  //    }
  //    INDEX_NOT_FOUND
  //  }
  //
  //  /**
  //    * <p>Finds the last index of the given object within the array.
  //    *
  //    * <p>This method returns {@link #INDEX_NOT_FOUND} ({@code -1}) for a {@code null} input array.
  //    *
  //    * @param array        the array to traverse backwards looking for the object, may be {@code null}
  //    * @param objectToFind the object to find, may be {@code null}
  //    * @return the last index of the object within the array,
  //    *         {@link #INDEX_NOT_FOUND} ({@code -1}) if not found or {@code null} array input
  //    */
  //  def lastIndexOf(array: Array[AnyRef], objectToFind: Any): Int = lastIndexOf(array, objectToFind, Integer.MAX_VALUE)
  //
  //  /**
  //    * <p>Finds the last index of the given object in the array starting at the given index.
  //    *
  //    * <p>This method returns {@link #INDEX_NOT_FOUND} ({@code -1}) for a {@code null} input array.
  //    *
  //    * <p>A negative startIndex will return {@link #INDEX_NOT_FOUND} ({@code -1}). A startIndex larger than
  //    * the array length will search from the end of the array.
  //    *
  //    * @param array        the array to traverse for looking for the object, may be {@code null}
  //    * @param objectToFind the object to find, may be {@code null}
  //    * @param startIndex   the start index to traverse backwards from
  //    * @return the last index of the object within the array,
  //    *         {@link #INDEX_NOT_FOUND} ({@code -1}) if not found or {@code null} array input
  //    */
  //  def lastIndexOf(array: Array[AnyRef], objectToFind: Any, startIndex: Int): Int = {
  //    if (array == null) return INDEX_NOT_FOUND
  //    if (startIndex < 0) return INDEX_NOT_FOUND
  //    else if (startIndex >= array.length) startIndex = array.length - 1
  //    if (objectToFind == null) for (i <- startIndex to 0 by -1) {
  //      if (array(i) == null) return i
  //    }
  //    else if (array.getClass.getComponentType.isInstance(objectToFind)) for (i <- startIndex to 0 by -1) {
  //      if (objectToFind == array(i)) return i
  //    }
  //    INDEX_NOT_FOUND
  //  }
  //
  //  def lastIndexOf(array: Array[Short], valueToFind: Short): Int = lastIndexOf(array, valueToFind, Integer.MAX_VALUE)
  //
  //  def lastIndexOf(array: Array[Short], valueToFind: Short, startIndex: Int): Int = {
  //    if (array == null) return INDEX_NOT_FOUND
  //    if (startIndex < 0) return INDEX_NOT_FOUND
  //    else if (startIndex >= array.length) startIndex = array.length - 1
  //    for (i <- startIndex to 0 by -1) {
  //      if (valueToFind == array(i)) return i
  //    }
  //    INDEX_NOT_FOUND
  //  }
  //
  //  /**
  //    * <p>Defensive programming technique to change a {@code null}
  //    * reference to an empty one.
  //    *
  //    * <p>This method returns an empty array for a {@code null} input array.
  //    *
  //    * <p>As a memory optimizing technique an empty array passed in will be overridden with
  //    * the empty {@code public static} references in this class.
  //    *
  //    * @param array the array to check for {@code null} or empty
  //    * @return the same array, {@code public static} empty array if {@code null} or empty input
  //    * @since 2.5
  //    */
  //  def nullToEmpty(array: Array[Boolean]): Array[Boolean] = {
  //    if (isEmpty(array)) return EMPTY_BOOLEAN_ARRAY
  //    array
  //  }
  //
  //  def nullToEmpty(array: Array[Boolean]): Array[Boolean] = {
  //    if (isEmpty(array)) return EMPTY_BOOLEAN_OBJECT_ARRAY
  //    array
  //  }
  //
  //  def nullToEmpty(array: Array[Byte]): Array[Byte] = {
  //    if (isEmpty(array)) return EMPTY_BYTE_ARRAY
  //    array
  //  }
  //
  //  def nullToEmpty(array: Array[Byte]): Array[Byte] = {
  //    if (isEmpty(array)) return EMPTY_BYTE_OBJECT_ARRAY
  //    array
  //  }
  //
  //  def nullToEmpty(array: Array[Char]): Array[Char] = {
  //    if (isEmpty(array)) return EMPTY_CHAR_ARRAY
  //    array
  //  }
  //
  //  def nullToEmpty(array: Array[Character]): Array[Character] = {
  //    if (isEmpty(array)) return EMPTY_CHARACTER_OBJECT_ARRAY
  //    array
  //  }
  //
  //  /**
  //    * <p>Defensive programming technique to change a {@code null}
  //    * reference to an empty one.
  //    *
  //    * <p>This method returns an empty array for a {@code null} input array.
  //    *
  //    * <p>As a memory optimizing technique an empty array passed in will be overridden with
  //    * the empty {@code public static} references in this class.
  //    *
  //    * @param array the array to check for {@code null} or empty
  //    * @return the same array, {@code public static} empty array if {@code null} or empty input
  //    * @since 3.2
  //    */
  //  def nullToEmpty(array: Array[Class[_]]): Array[Class[_]] = {
  //    if (isEmpty(array)) return EMPTY_CLASS_ARRAY
  //    array
  //  }
  //
  //  def nullToEmpty(array: Array[Double]): Array[Double] = {
  //    if (isEmpty(array)) return EMPTY_DOUBLE_ARRAY
  //    array
  //  }
  //
  //  def nullToEmpty(array: Array[Double]): Array[Double] = {
  //    if (isEmpty(array)) return EMPTY_DOUBLE_OBJECT_ARRAY
  //    array
  //  }
  //
  //  def nullToEmpty(array: Array[Float]): Array[Float] = {
  //    if (isEmpty(array)) return EMPTY_FLOAT_ARRAY
  //    array
  //  }
  //
  //  def nullToEmpty(array: Array[Float]): Array[Float] = {
  //    if (isEmpty(array)) return EMPTY_FLOAT_OBJECT_ARRAY
  //    array
  //  }
  //
  //  def nullToEmpty(array: Array[Int]): Array[Int] = {
  //    if (isEmpty(array)) return EMPTY_INT_ARRAY
  //    array
  //  }
  //
  //  def nullToEmpty(array: Array[Integer]): Array[Integer] = {
  //    if (isEmpty(array)) return EMPTY_INTEGER_OBJECT_ARRAY
  //    array
  //  }
  //
  //  def nullToEmpty(array: Array[Long]): Array[Long] = {
  //    if (isEmpty(array)) return EMPTY_LONG_ARRAY
  //    array
  //  }
  //
  //  def nullToEmpty(array: Array[Long]): Array[Long] = {
  //    if (isEmpty(array)) return EMPTY_LONG_OBJECT_ARRAY
  //    array
  //  }
  //
  //  def nullToEmpty(array: Array[AnyRef]): Array[AnyRef] = {
  //    if (isEmpty(array)) return EMPTY_OBJECT_ARRAY
  //    array
  //  }
  //
  //  def nullToEmpty(array: Array[Short]): Array[Short] = {
  //    if (isEmpty(array)) return EMPTY_SHORT_ARRAY
  //    array
  //  }
  //
  //  def nullToEmpty(array: Array[Short]): Array[Short] = {
  //    if (isEmpty(array)) return EMPTY_SHORT_OBJECT_ARRAY
  //    array
  //  }
  //
  //  def nullToEmpty(array: Array[String]): Array[String] = {
  //    if (isEmpty(array)) return EMPTY_STRING_ARRAY
  //    array
  //  }
  //
  //  /**
  //    * <p>Defensive programming technique to change a {@code null}
  //    * reference to an empty one.
  //    *
  //    * <p>This method returns an empty array for a {@code null} input array.
  //    *
  //    * @param array the array to check for {@code null} or empty
  //    * @param type  the class representation of the desired array
  //    * @tparam T    the class type
  //    * @return the same array, {@code public static} empty array if {@code null}
  //    * @throws IllegalArgumentException if the type argument is null
  //    * @since 3.5
  //    */
  //  def nullToEmpty[T](array: Array[T], `type`: Class[Array[T]]): Array[T] = {
  //    if (`type` == null) throw new IllegalArgumentException("The type must not be null")
  //    if (array == null) return `type`.cast(ReflectArray.newInstance(`type`.getComponentType, 0))
  //    array
  //  }
  //
  //  /**
  //    * <p>Removes the element at the specified position from the specified array.
  //    * All subsequent elements are shifted to the left (subtracts one from
  //    * their indices).
  //    *
  //    * <p>This method returns a new array with the same elements of the input
  //    * array except the element on the specified position. The component
  //    * type of the returned array is always the same as that of the input
  //    * array.
  //    *
  //    * <p>If the input array is {@code null}, an IndexOutOfBoundsException
  //    * will be thrown, because in that case no valid index can be specified.
  //    *
  //    * <pre>
  //    * ArrayUtils.remove([true], 0)              = []
  //    * ArrayUtils.remove([true, false], 0)       = [false]
  //    * ArrayUtils.remove([true, false], 1)       = [true]
  //    * ArrayUtils.remove([true, true, false], 1) = [true, false]
  //    * </pre>
  //    *
  //    * @param array the array to remove the element from, may not be {@code null}
  //    * @param index the position of the element to be removed
  //    * @return A new array containing the existing elements except the element
  //    *         at the specified position.
  //    * @throws IndexOutOfBoundsException if the index is out of range
  //    *                                   (index &lt; 0 || index &gt;= array.length), or if the array is {@code null}.
  //    * @since 2.1
  //    */
  //  def remove(array: Array[Boolean], index: Int): Array[Boolean] = remove(array.asInstanceOf[Any], index).asInstanceOf[Array[Boolean]]
  //
  //  /**
  //    * <p>Removes the element at the specified position from the specified array.
  //    * All subsequent elements are shifted to the left (subtracts one from
  //    * their indices).
  //    *
  //    * <p>This method returns a new array with the same elements of the input
  //    * array except the element on the specified position. The component
  //    * type of the returned array is always the same as that of the input
  //    * array.
  //    *
  //    * <p>If the input array is {@code null}, an IndexOutOfBoundsException
  //    * will be thrown, because in that case no valid index can be specified.
  //    *
  //    * <pre>
  //    * ArrayUtils.remove([1], 0)          = []
  //    * ArrayUtils.remove([1, 0], 0)       = [0]
  //    * ArrayUtils.remove([1, 0], 1)       = [1]
  //    * ArrayUtils.remove([1, 0, 1], 1)    = [1, 1]
  //    * </pre>
  //    *
  //    * @param array the array to remove the element from, may not be {@code null}
  //    * @param index the position of the element to be removed
  //    * @return A new array containing the existing elements except the element
  //    *         at the specified position.
  //    * @throws IndexOutOfBoundsException if the index is out of range
  //    *                                   (index &lt; 0 || index &gt;= array.length), or if the array is {@code null}.
  //    * @since 2.1
  //    */
  //  def remove(array: Array[Byte], index: Int): Array[Byte] = remove(array.asInstanceOf[Any], index).asInstanceOf[Array[Byte]]
  //
  //  /**
  //    * <p>Removes the element at the specified position from the specified array.
  //    * All subsequent elements are shifted to the left (subtracts one from
  //    * their indices).
  //    *
  //    * <p>This method returns a new array with the same elements of the input
  //    * array except the element on the specified position. The component
  //    * type of the returned array is always the same as that of the input
  //    * array.
  //    *
  //    * <p>If the input array is {@code null}, an IndexOutOfBoundsException
  //    * will be thrown, because in that case no valid index can be specified.
  //    *
  //    * <pre>
  //    * ArrayUtils.remove(['a'], 0)           = []
  //    * ArrayUtils.remove(['a', 'b'], 0)      = ['b']
  //    * ArrayUtils.remove(['a', 'b'], 1)      = ['a']
  //    * ArrayUtils.remove(['a', 'b', 'c'], 1) = ['a', 'c']
  //    * </pre>
  //    *
  //    * @param array the array to remove the element from, may not be {@code null}
  //    * @param index the position of the element to be removed
  //    * @return A new array containing the existing elements except the element
  //    *         at the specified position.
  //    * @throws IndexOutOfBoundsException if the index is out of range
  //    *                                   (index &lt; 0 || index &gt;= array.length), or if the array is {@code null}.
  //    * @since 2.1
  //    */
  //  def remove(array: Array[Char], index: Int): Array[Char] = remove(array.asInstanceOf[Any], index).asInstanceOf[Array[Char]]
  //
  //  /**
  //    * <p>Removes the element at the specified position from the specified array.
  //    * All subsequent elements are shifted to the left (subtracts one from
  //    * their indices).
  //    *
  //    * <p>This method returns a new array with the same elements of the input
  //    * array except the element on the specified position. The component
  //    * type of the returned array is always the same as that of the input
  //    * array.
  //    *
  //    * <p>If the input array is {@code null}, an IndexOutOfBoundsException
  //    * will be thrown, because in that case no valid index can be specified.
  //    *
  //    * <pre>
  //    * ArrayUtils.remove([1.1], 0)           = []
  //    * ArrayUtils.remove([2.5, 6.0], 0)      = [6.0]
  //    * ArrayUtils.remove([2.5, 6.0], 1)      = [2.5]
  //    * ArrayUtils.remove([2.5, 6.0, 3.8], 1) = [2.5, 3.8]
  //    * </pre>
  //    *
  //    * @param array the array to remove the element from, may not be {@code null}
  //    * @param index the position of the element to be removed
  //    * @return A new array containing the existing elements except the element
  //    *         at the specified position.
  //    * @throws IndexOutOfBoundsException if the index is out of range
  //    *                                   (index &lt; 0 || index &gt;= array.length), or if the array is {@code null}.
  //    * @since 2.1
  //    */
  //  def remove(array: Array[Double], index: Int): Array[Double] = remove(array.asInstanceOf[Any], index).asInstanceOf[Array[Double]]
  //
  //  def remove(array: Array[Float], index: Int): Array[Float] = remove(array.asInstanceOf[Any], index).asInstanceOf[Array[Float]]
  //
  //  /**
  //    * <p>Removes the element at the specified position from the specified array.
  //    * All subsequent elements are shifted to the left (subtracts one from
  //    * their indices).
  //    *
  //    * <p>This method returns a new array with the same elements of the input
  //    * array except the element on the specified position. The component
  //    * type of the returned array is always the same as that of the input
  //    * array.
  //    *
  //    * <p>If the input array is {@code null}, an IndexOutOfBoundsException
  //    * will be thrown, because in that case no valid index can be specified.
  //    *
  //    * <pre>
  //    * ArrayUtils.remove([1], 0)         = []
  //    * ArrayUtils.remove([2, 6], 0)      = [6]
  //    * ArrayUtils.remove([2, 6], 1)      = [2]
  //    * ArrayUtils.remove([2, 6, 3], 1)   = [2, 3]
  //    * </pre>
  //    *
  //    * @param array the array to remove the element from, may not be {@code null}
  //    * @param index the position of the element to be removed
  //    * @return A new array containing the existing elements except the element
  //    *         at the specified position.
  //    * @throws IndexOutOfBoundsException if the index is out of range
  //    *                                   (index &lt; 0 || index &gt;= array.length), or if the array is {@code null}.
  //    * @since 2.1
  //    */
  //  def remove(array: Array[Int], index: Int): Array[Int] = remove(array.asInstanceOf[Any], index).asInstanceOf[Array[Int]]
  //
  //  def remove(array: Array[Long], index: Int): Array[Long] = remove(array.asInstanceOf[Any], index).asInstanceOf[Array[Long]]
  //
  //  /**
  //    * <p>Removes the element at the specified position from the specified array.
  //    * All subsequent elements are shifted to the left (subtracts one from
  //    * their indices).
  //    *
  //    * <p>This method returns a new array with the same elements of the input
  //    * array except the element on the specified position. The component
  //    * type of the returned array is always the same as that of the input
  //    * array.
  //    *
  //    * <p>If the input array is {@code null}, an IndexOutOfBoundsException
  //    * will be thrown, because in that case no valid index can be specified.
  //    *
  //    * @param array the array to remove the element from, may not be {@code null}
  //    * @param index the position of the element to be removed
  //    * @return A new array containing the existing elements except the element
  //    *         at the specified position.
  //    * @throws IndexOutOfBoundsException if the index is out of range
  //    *                                   (index &lt; 0 || index &gt;= array.length), or if the array is {@code null}.
  //    * @since 2.1
  //    */
  //  private def remove(array: Any, index: Int) = {
  //    val length = getLength(array)
  //    if (index < 0 || index >= length) throw new IndexOutOfBoundsException("Index: " + index + ", Length: " + length)
  //    val result = ReflectArray.newInstance(array.getClass.getComponentType, length - 1)
  //    System.arraycopy(array, 0, result, 0, index)
  //    if (index < length - 1) System.arraycopy(array, index + 1, result, index, length - index - 1)
  //    result
  //  }
  //
  //  def remove(array: Array[Short], index: Int): Array[Short] =
  //    remove(array.asInstanceOf[Any], index).asInstanceOf[Array[Short]]
  //
  //  /**
  //    * <p>Removes the element at the specified position from the specified array.
  //    * All subsequent elements are shifted to the left (subtracts one from
  //    * their indices).
  //    *
  //    * <p>This method returns a new array with the same elements of the input
  //    * array except the element on the specified position. The component
  //    * type of the returned array is always the same as that of the input
  //    * array.
  //    *
  //    * <p>If the input array is {@code null}, an IndexOutOfBoundsException
  //    * will be thrown, because in that case no valid index can be specified.
  //    *
  //    * <pre>
  //    * ArrayUtils.remove(["a"], 0)           = []
  //    * ArrayUtils.remove(["a", "b"], 0)      = ["b"]
  //    * ArrayUtils.remove(["a", "b"], 1)      = ["a"]
  //    * ArrayUtils.remove(["a", "b", "c"], 1) = ["a", "c"]
  //    * </pre>
  //    *
  //    * @param T     the component type of the array
  //    * @param array the array to remove the element from, may not be {@code null}
  //    * @param index the position of the element to be removed
  //    * @return A new array containing the existing elements except the element
  //    *         at the specified position.
  //    * @throws IndexOutOfBoundsException if the index is out of range
  //    *                                   (index &lt; 0 || index &gt;= array.length), or if the array is {@code null}.
  //    * @since 2.1
  //    */
  //  @SuppressWarnings(Array("unchecked")) // remove() always creates an array of the same type as its input
  //  def remove[T](array: Array[T], index: Int): Array[T] =
  //    remove(array.asInstanceOf[Any], index).asInstanceOf[Array[T]]
  //
  //  /**
  //    * <p>Removes the elements at the specified positions from the specified array.
  //    * All remaining elements are shifted to the left.
  //    *
  //    * <p>This method returns a new array with the same elements of the input
  //    * array except those at the specified positions. The component
  //    * type of the returned array is always the same as that of the input
  //    * array.
  //    *
  //    * <p>If the input array is {@code null}, an IndexOutOfBoundsException
  //    * will be thrown, because in that case no valid index can be specified.
  //    *
  //    * <pre>
  //    * ArrayUtils.removeAll([true, false, true], 0, 2) = [false]
  //    * ArrayUtils.removeAll([true, false, true], 1, 2) = [true]
  //    * </pre>
  //    *
  //    * @param array   the array to remove the element from, may not be {@code null}
  //    * @param indices the positions of the elements to be removed
  //    * @return A new array containing the existing elements except those
  //    *         at the specified positions.
  //    * @throws IndexOutOfBoundsException if any index is out of range
  //    *                                   (index &lt; 0 || index &gt;= array.length), or if the array is {@code null}.
  //    * @since 3.0.1
  //    */
  //  def removeAll (array: Array[Boolean], indices: Int *): Array[Boolean] = {
  //    return removeAll (array.asInstanceOf[Any], indices).asInstanceOf[Array[Boolean]]
  //  }
  //
  //  /**
  //    * <p>Removes the elements at the specified positions from the specified array.
  //    * All remaining elements are shifted to the left.
  //    *
  //    * <p>This method returns a new array with the same elements of the input
  //    * array except those at the specified positions. The component
  //    * type of the returned array is always the same as that of the input
  //    * array.
  //    *
  //    * <p>If the input array is {@code null}, an IndexOutOfBoundsException
  //    * will be thrown, because in that case no valid index can be specified.
  //    *
  //    * <pre>
  //    * ArrayUtils.removeAll([1], 0)             = []
  //    * ArrayUtils.removeAll([2, 6], 0)          = [6]
  //    * ArrayUtils.removeAll([2, 6], 0, 1)       = []
  //    * ArrayUtils.removeAll([2, 6, 3], 1, 2)    = [2]
  //    * ArrayUtils.removeAll([2, 6, 3], 0, 2)    = [6]
  //    * ArrayUtils.removeAll([2, 6, 3], 0, 1, 2) = []
  //    * </pre>
  //    *
  //    * @param array   the array to remove the element from, may not be {@code null}
  //    * @param indices the positions of the elements to be removed
  //    * @return A new array containing the existing elements except those
  //    *         at the specified positions.
  //    * @throws IndexOutOfBoundsException if any index is out of range
  //    *                                   (index &lt; 0 || index &gt;= array.length), or if the array is {@code null}.
  //    * @since 3.0.1
  //    */
  //  def removeAll (array: Array[Byte], indices: Int *): Array[Byte] = {
  //    return removeAll (array.asInstanceOf[Any], indices).asInstanceOf[Array[Byte]]
  //  }
  //  def removeAll (array: Array[Char], indices: Int *): Array[Char] = {
  //    return removeAll (array.asInstanceOf[Any], indices).asInstanceOf[Array[Char]]
  //  }
  //  def removeAll (array: Array[Double], indices: Int *): Array[Double] = {
  //    return removeAll (array.asInstanceOf[Any], indices).asInstanceOf[Array[Double]]
  //  }
  //  def removeAll (array: Array[Float], indices: Int *): Array[Float] = {
  //    return removeAll (array.asInstanceOf[Any], indices).asInstanceOf[Array[Float]]
  //  }
  //  def removeAll (array: Array[Int], indices: Int *): Array[Int] = {
  //    return removeAll (array.asInstanceOf[Any], indices).asInstanceOf[Array[Int]]
  //  }
  //  def removeAll (array: Array[Long], indices: Int *): Array[Long] = {
  //    return removeAll (array.asInstanceOf[Any], indices).asInstanceOf[Array[Long]]
  //  }
  //
  //  /**
  //    * Removes multiple array elements specified by indices.
  //    *
  //    * @param array   source
  //    * @param indices to remove
  //    * @return new array of same type minus elements specified by the set bits in {@code indices}
  //    * @since 3.2
  //    */
  //  // package protected for access by unit tests
  //  private[lang3] def removeAll (array: Any, indices: ju.BitSet): Any = {
  //    if (array == null) return null
  //    val srcLength: Int = getLength (array)
  //    // No need to check maxIndex here, because method only currently called from removeElements()
  //    // which guarantee to generate only valid bit entries.
  //    //        final int maxIndex = indices.length();
  //    //        if (maxIndex > srcLength) {
  //    //            throw new IndexOutOfBoundsException("Index: " + (maxIndex-1) + ", Length: " + srcLength);
  //    //        }
  //    val removals: Int = indices.cardinality // true bits are items to remove
  //    val result: Any = ReflectArray.newInstance (array.getClass.getComponentType, srcLength - removals)
  //    var srcIndex: Int = 0
  //    var destIndex: Int = 0
  //    var count: Int = 0
  //    var set: Int = 0
  //
  //    while ((set = indices.nextSetBit (srcIndex)) != -1) {
  //      count = set - srcIndex
  //      if (count > 0) {
  //        System.arraycopy (array, srcIndex, result, destIndex, count)
  //        destIndex += count
  //      }
  //      srcIndex = indices.nextClearBit (set)
  //    }
  //    count = srcLength - srcIndex
  //    if (count > 0) {
  //      System.arraycopy (array, srcIndex, result, destIndex, count)
  //    }
  //
  //    result
  //  }
  //
  //  /**
  //    * Removes multiple array elements specified by index.
  //    *
  //    * @param array   source
  //    * @param indices to remove
  //    * @return new array of same type minus elements specified by unique values of {@code indices}
  //    * @since 3.0.1
  //    */
  //  private[lang3] def removeAll (array: Any, indices: Int *): Any = {
  //    val length: Int = getLength (array)
  //    var diff: Int = 0 // number of distinct indexes, i.e. number of entries that will be removed
  //    val clonedIndices: Array[Int] = clone(indices.toArray)
  //    ju.Arrays.sort (clonedIndices)
  //    // identify length of result array
  //    if (isNotEmpty (clonedIndices) ) {
  //      var i: Int = clonedIndices.length
  //      var prevIndex: Int = length
  //
  //      var done: Boolean = false
  //      while ({i -= 1; i } >= 0 && !done) {
  //        val index: Int = clonedIndices (i)
  //        if (index < 0 || index >= length) {
  //          throw new IndexOutOfBoundsException ("Index: " + index + ", Length: " + length)
  //        }
  //        if (index >= prevIndex) done = true
  //        else {
  //          diff += 1
  //          prevIndex = index
  //        }
  //      }
  //    }
  //    // create result array
  //    val result: Any = ReflectArray.newInstance(array.getClass.getComponentType, length - diff)
  //
  //    if (diff < length) {
  //      var `end`: Int = length // index just after last copy
  //      var dest: Int = length - diff // number of entries so far not copied
  //      for (i <- clonedIndices.length - 1 to 0 by - 1) {
  //        val index: Int = clonedIndices (i)
  //        if (`end` - index > 1) { // same as (cp > 0)
  //          val cp: Int = `end` - index - 1
  //          dest -= cp
  //          System.arraycopy (array, index + 1, result, dest, cp)
  //          // After this copy, we still have room for dest items.
  //        }
  //        `end` = index
  //      }
  //      if (`end` > 0) {
  //        System.arraycopy (array, 0, result, 0, `end`)
  //      }
  //    }
  //
  //    result
  //  }
  //
  //  def removeAll (array: Array[Short], indices: Int*): Array[Short] =
  //    removeAll(array.asInstanceOf[Any], indices:_*).asInstanceOf[Array[Short]]
  //
  //  /**
  //    * <p>Removes the elements at the specified positions from the specified array.
  //    * All remaining elements are shifted to the left.
  //    *
  //    * <p>This method returns a new array with the same elements of the input
  //    * array except those at the specified positions. The component
  //    * type of the returned array is always the same as that of the input
  //    * array.
  //    *
  //    * <p>If the input array is {@code null}, an IndexOutOfBoundsException
  //    * will be thrown, because in that case no valid index can be specified.
  //    *
  //    * <pre>
  //    * ArrayUtils.removeAll(["a", "b", "c"], 0, 2) = ["b"]
  //    * ArrayUtils.removeAll(["a", "b", "c"], 1, 2) = ["a"]
  //    * </pre>
  //    *
  //    * @tparam T      the component type of the array
  //    * @param array   the array to remove the element from, may not be {@code null}
  //    * @param indices the positions of the elements to be removed
  //    * @return A new array containing the existing elements except those
  //    *         at the specified positions.
  //    * @throws IndexOutOfBoundsException if any index is out of range
  //    *                                   (index &lt; 0 || index &gt;= array.length), or if the array is {@code null}.
  //    * @since 3.0.1
  //    */
  //  @SuppressWarnings (Array ("unchecked") ) // removeAll() always creates an array of the same type as its input
  //  def removeAll[T](array: Array[T], indices: Int*): Array[T] =
  //    removeAll(array.asInstanceOf[Any], indices:_*).asInstanceOf[Array[T]]
  //
  //  /**
  //    * Removes the occurrences of the specified element from the specified boolean array.
  //    *
  //    * <p>
  //    * All subsequent elements are shifted to the left (subtracts one from their indices).
  //    * If the array doesn't contains such an element, no elements are removed from the array.
  //    * {@code null} will be returned if the input array is {@code null}.
  //    * </p>
  //    *
  //    * @param element the element to remove
  //    * @param array   the input array
  //    * @return A new array containing the existing elements except the occurrences of the specified element.
  //    * @since 3.5
  //    * @deprecated Use {@link #removeAllOccurrences ( boolean[ ], boolean)}
  //    */
  //  @deprecated def removeAllOccurences(array: Array[Boolean], element: Boolean): Array[Boolean] =
  //    removeAll(array.asInstanceOf[Any], indexesOf(array, element)).asInstanceOf[Array[Boolean]]
  //
  //
  //  /**
  //    * Removes the occurrences of the specified element from the specified byte array.
  //    *
  //    * <p>
  //    * All subsequent elements are shifted to the left (subtracts one from their indices).
  //    * If the array doesn't contains such an element, no elements are removed from the array.
  //    * {@code null} will be returned if the input array is {@code null}.
  //    * </p>
  //    *
  //    * @param element the element to remove
  //    * @param array   the input array
  //    * @return A new array containing the existing elements except the occurrences of the specified element.
  //    * @since 3.5
  //    * @deprecated Use {@link #removeAllOccurrences ( byte[ ], byte)}
  //    */
  //  @deprecated def removeAllOccurences(array: Array[Byte], element: Byte): Array[Byte] =
  //    removeAll(array.asInstanceOf[Any], indexesOf(array, element)).asInstanceOf[Array[Byte]]
  //
  //
  //  /**
  //    * Removes the occurrences of the specified element from the specified char array.
  //    *
  //    * <p>
  //    * All subsequent elements are shifted to the left (subtracts one from their indices).
  //    * If the array doesn't contains such an element, no elements are removed from the array.
  //    * {@code null} will be returned if the input array is {@code null}.
  //    * </p>
  //    *
  //    * @param element the element to remove
  //    * @param array   the input array
  //    * @return A new array containing the existing elements except the occurrences of the specified element.
  //    * @since 3.5
  //    * @deprecated Use {@link #removeAllOccurrences ( char[ ], char)}
  //    */
  //  @deprecated def removeAllOccurences(array: Array[Char], element: Char): Array[Char] =
  //    removeAll(array.asInstanceOf[Any], indexesOf(array, element)).asInstanceOf[Array[Char]]
  //
  //  /**
  //    * Removes the occurrences of the specified element from the specified double array.
  //    *
  //    * <p>
  //    * All subsequent elements are shifted to the left (subtracts one from their indices).
  //    * If the array doesn't contains such an element, no elements are removed from the array.
  //    * {@code null} will be returned if the input array is {@code null}.
  //    * </p>
  //    *
  //    * @param element the element to remove
  //    * @param array   the input array
  //    * @return A new array containing the existing elements except the occurrences of the specified element.
  //    * @since 3.5
  //    * @deprecated Use {@link #removeAllOccurrences ( double[ ], double)}
  //    */
  //  @deprecated def removeAllOccurences(array: Array[Double], element: Double): Array[Double] =
  //    removeAll(array.asInstanceOf[Any], indexesOf (array, element) ).asInstanceOf[Array[Double]]
  //
  //  /**
  //    * Removes the occurrences of the specified element from the specified float array.
  //    *
  //    * <p>
  //    * All subsequent elements are shifted to the left (subtracts one from their indices).
  //    * If the array doesn't contains such an element, no elements are removed from the array.
  //    * {@code null} will be returned if the input array is {@code null}.
  //    * </p>
  //    *
  //    * @param element the element to remove
  //    * @param array   the input array
  //    * @return A new array containing the existing elements except the occurrences of the specified element.
  //    * @since 3.5
  //    * @deprecated Use {@link #removeAllOccurrences ( float[ ], float)}
  //    */
  //  @deprecated def removeAllOccurences (array: Array[Float], element: Float): Array[Float] =
  //    removeAll(array.asInstanceOf[Any], indexesOf(array, element)).asInstanceOf[Array[Float]]
  //
  //  /**
  //    * Removes the occurrences of the specified element from the specified int array.
  //    *
  //    * <p>
  //    * All subsequent elements are shifted to the left (subtracts one from their indices).
  //    * If the array doesn't contains such an element, no elements are removed from the array.
  //    * {@code null} will be returned if the input array is {@code null}.
  //    * </p>
  //    *
  //    * @param element the element to remove
  //    * @param array   the input array
  //    * @return A new array containing the existing elements except the occurrences of the specified element.
  //    * @since 3.5
  //    * @deprecated Use {@link #removeAllOccurrences ( int[ ], int)}
  //    */
  //  @deprecated def removeAllOccurences (array: Array[Int], element: Int): Array[Int] =
  //    removeAll(array.asInstanceOf[Any], indexesOf (array, element)).asInstanceOf[Array[Int]]
  //
  //  /**
  //    * Removes the occurrences of the specified element from the specified long array.
  //    *
  //    * <p>
  //    * All subsequent elements are shifted to the left (subtracts one from their indices).
  //    * If the array doesn't contains such an element, no elements are removed from the array.
  //    * {@code null} will be returned if the input array is {@code null}.
  //    * </p>
  //    *
  //    * @param element the element to remove
  //    * @param array   the input array
  //    * @return A new array containing the existing elements except the occurrences of the specified element.
  //    * @since 3.5
  //    * @deprecated Use {@link #removeAllOccurrences ( long[ ], long)}
  //    */
  //  @deprecated def removeAllOccurences (array: Array[Long], element: Long): Array[Long] =
  //    removeAll(array.asInstanceOf[Any], indexesOf(array, element)).asInstanceOf[Array[Long]]
  //
  //  /**
  //    * Removes the occurrences of the specified element from the specified short array.
  //    *
  //    * <p>
  //    * All subsequent elements are shifted to the left (subtracts one from their indices).
  //    * If the array doesn't contains such an element, no elements are removed from the array.
  //    * {@code null} will be returned if the input array is {@code null}.
  //    * </p>
  //    *
  //    * @param element the element to remove
  //    * @param array   the input array
  //    * @return A new array containing the existing elements except the occurrences of the specified element.
  //    * @since 3.5
  //    * @deprecated Use {@link #removeAllOccurrences ( short[ ], short)}
  //    */
  //  @deprecated def removeAllOccurences (array: Array[Short], element: Short): Array[Short] =
  //    removeAll (array.asInstanceOf[Any], indexesOf (array, element) ).asInstanceOf[Array[Short]]
  //
  //  /**
  //    * Removes the occurrences of the specified element from the specified array.
  //    *
  //    * <p>
  //    * All subsequent elements are shifted to the left (subtracts one from their indices).
  //    * If the array doesn't contains such an element, no elements are removed from the array.
  //    * {@code null} will be returned if the input array is {@code null}.
  //    * </p>
  //    *
  //    * @param T       the type of object in the array
  //    * @param element the element to remove
  //    * @param array   the input array
  //    * @return A new array containing the existing elements except the occurrences of the specified element.
  //    * @since 3.5
  //    * @deprecated Use {@link #removeAllOccurrences ( Object[ ], Object)}
  //    */
  //  @deprecated def removeAllOccurences[T] (array: Array[T], element: T): Array[T] =
  //    removeAll(array.asInstanceOf[Any], indexesOf(array, element)).asInstanceOf[Array[T]]
  //
  //
  //  /**
  //    * Removes the occurrences of the specified element from the specified boolean array.
  //    *
  //    * <p>
  //    * All subsequent elements are shifted to the left (subtracts one from their indices).
  //    * If the array doesn't contains such an element, no elements are removed from the array.
  //    * {@code null} will be returned if the input array is {@code null}.
  //    * </p>
  //    *
  //    * @param element the element to remove
  //    * @param array   the input array
  //    * @return A new array containing the existing elements except the occurrences of the specified element.
  //    * @since 3.10
  //    */
  //  def removeAllOccurrences (array: Array[Boolean], element: Boolean): Array[Boolean] =
  //    removeAll(array.asInstanceOf[Any], indexesOf(array, element)).asInstanceOf[Array[Boolean]]
  //
  //  /**
  //    * Removes the occurrences of the specified element from the specified byte array.
  //    *
  //    * <p>
  //    * All subsequent elements are shifted to the left (subtracts one from their indices).
  //    * If the array doesn't contains such an element, no elements are removed from the array.
  //    * {@code null} will be returned if the input array is {@code null}.
  //    * </p>
  //    *
  //    * @param element the element to remove
  //    * @param array   the input array
  //    * @return A new array containing the existing elements except the occurrences of the specified element.
  //    * @since 3.10
  //    */
  //  def removeAllOccurrences (array: Array[Byte], element: Byte): Array[Byte] =
  //    removeAll(array.asInstanceOf[Any], indexesOf(array, element)).asInstanceOf[Array[Byte]]
  //
  //
  //  /**
  //    * Removes the occurrences of the specified element from the specified char array.
  //    *
  //    * <p>
  //    * All subsequent elements are shifted to the left (subtracts one from their indices).
  //    * If the array doesn't contains such an element, no elements are removed from the array.
  //    * {@code null} will be returned if the input array is {@code null}.
  //    * </p>
  //    *
  //    * @param element the element to remove
  //    * @param array   the input array
  //    * @return A new array containing the existing elements except the occurrences of the specified element.
  //    * @since 3.10
  //    */
  //  def removeAllOccurrences (array: Array[Char], element: Char): Array[Char] =
  //    removeAll (array.asInstanceOf[Any], indexesOf(array, element)).asInstanceOf[Array[Char]]
  //
  //  /**
  //    * Removes the occurrences of the specified element from the specified double array.
  //    *
  //    * <p>
  //    * All subsequent elements are shifted to the left (subtracts one from their indices).
  //    * If the array doesn't contains such an element, no elements are removed from the array.
  //    * {@code null} will be returned if the input array is {@code null}.
  //    * </p>
  //    *
  //    * @param element the element to remove
  //    * @param array   the input array
  //    * @return A new array containing the existing elements except the occurrences of the specified element.
  //    * @since 3.10
  //    */
  //  def removeAllOccurrences (array: Array[Double], element: Double): Array[Double] =
  //    removeAll (array.asInstanceOf[Any], indexesOf(array, element)).asInstanceOf[Array[Double]]
  //
  //  /**
  //    * Removes the occurrences of the specified element from the specified float array.
  //    *
  //    * <p>
  //    * All subsequent elements are shifted to the left (subtracts one from their indices).
  //    * If the array doesn't contains such an element, no elements are removed from the array.
  //    * {@code null} will be returned if the input array is {@code null}.
  //    * </p>
  //    *
  //    * @param element the element to remove
  //    * @param array   the input array
  //    * @return A new array containing the existing elements except the occurrences of the specified element.
  //    * @since 3.10
  //    */
  //  def removeAllOccurrences (array: Array[Float], element: Float): Array[Float] =
  //    removeAll (array.asInstanceOf[Any], indexesOf(array, element)).asInstanceOf[Array[Float]]
  //
  //  /**
  //    * Removes the occurrences of the specified element from the specified int array.
  //    *
  //    * <p>
  //    * All subsequent elements are shifted to the left (subtracts one from their indices).
  //    * If the array doesn't contains such an element, no elements are removed from the array.
  //    * {@code null} will be returned if the input array is {@code null}.
  //    * </p>
  //    *
  //    * @param element the element to remove
  //    * @param array   the input array
  //    * @return A new array containing the existing elements except the occurrences of the specified element.
  //    * @since 3.10
  //    */
  //  def removeAllOccurrences (array: Array[Int], element: Int): Array[Int] =
  //    removeAll(array.asInstanceOf[Any], indexesOf(array, element)).asInstanceOf[Array[Int]]
  //
  //  /**
  //    * Removes the occurrences of the specified element from the specified long array.
  //    *
  //    * <p>
  //    * All subsequent elements are shifted to the left (subtracts one from their indices).
  //    * If the array doesn't contains such an element, no elements are removed from the array.
  //    * {@code null} will be returned if the input array is {@code null}.
  //    * </p>
  //    *
  //    * @param element the element to remove
  //    * @param array   the input array
  //    * @return A new array containing the existing elements except the occurrences of the specified element.
  //    * @since 3.10
  //    */
  //  def removeAllOccurrences (array: Array[Long], element: Long): Array[Long] =
  //    removeAll(array.asInstanceOf[Any], indexesOf(array, element)).asInstanceOf[Array[Long]]
  //
  //  /**
  //    * Removes the occurrences of the specified element from the specified short array.
  //    *
  //    * <p>
  //    * All subsequent elements are shifted to the left (subtracts one from their indices).
  //    * If the array doesn't contains such an element, no elements are removed from the array.
  //    * {@code null} will be returned if the input array is {@code null}.
  //    * </p>
  //    *
  //    * @param element the element to remove
  //    * @param array   the input array
  //    * @return A new array containing the existing elements except the occurrences of the specified element.
  //    * @since 3.10
  //    */
  //  def removeAllOccurrences (array: Array[Short], element: Short): Array[Short] =
  //    removeAll (array.asInstanceOf[Any], indexesOf (array, element) ).asInstanceOf[Array[Short]]
  //
  //  /**
  //    * Removes the occurrences of the specified element from the specified array.
  //    *
  //    * <p>
  //    * All subsequent elements are shifted to the left (subtracts one from their indices).
  //    * If the array doesn't contains such an element, no elements are removed from the array.
  //    * {@code null} will be returned if the input array is {@code null}.
  //    * </p>
  //    *
  //    * @tparam T      the type of object in the array
  //    * @param element the element to remove
  //    * @param array   the input array
  //    * @return A new array containing the existing elements except the occurrences of the specified element.
  //    * @since 3.10
  //    */
  //  def removeAllOccurrences[T] (array: Array[T], element: T): Array[T] =
  //    removeAll(array.asInstanceOf[Any], indexesOf(array, element)).asInstanceOf[Array[T]]
  //
  //  /**
  //    * <p>Removes the first occurrence of the specified element from the
  //    * specified array. All subsequent elements are shifted to the left
  //    * (subtracts one from their indices). If the array doesn't contains
  //    * such an element, no elements are removed from the array.
  //    *
  //    * <p>This method returns a new array with the same elements of the input
  //    * array except the first occurrence of the specified element. The component
  //    * type of the returned array is always the same as that of the input
  //    * array.
  //    *
  //    * <pre>
  //    * ArrayUtils.removeElement(null, true)                = null
  //    * ArrayUtils.removeElement([], true)                  = []
  //    * ArrayUtils.removeElement([true], false)             = [true]
  //    * ArrayUtils.removeElement([true, false], false)      = [true]
  //    * ArrayUtils.removeElement([true, false, true], true) = [false, true]
  //    * </pre>
  //    *
  //    * @param array   the array to remove the element from, may be {@code null}
  //    * @param element the element to be removed
  //    * @return A new array containing the existing elements except the first
  //    *         occurrence of the specified element.
  //    * @since 2.1
  //    */
  //  def removeElement (array: Array[Boolean], element: Boolean): Array[Boolean] = {
  //    val index: Int = indexOf(array, element)
  //    if (index == INDEX_NOT_FOUND) clone(array)
  //    else remove(array, index)
  //  }
  //
  //  /**
  //    * <p>Removes the first occurrence of the specified element from the
  //    * specified array. All subsequent elements are shifted to the left
  //    * (subtracts one from their indices). If the array doesn't contains
  //    * such an element, no elements are removed from the array.
  //    *
  //    * <p>This method returns a new array with the same elements of the input
  //    * array except the first occurrence of the specified element. The component
  //    * type of the returned array is always the same as that of the input
  //    * array.
  //    *
  //    * <pre>
  //    * ArrayUtils.removeElement(null, 1)        = null
  //    * ArrayUtils.removeElement([], 1)          = []
  //    * ArrayUtils.removeElement([1], 0)         = [1]
  //    * ArrayUtils.removeElement([1, 0], 0)      = [1]
  //    * ArrayUtils.removeElement([1, 0, 1], 1)   = [0, 1]
  //    * </pre>
  //    *
  //    * @param array   the array to remove the element from, may be {@code null}
  //    * @param element the element to be removed
  //    * @return A new array containing the existing elements except the first
  //    *         occurrence of the specified element.
  //    * @since 2.1
  //    */
  //  def removeElement (array: Array[Byte], element: Byte): Array[Byte] = {
  //    val index: Int = indexOf (array, element)
  //    if (index == INDEX_NOT_FOUND) clone(array)
  //    else remove(array, index)
  //  }
  //
  //  /**
  //    * <p>Removes the first occurrence of the specified element from the
  //    * specified array. All subsequent elements are shifted to the left
  //    * (subtracts one from their indices). If the array doesn't contains
  //    * such an element, no elements are removed from the array.
  //    *
  //    * <p>This method returns a new array with the same elements of the input
  //    * array except the first occurrence of the specified element. The component
  //    * type of the returned array is always the same as that of the input
  //    * array.
  //    *
  //    * <pre>
  //    * ArrayUtils.removeElement(null, 'a')            = null
  //    * ArrayUtils.removeElement([], 'a')              = []
  //    * ArrayUtils.removeElement(['a'], 'b')           = ['a']
  //    * ArrayUtils.removeElement(['a', 'b'], 'a')      = ['b']
  //    * ArrayUtils.removeElement(['a', 'b', 'a'], 'a') = ['b', 'a']
  //    * </pre>
  //    *
  //    * @param array   the array to remove the element from, may be {@code null}
  //    * @param element the element to be removed
  //    * @return A new array containing the existing elements except the first
  //    *         occurrence of the specified element.
  //    * @since 2.1
  //    */
  //  def removeElement (array: Array[Char], element: Char): Array[Char] = {
  //    val index: Int = indexOf (array, element)
  //    if (index == INDEX_NOT_FOUND) clone (array)
  //    else remove (array, index)
  //  }
  //
  //  /**
  //    * <p>Removes the first occurrence of the specified element from the
  //    * specified array. All subsequent elements are shifted to the left
  //    * (subtracts one from their indices). If the array doesn't contains
  //    * such an element, no elements are removed from the array.
  //    *
  //    * <p>This method returns a new array with the same elements of the input
  //    * array except the first occurrence of the specified element. The component
  //    * type of the returned array is always the same as that of the input
  //    * array.
  //    *
  //    * <pre>
  //    * ArrayUtils.removeElement(null, 1.1)            = null
  //    * ArrayUtils.removeElement([], 1.1)              = []
  //    * ArrayUtils.removeElement([1.1], 1.2)           = [1.1]
  //    * ArrayUtils.removeElement([1.1, 2.3], 1.1)      = [2.3]
  //    * ArrayUtils.removeElement([1.1, 2.3, 1.1], 1.1) = [2.3, 1.1]
  //    * </pre>
  //    *
  //    * @param array   the array to remove the element from, may be {@code null}
  //    * @param element the element to be removed
  //    * @return A new array containing the existing elements except the first
  //    *         occurrence of the specified element.
  //    * @since 2.1
  //    */
  //  def removeElement (array: Array[Double], element: Double): Array[Double] = {
  //    val index: Int = indexOf (array, element)
  //    if (index == INDEX_NOT_FOUND) clone(array)
  //    else remove(array, index)
  //  }
  //
  //  def removeElement (array: Array[Float], element: Float): Array[Float] = {
  //    val index: Int = indexOf (array, element)
  //    if (index == INDEX_NOT_FOUND) clone(array)
  //    else remove(array, index)
  //  }
  //
  //  /**
  //    * <p>Removes the first occurrence of the specified element from the
  //    * specified array. All subsequent elements are shifted to the left
  //    * (subtracts one from their indices). If the array doesn't contains
  //    * such an element, no elements are removed from the array.
  //    *
  //    * <p>This method returns a new array with the same elements of the input
  //    * array except the first occurrence of the specified element. The component
  //    * type of the returned array is always the same as that of the input
  //    * array.
  //    *
  //    * <pre>
  //    * ArrayUtils.removeElement(null, 1)      = null
  //    * ArrayUtils.removeElement([], 1)        = []
  //    * ArrayUtils.removeElement([1], 2)       = [1]
  //    * ArrayUtils.removeElement([1, 3], 1)    = [3]
  //    * ArrayUtils.removeElement([1, 3, 1], 1) = [3, 1]
  //    * </pre>
  //    *
  //    * @param array   the array to remove the element from, may be {@code null}
  //    * @param element the element to be removed
  //    * @return A new array containing the existing elements except the first
  //    *         occurrence of the specified element.
  //    * @since 2.1
  //    */
  //  def removeElement (array: Array[Int], element: Int): Array[Int] = {
  //    val index: Int = indexOf (array, element)
  //    if (index == INDEX_NOT_FOUND) clone(array)
  //    else remove(array, index)
  //  }
  //
  //  def removeElement (array: Array[Long], element: Long): Array[Long] = {
  //    val index: Int = indexOf (array, element)
  //    if (index == INDEX_NOT_FOUND) clone(array)
  //    else remove(array, index)
  //  }
  //
  //  def removeElement (array: Array[Short], element: Short): Array[Short] = {
  //    val index: Int = indexOf (array, element)
  //    if (index == INDEX_NOT_FOUND) clone(array)
  //    else remove(array, index)
  //  }
  //
  //  /**
  //    * <p>Removes the first occurrence of the specified element from the
  //    * specified array. All subsequent elements are shifted to the left
  //    * (subtracts one from their indices). If the array doesn't contains
  //    * such an element, no elements are removed from the array.
  //    *
  //    * <p>This method returns a new array with the same elements of the input
  //    * array except the first occurrence of the specified element. The component
  //    * type of the returned array is always the same as that of the input
  //    * array.
  //    *
  //    * <pre>
  //    * ArrayUtils.removeElement(null, "a")            = null
  //    * ArrayUtils.removeElement([], "a")              = []
  //    * ArrayUtils.removeElement(["a"], "b")           = ["a"]
  //    * ArrayUtils.removeElement(["a", "b"], "a")      = ["b"]
  //    * ArrayUtils.removeElement(["a", "b", "a"], "a") = ["b", "a"]
  //    * </pre>
  //    *
  //    * @tparam T      the component type of the array
  //    * @param array   the array to remove the element from, may be {@code null}
  //    * @param element the element to be removed
  //    * @return A new array containing the existing elements except the first
  //    *         occurrence of the specified element.
  //    * @since 2.1
  //    */
  //  def removeElement[T](array: Array[T], element: Any): Array[T] = {
  //    val index: Int = indexOf(array, element)
  //    if (index == INDEX_NOT_FOUND) clone(array)
  //    else remove(array, index)
  //  }
  //
  //  /**
  //    * <p>Removes occurrences of specified elements, in specified quantities,
  //    * from the specified array. All subsequent elements are shifted left.
  //    * For any element-to-be-removed specified in greater quantities than
  //    * contained in the original array, no change occurs beyond the
  //    * removal of the existing matching items.
  //    *
  //    * <p>This method returns a new array with the same elements of the input
  //    * array except for the earliest-encountered occurrences of the specified
  //    * elements. The component type of the returned array is always the same
  //    * as that of the input array.
  //    *
  //    * <pre>
  //    * ArrayUtils.removeElements(null, true, false)               = null
  //    * ArrayUtils.removeElements([], true, false)                 = []
  //    * ArrayUtils.removeElements([true], false, false)            = [true]
  //    * ArrayUtils.removeElements([true, false], true, true)       = [false]
  //    * ArrayUtils.removeElements([true, false, true], true)       = [false, true]
  //    * ArrayUtils.removeElements([true, false, true], true, true) = [false]
  //    * </pre>
  //    *
  //    * @param array  the array to remove the element from, may be {@code null}
  //    * @param values the elements to be removed
  //    * @return A new array containing the existing elements except the
  //    *         earliest-encountered occurrences of the specified elements.
  //    * @since 3.0.1
  //    */
  //  def removeElements (array: Array[Boolean], values: Boolean *): Array[Boolean] = {
  //    if (isEmpty (array) || isEmpty(values)) return clone(array)
  //
  //    val occurrences: ju.HashMap[Boolean, MutableInt] = new ju.HashMap[Boolean, MutableInt] (2) // only two possible values here
  //    for (v <- values) {
  //      val boxed: Boolean = JavaBoolean.valueOf(v)
  //      val count: MutableInt = occurrences.get (boxed)
  //      if (count == null) occurrences.put (boxed, new MutableInt (1) )
  //      else count.increment ()
  //    }
  //
  //    val toRemove: ju.BitSet = new ju.BitSet
  //    for (i <- 0 until array.length) {
  //      val key: Boolean = array(i)
  //      val count: MutableInt = occurrences.get (key)
  //      if (count != null) {
  //        if (count.decrementAndGet == 0) occurrences.remove (key)
  //        toRemove.set(i)
  //      }
  //    }
  //
  //    removeAll (array, toRemove).asInstanceOf[Array[Boolean]]
  //  }
  //
  //  /**
  //    * <p>Removes occurrences of specified elements, in specified quantities,
  //    * from the specified array. All subsequent elements are shifted left.
  //    * For any element-to-be-removed specified in greater quantities than
  //    * contained in the original array, no change occurs beyond the
  //    * removal of the existing matching items.
  //    *
  //    * <p>This method returns a new array with the same elements of the input
  //    * array except for the earliest-encountered occurrences of the specified
  //    * elements. The component type of the returned array is always the same
  //    * as that of the input array.
  //    *
  //    * <pre>
  //    * ArrayUtils.removeElements(null, 1, 2)      = null
  //    * ArrayUtils.removeElements([], 1, 2)        = []
  //    * ArrayUtils.removeElements([1], 2, 3)       = [1]
  //    * ArrayUtils.removeElements([1, 3], 1, 2)    = [3]
  //    * ArrayUtils.removeElements([1, 3, 1], 1)    = [3, 1]
  //    * ArrayUtils.removeElements([1, 3, 1], 1, 1) = [3]
  //    * </pre>
  //    *
  //    * @param array  the array to remove the element from, may be {@code null}
  //    * @param values the elements to be removed
  //    * @return A new array containing the existing elements except the
  //    *         earliest-encountered occurrences of the specified elements.
  //    * @since 3.0.1
  //    */
  //  def removeElements (array: Array[Byte], values: Byte *): Array[Byte] = {
  //    if (isEmpty (array) || isEmpty(values)) return clone(array)
  //
  //    val occurrences: ju.Map[Byte, MutableInt] = new ju.HashMap[Byte, MutableInt] (values.length)
  //    for (v <- values) {
  //      val boxed: Byte = JavaByte.valueOf(v)
  //      val count: MutableInt = occurrences.get (boxed)
  //      if (count == null) {
  //        occurrences.put (boxed, new MutableInt (1) )
  //      }
  //      else {
  //        count.increment ()
  //      }
  //    }
  //    val toRemove: ju.BitSet = new ju.BitSet
  //    for (i <- 0 until array.length) {
  //      val key: Byte = array (i)
  //      val count: MutableInt = occurrences.get (key)
  //      if (count != null) {
  //        if (count.decrementAndGet == 0) {
  //          occurrences.remove (key)
  //        }
  //        toRemove.set (i)
  //      }
  //    }
  //    return removeAll (array, toRemove).asInstanceOf[Array[Byte]]
  //  }
  //  def removeElements (array: Array[Char], values: Char *): Array[Char] = {
  //    if (isEmpty (array) || isEmpty(values)) return clone(array)
  //
  //    val occurrences: ju.HashMap[Character, MutableInt] = new ju.HashMap[Character, MutableInt] (values.length)
  //    for (v <- values) {
  //      val boxed: Character = Character.valueOf (v)
  //      val count: MutableInt = occurrences.get (boxed)
  //      if (count == null) {
  //        occurrences.put (boxed, new MutableInt (1) )
  //      }
  //      else {
  //        count.increment ()
  //      }
  //    }
  //    val toRemove: ju.BitSet = new ju.BitSet
  //    for (i <- 0 until array.length) {
  //      val key: Char = array (i)
  //      val count: MutableInt = occurrences.get (key)
  //      if (count != null) {
  //        if (count.decrementAndGet == 0) {
  //          occurrences.remove (key)
  //        }
  //        toRemove.set (i)
  //      }
  //    }
  //    return removeAll (array, toRemove).asInstanceOf[Array[Char]]
  //  }
  //  def removeElements (array: Array[Double], values: Double *): Array[Double] = {
  //    if (isEmpty (array) || isEmpty (values) ) {
  //      return clone (array)
  //    }
  //    val occurrences: ju.HashMap[Double, MutableInt] = new ju.HashMap[Double, MutableInt] (values.length)
  //    for (v <- values) {
  //      val boxed: Double = Double.valueOf (v)
  //      val count: MutableInt = occurrences.get (boxed)
  //      if (count == null) {
  //        occurrences.put (boxed, new MutableInt (1) )
  //      }
  //      else {
  //        count.increment ()
  //      }
  //    }
  //    val toRemove: ju.BitSet = new ju.BitSet
  //    for (i <- 0 until array.length) {
  //      val key: Double = array (i)
  //      val count: MutableInt = occurrences.get (key)
  //      if (count != null) {
  //        if (count.decrementAndGet == 0) {
  //          occurrences.remove (key)
  //        }
  //        toRemove.set (i)
  //      }
  //    }
  //    return removeAll (array, toRemove).asInstanceOf[Array[Double]]
  //  }
  //  def removeElements (array: Array[Float], values: Float *): Array[Float] = {
  //    if (isEmpty (array) || isEmpty(values)) clone (array)
  //
  //    val occurrences: ju.HashMap[Float, MutableInt] = new ju.HashMap[Float, MutableInt] (values.length)
  //    for (v <- values) {
  //      val boxed: Float = JavaFloat.valueOf (v)
  //      val count: MutableInt = occurrences.get (boxed)
  //      if (count == null) {
  //        occurrences.put (boxed, new MutableInt (1) )
  //      }
  //      else {
  //        count.increment ()
  //      }
  //    }
  //    val toRemove: ju.BitSet = new ju.BitSet
  //    for (i <- 0 until array.length) {
  //      val key: Float = array (i)
  //      val count: MutableInt = occurrences.get (key)
  //      if (count != null) {
  //        if (count.decrementAndGet == 0) {
  //          occurrences.remove (key)
  //        }
  //        toRemove.set (i)
  //      }
  //    }
  //    return removeAll (array, toRemove).asInstanceOf[Array[Float]]
  //  }
  //  def removeElements (array: Array[Int], values: Int *): Array[Int] = {
  //    if (isEmpty (array) || isEmpty(values)) return clone(array)
  //
  //    val occurrences: ju.HashMap[Integer, MutableInt] = new ju.HashMap[Integer, MutableInt] (values.length)
  //    for (v <- values) {
  //      val boxed: Integer = Integer.valueOf (v)
  //      val count: MutableInt = occurrences.get (boxed)
  //      if (count == null) {
  //        occurrences.put (boxed, new MutableInt (1) )
  //      }
  //      else {
  //        count.increment ()
  //      }
  //    }
  //    val toRemove: ju.BitSet = new ju.BitSet
  //    for (i <- 0 until array.length) {
  //      val key: Int = array (i)
  //      val count: MutableInt = occurrences.get (key)
  //      if (count != null) {
  //        if (count.decrementAndGet == 0) {
  //          occurrences.remove (key)
  //        }
  //        toRemove.set (i)
  //      }
  //    }
  //    return removeAll (array, toRemove).asInstanceOf[Array[Int]]
  //  }
  //  def removeElements (array: Array[Long], values: Long *): Array[Long] = {
  //    if (isEmpty (array) || isEmpty(values)) return clone(array)
  //
  //    val occurrences: ju.HashMap[Long, MutableInt] = new ju.HashMap[Long, MutableInt] (values.length)
  //    for (v <- values) {
  //      val boxed: Long = JavaLong.valueOf(v)
  //      val count: MutableInt = occurrences.get (boxed)
  //      if (count == null) {
  //        occurrences.put (boxed, new MutableInt (1) )
  //      }
  //      else {
  //        count.increment ()
  //      }
  //    }
  //    val toRemove: ju.BitSet = new ju.BitSet
  //    for (i <- 0 until array.length) {
  //      val key: Long = array (i)
  //      val count: MutableInt = occurrences.get (key)
  //      if (count != null) {
  //        if (count.decrementAndGet == 0) {
  //          occurrences.remove (key)
  //        }
  //        toRemove.set (i)
  //      }
  //    }
  //    return removeAll (array, toRemove).asInstanceOf[Array[Long]]
  //  }
  //  def removeElements (array: Array[Short], values: Short *): Array[Short] = {
  //    if (isEmpty (array) || isEmpty(values)) return clone (array)
  //
  //    val occurrences: ju.HashMap[Short, MutableInt] = new ju.HashMap[Short, MutableInt] (values.length)
  //    for (v <- values) {
  //      val boxed: Short = JavaShort.valueOf(v)
  //      val count: MutableInt = occurrences.get (boxed)
  //      if (count == null) {
  //        occurrences.put (boxed, new MutableInt (1) )
  //      }
  //      else {
  //        count.increment ()
  //      }
  //    }
  //    val toRemove: ju.BitSet = new ju.BitSet
  //    for (i <- 0 until array.length) {
  //      val key: Short = array (i)
  //      val count: MutableInt = occurrences.get (key)
  //      if (count != null) {
  //        if (count.decrementAndGet == 0) {
  //          occurrences.remove (key)
  //        }
  //        toRemove.set (i)
  //      }
  //    }
  //    return removeAll (array, toRemove).asInstanceOf[Array[Short]]
  //  }
  //
  //  /**
  //    * <p>Removes occurrences of specified elements, in specified quantities,
  //    * from the specified array. All subsequent elements are shifted left.
  //    * For any element-to-be-removed specified in greater quantities than
  //    * contained in the original array, no change occurs beyond the
  //    * removal of the existing matching items.
  //    *
  //    * <p>This method returns a new array with the same elements of the input
  //    * array except for the earliest-encountered occurrences of the specified
  //    * elements. The component type of the returned array is always the same
  //    * as that of the input array.
  //    *
  //    * <pre>
  //    * ArrayUtils.removeElements(null, "a", "b")            = null
  //    * ArrayUtils.removeElements([], "a", "b")              = []
  //    * ArrayUtils.removeElements(["a"], "b", "c")           = ["a"]
  //    * ArrayUtils.removeElements(["a", "b"], "a", "c")      = ["b"]
  //    * ArrayUtils.removeElements(["a", "b", "a"], "a")      = ["b", "a"]
  //    * ArrayUtils.removeElements(["a", "b", "a"], "a", "a") = ["b"]
  //    * </pre>
  //    *
  //    * @tparam T     the component type of the array
  //    * @param array  the array to remove the element from, may be {@code null}
  //    * @param values the elements to be removed
  //    * @return A new array containing the existing elements except the
  //    *         earliest-encountered occurrences of the specified elements.
  //    * @since 3.0.1
  //    */
  //  @SafeVarargs def removeElements[T] (array: Array[T], values: T*): Array[T] = {
  //    if (isEmpty(array) || isEmpty(values)) return clone (array)
  //
  //    val occurrences: ju.HashMap[T, MutableInt] = new ju.HashMap[T, MutableInt] (values.length)
  //    for (v <- values) {
  //      val count: MutableInt = occurrences.get (v)
  //      if (count == null) {
  //        occurrences.put (v, new MutableInt (1) )
  //      }
  //      else {
  //        count.increment ()
  //      }
  //    }
  //    val toRemove: ju.BitSet = new ju.BitSet
  //    for (i <- 0 until array.length) {
  //      val key: T = array (i)
  //      val count: MutableInt = occurrences.get (key)
  //      if (count != null) {
  //        if (count.decrementAndGet == 0) {
  //          occurrences.remove (key)
  //        }
  //        toRemove.set (i)
  //      }
  //    }
  //    @SuppressWarnings (Array ("unchecked") ) val result: Array[T] = removeAll (array, toRemove).asInstanceOf[Array[T]]
  //    return result
  //  }
  //
  //  /**
  //    * <p>Reverses the order of the given array.
  //    *
  //    * <p>This method does nothing for a {@code null} input array.
  //    *
  //    * @param array the array to reverse, may be {@code null}
  //    */
  //  def reverse (array: Array[Boolean] ): Unit = {
  //    if (array == null) {
  //      return
  //    }
  //    reverse (array, 0, array.length)
  //  }
  //
  //  /**
  //    * <p>
  //    * Reverses the order of the given array in the given range.
  //    *
  //    * <p>
  //    * This method does nothing for a {@code null} input array.
  //    *
  //    * @param array
  //    * the array to reverse, may be {@code null}
  //    * @param startIndexInclusive
  //    * the starting index. Undervalue (&lt;0) is promoted to 0, overvalue (&gt;array.length) results in no
  //    * change.
  //    * @param endIndexExclusive
  //    * elements up to endIndex-1 are reversed in the array. Undervalue (&lt; start index) results in no
  //    * change. Overvalue (&gt;array.length) is demoted to array length.
  //    * @since 3.2
  //    */
  //  def reverse (array: Array[Boolean], startIndexInclusive: Int, endIndexExclusive: Int): Unit = {
  //    if (array == null) return
  //
  //    var i: Int = Math.max (startIndexInclusive, 0)
  //    var j: Int = Math.min (array.length, endIndexExclusive) - 1
  //    var tmp: Boolean = false
  //
  //    while (j > i) {
  //      tmp = array (j)
  //      array (j) = array (i)
  //      array (i) = tmp
  //      j -= 1
  //      i += 1
  //    }
  //  }
  //
  //  def reverse (array: Array[Byte] ): Unit = {
  //    if (array == null) return
  //
  //    reverse(array, 0, array.length)
  //  }
  //
  //  def reverse (array: Array[Byte], startIndexInclusive: Int, endIndexExclusive: Int): Unit = {
  //    if (array == null) return
  //
  //    var i: Int = Math.max (startIndexInclusive, 0)
  //    var j: Int = Math.min (array.length, endIndexExclusive) - 1
  //    var tmp: Byte = 0
  //
  //    while (j > i) {
  //      tmp = array (j)
  //      array (j) = array (i)
  //      array (i) = tmp
  //      j -= 1
  //      i += 1
  //    }
  //  }
  //
  //  def reverse (array: Array[Char] ): Unit = {
  //    if (array == null) return
  //
  //    reverse(array, 0, array.length)
  //  }
  //
  //  def reverse (array: Array[Char], startIndexInclusive: Int, endIndexExclusive: Int): Unit = {
  //    if (array == null) return
  //
  //    var i: Int = Math.max (startIndexInclusive, 0)
  //    var j: Int = Math.min (array.length, endIndexExclusive) - 1
  //    var tmp: Char = 0
  //
  //    while (j > i) {
  //      tmp = array (j)
  //      array (j) = array (i)
  //      array (i) = tmp
  //      j -= 1
  //      i += 1
  //    }
  //  }
  //
  //  def reverse (array: Array[Double] ): Unit = {
  //    if (array == null) return
  //
  //    reverse (array, 0, array.length)
  //  }
  //
  //  def reverse (array: Array[Double], startIndexInclusive: Int, endIndexExclusive: Int): Unit = {
  //    if (array == null) {
  //      return
  //    }
  //    var i: Int = Math.max (startIndexInclusive, 0)
  //    var j: Int = Math.min (array.length, endIndexExclusive) - 1
  //    var tmp: Double = .0
  //
  //    while (j > i) {
  //      tmp = array (j)
  //      array (j) = array (i)
  //      array (i) = tmp
  //      j -= 1
  //      i += 1
  //    }
  //  }
  //
  //  def reverse (array: Array[Float] ): Unit = {
  //    if (array == null) {
  //      return
  //    }
  //    reverse(array, 0, array.length)
  //  }
  //
  //  def reverse (array: Array[Float], startIndexInclusive: Int, endIndexExclusive: Int): Unit = {
  //    if (array == null) return
  //
  //    var i: Int = Math.max (startIndexInclusive, 0)
  //    var j: Int = Math.min (array.length, endIndexExclusive) - 1
  //    var tmp: Float = 0.0f
  //
  //
  //    while (j > i) {
  //      tmp = array (j)
  //      array (j) = array (i)
  //      array (i) = tmp
  //      j -= 1
  //      i += 1
  //    }
  //  }
  //
  //  def reverse (array: Array[Int] ): Unit = {
  //    if (array == null) return
  //
  //    reverse(array, 0, array.length)
  //  }
  //  def reverse (array: Array[Int], startIndexInclusive: Int, endIndexExclusive: Int): Unit = {
  //    if (array == null) return
  //
  //    var i: Int = Math.max (startIndexInclusive, 0)
  //    var j: Int = Math.min (array.length, endIndexExclusive) - 1
  //    var tmp: Int = 0
  //
  //    while (j > i) {
  //      tmp = array (j)
  //      array (j) = array (i)
  //      array (i) = tmp
  //      j -= 1
  //      i += 1
  //    }
  //  }
  //
  //  def reverse (array: Array[Long] ): Unit = {
  //    if (array == null) return
  //    reverse (array, 0, array.length)
  //  }
  //
  //  def reverse (array: Array[Long], startIndexInclusive: Int, endIndexExclusive: Int): Unit = {
  //    if (array == null) return
  //
  //    var i: Int = Math.max (startIndexInclusive, 0)
  //    var j: Int = Math.min (array.length, endIndexExclusive) - 1
  //    var tmp: Long = 0L
  //
  //    while (j > i) {
  //      tmp = array (j)
  //      array (j) = array (i)
  //      array (i) = tmp
  //      j -= 1
  //      i += 1
  //    }
  //  }
  //
  //  /**
  //    * <p>Reverses the order of the given array.
  //    *
  //    * <p>There is no special handling for multi-dimensional arrays.
  //    *
  //    * <p>This method does nothing for a {@code null} input array.
  //    *
  //    * @param array the array to reverse, may be {@code null}
  //    */
  //  def reverse (array: Array[AnyRef] ): Unit = {
  //    if (array == null) return
  //
  //    reverse(array, 0, array.length)
  //  }
  //
  //  /**
  //    * <p>
  //    * Reverses the order of the given array in the given range.
  //    *
  //    * <p>
  //    * This method does nothing for a {@code null} input array.
  //    *
  //    * @param array
  //    * the array to reverse, may be {@code null}
  //    * @param startIndexInclusive
  //    * the starting index. Under value (&lt;0) is promoted to 0, over value (&gt;array.length) results in no
  //    * change.
  //    * @param endIndexExclusive
  //    * elements up to endIndex-1 are reversed in the array. Under value (&lt; start index) results in no
  //    * change. Over value (&gt;array.length) is demoted to array length.
  //    * @since 3.2
  //    */
  //  def reverse (array: Array[AnyRef], startIndexInclusive: Int, endIndexExclusive: Int): Unit = {
  //    if (array == null) return
  //
  //    var i: Int = Math.max (startIndexInclusive, 0)
  //    var j: Int = Math.min (array.length, endIndexExclusive) - 1
  //    var tmp: AnyRef = null
  //
  //    while (j > i) {
  //      tmp = array(j)
  //      array(j) = array(i)
  //      array(i) = tmp
  //      j -= 1
  //      i += 1
  //    }
  //  }
  //  def reverse (array: Array[Short] ): Unit = {
  //    if (array == null) return
  //
  //    reverse (array, 0, array.length)
  //  }
  //  def reverse (array: Array[Short], startIndexInclusive: Int, endIndexExclusive: Int): Unit = {
  //    if (array == null) return
  //
  //    var i: Int = Math.max (startIndexInclusive, 0)
  //    var j: Int = Math.min (array.length, endIndexExclusive) - 1
  //    var tmp: Short = 0
  //
  //    while (j > i) {
  //      tmp = array(j)
  //      array (j) = array (i)
  //      array (i) = tmp
  //      j -= 1
  //      i += 1
  //    }
  //  }
  //
  //  /**
  //    * Shifts the order of the given boolean array.
  //    *
  //    * <p>There is no special handling for multi-dimensional arrays. This method
  //    * does nothing for {@code null} or empty input arrays.</p>
  //    *
  //    * @param array the array to shift, may be {@code null}
  //    * @param offset
  //    *              The number of positions to rotate the elements.  If the offset is larger than the number of elements to
  //    *              rotate, than the effective offset is modulo the number of elements to rotate.
  //    * @since 3.5
  //    */
  //  def shift (array: Array[Boolean], offset: Int): Unit = {
  //    if (array == null) {
  //      return
  //    }
  //    shift (array, 0, array.length, offset)
  //  }
  //
  //  /**
  //    * Shifts the order of a series of elements in the given boolean array.
  //    *
  //    * <p>There is no special handling for multi-dimensional arrays. This method
  //    * does nothing for {@code null} or empty input arrays.</p>
  //    *
  //    * @param array
  //    * the array to shift, may be {@code null}
  //    * @param startIndexInclusive
  //    * the starting index. Undervalue (&lt;0) is promoted to 0, overvalue (&gt;array.length) results in no
  //    * change.
  //    * @param endIndexExclusive
  //    * elements up to endIndex-1 are shifted in the array. Undervalue (&lt; start index) results in no
  //    * change. Overvalue (&gt;array.length) is demoted to array length.
  //    * @param offset
  //    * The number of positions to rotate the elements.  If the offset is larger than the number of elements to
  //    * rotate, than the effective offset is modulo the number of elements to rotate.
  //    * @since 3.5
  //    */
  //  def shift (array: Array[Boolean], startIndexInclusive: Int, endIndexExclusive: Int, offset: Int): Unit = {
  //    if (array == null) return
  //    if (startIndexInclusive >= array.length - 1 || endIndexExclusive <= 0) return
  //
  //    if (startIndexInclusive < 0) {
  //      startIndexInclusive = 0
  //    }
  //    if (endIndexExclusive >= array.length) {
  //      endIndexExclusive = array.length
  //    }
  //    var n: Int = endIndexExclusive - startIndexInclusive
  //    if (n <= 1) return
  //
  //    offset %= n
  //    if (offset < 0) offset += n
  //
  //    // For algorithm explanations and proof of O(n) time complexity and O(1) space complexity
  //    // see https://beradrian.wordpress.com/2015/04/07/shift-an-array-in-on-in-place/
  //    while (n > 1 && offset > 0) {
  //      val n_offset: Int = n - offset
  //      if (offset > n_offset) {
  //        swap (array, startIndexInclusive, startIndexInclusive + n - n_offset, n_offset)
  //        n = offset
  //        offset -= n_offset
  //      }
  //      else {
  //        if (offset < n_offset) {
  //          swap (array, startIndexInclusive, startIndexInclusive + n_offset, offset)
  //          startIndexInclusive += offset
  //          n = n_offset
  //        }
  //        else {
  //          swap (array, startIndexInclusive, startIndexInclusive + n_offset, offset)
  //          break //todo: break is not supported
  //
  //        }
  //      }
  //    }
  //  }
  //
  //  /**
  //    * Shifts the order of the given byte array.
  //    *
  //    * <p>There is no special handling for multi-dimensional arrays. This method
  //    * does nothing for {@code null} or empty input arrays.</p>
  //    *
  //    * @param array the array to shift, may be {@code null}
  //    * @param offset
  //    *              The number of positions to rotate the elements.  If the offset is larger than the number of elements to
  //    *              rotate, than the effective offset is modulo the number of elements to rotate.
  //    * @since 3.5
  //    */
  //  def shift (array: Array[Byte], offset: Int): Unit = {
  //    if (array == null) return
  //    shift(array, 0, array.length, offset)
  //  }
  //
  //  /**
  //    * Shifts the order of a series of elements in the given byte array.
  //    *
  //    * <p>There is no special handling for multi-dimensional arrays. This method
  //    * does nothing for {@code null} or empty input arrays.</p>
  //    *
  //    * @param array
  //    * the array to shift, may be {@code null}
  //    * @param startIndexInclusive
  //    * the starting index. Undervalue (&lt;0) is promoted to 0, overvalue (&gt;array.length) results in no
  //    * change.
  //    * @param endIndexExclusive
  //    * elements up to endIndex-1 are shifted in the array. Undervalue (&lt; start index) results in no
  //    * change. Overvalue (&gt;array.length) is demoted to array length.
  //    * @param offset
  //    * The number of positions to rotate the elements.  If the offset is larger than the number of elements to
  //    * rotate, than the effective offset is modulo the number of elements to rotate.
  //    * @since 3.5
  //    */
  //  def shift (array: Array[Byte], startIndexInclusive: Int, endIndexExclusive: Int, offset: Int): Unit = {
  //    if (array == null) return
  //    if (startIndexInclusive >= array.length - 1 || endIndexExclusive <= 0) return
  //    if (startIndexInclusive < 0) startIndexInclusive = 0
  //    if (endIndexExclusive >= array.length) endIndexExclusive = array.length
  //
  //    var n: Int = endIndexExclusive - startIndexInclusive
  //    if (n <= 1) return
  //
  //    offset %= n
  //    if (offset < 0) offset += n
  //
  //    while (n > 1 && offset > 0) {
  //      val n_offset: Int = n - offset
  //      if (offset > n_offset) {
  //        swap (array, startIndexInclusive, startIndexInclusive + n - n_offset, n_offset)
  //        n = offset
  //        offset -= n_offset
  //      }
  //      else {
  //        if (offset < n_offset) {
  //          swap (array, startIndexInclusive, startIndexInclusive + n_offset, offset)
  //          startIndexInclusive += offset
  //          n = n_offset
  //        }
  //        else {
  //          swap (array, startIndexInclusive, startIndexInclusive + n_offset, offset)
  //          break //todo: break is not supported
  //
  //        }
  //      }
  //    }
  //  }
  //
  //  /**
  //    * Shifts the order of the given char array.
  //    *
  //    * <p>There is no special handling for multi-dimensional arrays. This method
  //    * does nothing for {@code null} or empty input arrays.</p>
  //    *
  //    * @param array the array to shift, may be {@code null}
  //    * @param offset
  //    *              The number of positions to rotate the elements.  If the offset is larger than the number of elements to
  //    *              rotate, than the effective offset is modulo the number of elements to rotate.
  //    * @since 3.5
  //    */
  //  def shift (array: Array[Char], offset: Int): Unit = {
  //    if (array == null) {
  //      return
  //    }
  //    shift (array, 0, array.length, offset)
  //  }
  //
  //  /**
  //    * Shifts the order of a series of elements in the given char array.
  //    *
  //    * <p>There is no special handling for multi-dimensional arrays. This method
  //    * does nothing for {@code null} or empty input arrays.</p>
  //    *
  //    * @param array
  //    * the array to shift, may be {@code null}
  //    * @param startIndexInclusive
  //    * the starting index. Undervalue (&lt;0) is promoted to 0, overvalue (&gt;array.length) results in no
  //    * change.
  //    * @param endIndexExclusive
  //    * elements up to endIndex-1 are shifted in the array. Undervalue (&lt; start index) results in no
  //    * change. Overvalue (&gt;array.length) is demoted to array length.
  //    * @param offset
  //    * The number of positions to rotate the elements.  If the offset is larger than the number of elements to
  //    * rotate, than the effective offset is modulo the number of elements to rotate.
  //    * @since 3.5
  //    */
  //  def shift (array: Array[Char], startIndexInclusive: Int, endIndexExclusive: Int, offset: Int): Unit = {
  //    if (array == null) return
  //    if (startIndexInclusive >= array.length - 1 || endIndexExclusive <= 0) return
  //    if (startIndexInclusive < 0) startIndexInclusive = 0
  //    if (endIndexExclusive >= array.length) endIndexExclusive = array.length
  //
  //    var n: Int = endIndexExclusive - startIndexInclusive
  //    if (n <= 1) return
  //
  //    offset %= n
  //    if (offset < 0) offset += n
  //
  //    while (n > 1 && offset > 0) {
  //      val n_offset: Int = n - offset
  //      if (offset > n_offset) {
  //        swap (array, startIndexInclusive, startIndexInclusive + n - n_offset, n_offset)
  //        n = offset
  //        offset -= n_offset
  //      }
  //      else {
  //        if (offset < n_offset) {
  //          swap (array, startIndexInclusive, startIndexInclusive + n_offset, offset)
  //          startIndexInclusive += offset
  //          n = n_offset
  //        }
  //        else {
  //          swap (array, startIndexInclusive, startIndexInclusive + n_offset, offset)
  //          break //todo: break is not supported
  //
  //        }
  //      }
  //    }
  //  }
  //
  //  /**
  //    * Shifts the order of the given double array.
  //    *
  //    * <p>There is no special handling for multi-dimensional arrays. This method
  //    * does nothing for {@code null} or empty input arrays.</p>
  //    *
  //    * @param array the array to shift, may be {@code null}
  //    * @param offset
  //    *              The number of positions to rotate the elements.  If the offset is larger than the number of elements to
  //    *              rotate, than the effective offset is modulo the number of elements to rotate.
  //    * @since 3.5
  //    */
  //  def shift (array: Array[Double], offset: Int): Unit = {
  //    if (array == null) return
  //
  //    shift (array, 0, array.length, offset)
  //  }
  //
  //  /**
  //    * Shifts the order of a series of elements in the given double array.
  //    *
  //    * <p>There is no special handling for multi-dimensional arrays. This method
  //    * does nothing for {@code null} or empty input arrays.</p>
  //    *
  //    * @param array
  //    * the array to shift, may be {@code null}
  //    * @param startIndexInclusive
  //    * the starting index. Undervalue (&lt;0) is promoted to 0, overvalue (&gt;array.length) results in no
  //    * change.
  //    * @param endIndexExclusive
  //    * elements up to endIndex-1 are shifted in the array. Undervalue (&lt; start index) results in no
  //    * change. Overvalue (&gt;array.length) is demoted to array length.
  //    * @param offset
  //    * The number of positions to rotate the elements.  If the offset is larger than the number of elements to
  //    * rotate, than the effective offset is modulo the number of elements to rotate.
  //    * @since 3.5
  //    */
  //  def shift (array: Array[Double], startIndexInclusive: Int, endIndexExclusive: Int, offset: Int): Unit = {
  //    if (array == null) return
  //    if (startIndexInclusive >= array.length - 1 || endIndexExclusive <= 0) return
  //
  //    if (startIndexInclusive < 0) startIndexInclusive = 0
  //    if (endIndexExclusive >= array.length) endIndexExclusive = array.length
  //
  //    var n: Int = endIndexExclusive - startIndexInclusive
  //    if (n <= 1) return
  //
  //    offset %= n
  //    if (offset < 0) offset += n
  //
  //    while (n > 1 && offset > 0) {
  //      val n_offset: Int = n - offset
  //      if (offset > n_offset) {
  //        swap (array, startIndexInclusive, startIndexInclusive + n - n_offset, n_offset)
  //        n = offset
  //        offset -= n_offset
  //      }
  //      else {
  //        if (offset < n_offset) {
  //          swap (array, startIndexInclusive, startIndexInclusive + n_offset, offset)
  //          startIndexInclusive += offset
  //          n = n_offset
  //        }
  //        else {
  //          swap (array, startIndexInclusive, startIndexInclusive + n_offset, offset)
  //          break //todo: break is not supported
  //
  //        }
  //      }
  //    }
  //  }
  //
  //  /**
  //    * Shifts the order of the given float array.
  //    *
  //    * <p>There is no special handling for multi-dimensional arrays. This method
  //    * does nothing for {@code null} or empty input arrays.</p>
  //    *
  //    * @param array the array to shift, may be {@code null}
  //    * @param offset
  //    *              The number of positions to rotate the elements.  If the offset is larger than the number of elements to
  //    *              rotate, than the effective offset is modulo the number of elements to rotate.
  //    * @since 3.5
  //    */
  //  def shift (array: Array[Float], offset: Int): Unit = {
  //    if (array == null) return
  //    shift (array, 0, array.length, offset)
  //  }
  //
  //  /**
  //    * Shifts the order of a series of elements in the given float array.
  //    *
  //    * <p>There is no special handling for multi-dimensional arrays. This method
  //    * does nothing for {@code null} or empty input arrays.</p>
  //    *
  //    * @param array
  //    * the array to shift, may be {@code null}
  //    * @param startIndexInclusive
  //    * the starting index. Undervalue (&lt;0) is promoted to 0, overvalue (&gt;array.length) results in no
  //    * change.
  //    * @param endIndexExclusive
  //    * elements up to endIndex-1 are shifted in the array. Undervalue (&lt; start index) results in no
  //    * change. Overvalue (&gt;array.length) is demoted to array length.
  //    * @param offset
  //    * The number of positions to rotate the elements.  If the offset is larger than the number of elements to
  //    * rotate, than the effective offset is modulo the number of elements to rotate.
  //    * @since 3.5
  //    */
  //  def shift (array: Array[Float], startIndexInclusive: Int, endIndexExclusive: Int, offset: Int): Unit = {
  //    if (array == null) return
  //    if (startIndexInclusive >= array.length - 1 || endIndexExclusive <= 0) return
  //    if (startIndexInclusive < 0) startIndexInclusive = 0
  //    if (endIndexExclusive >= array.length) endIndexExclusive = array.length
  //
  //    var n: Int = endIndexExclusive - startIndexInclusive
  //    if (n <= 1) return
  //    offset %= n
  //
  //    if (offset < 0) offset += n
  //
  //    while (n > 1 && offset > 0) {
  //      val n_offset: Int = n - offset
  //      if (offset > n_offset) {
  //        swap (array, startIndexInclusive, startIndexInclusive + n - n_offset, n_offset)
  //        n = offset
  //        offset -= n_offset
  //      } else {
  //        if (offset < n_offset) {
  //          swap (array, startIndexInclusive, startIndexInclusive + n_offset, offset)
  //          startIndexInclusive += offset
  //          n = n_offset
  //        }
  //        else {
  //          swap (array, startIndexInclusive, startIndexInclusive + n_offset, offset)
  //          break //todo: break is not supported
  //
  //        }
  //      }
  //    }
  //  }
  //
  //  /**
  //    * Shifts the order of the given int array.
  //    *
  //    * <p>There is no special handling for multi-dimensional arrays. This method
  //    * does nothing for {@code null} or empty input arrays.</p>
  //    *
  //    * @param array the array to shift, may be {@code null}
  //    * @param offset
  //    *              The number of positions to rotate the elements.  If the offset is larger than the number of elements to
  //    *              rotate, than the effective offset is modulo the number of elements to rotate.
  //    * @since 3.5
  //    */
  //  def shift (array: Array[Int], offset: Int): Unit = {
  //    if (array == null) return
  //    shift (array, 0, array.length, offset)
  //  }
  //
  //  /**
  //    * Shifts the order of a series of elements in the given int array.
  //    *
  //    * <p>There is no special handling for multi-dimensional arrays. This method
  //    * does nothing for {@code null} or empty input arrays.</p>
  //    *
  //    * @param array
  //    * the array to shift, may be {@code null}
  //    * @param startIndexInclusive
  //    * the starting index. Undervalue (&lt;0) is promoted to 0, overvalue (&gt;array.length) results in no
  //    * change.
  //    * @param endIndexExclusive
  //    * elements up to endIndex-1 are shifted in the array. Undervalue (&lt; start index) results in no
  //    * change. Overvalue (&gt;array.length) is demoted to array length.
  //    * @param offset
  //    * The number of positions to rotate the elements.  If the offset is larger than the number of elements to
  //    * rotate, than the effective offset is modulo the number of elements to rotate.
  //    * @since 3.5
  //    */
  //  def shift (array: Array[Int], startIndexInclusive: Int, endIndexExclusive: Int, offset: Int): Unit = {
  //    if (array == null) {
  //      return
  //    }
  //    if (startIndexInclusive >= array.length - 1 || endIndexExclusive <= 0) {
  //      return
  //    }
  //    if (startIndexInclusive < 0) startIndexInclusive = 0
  //    if (endIndexExclusive >= array.length) endIndexExclusive = array.length
  //
  //    var n: Int = endIndexExclusive - startIndexInclusive
  //    if (n <= 1) return
  //
  //    offset %= n
  //    if (offset < 0) offset += n
  //
  //    while (n > 1 && offset > 0) {
  //      val n_offset: Int = n - offset
  //      if (offset > n_offset) {
  //        swap (array, startIndexInclusive, startIndexInclusive + n - n_offset, n_offset)
  //        n = offset
  //        offset -= n_offset
  //      }
  //      else {
  //        if (offset < n_offset) {
  //          swap (array, startIndexInclusive, startIndexInclusive + n_offset, offset)
  //          startIndexInclusive += offset
  //          n = n_offset
  //        }
  //        else {
  //          swap (array, startIndexInclusive, startIndexInclusive + n_offset, offset)
  //          break //todo: break is not supported
  //
  //        }
  //      }
  //    }
  //  }
  //
  //  /**
  //    * Shifts the order of the given long array.
  //    *
  //    * <p>There is no special handling for multi-dimensional arrays. This method
  //    * does nothing for {@code null} or empty input arrays.</p>
  //    *
  //    * @param array the array to shift, may be {@code null}
  //    * @param offset
  //    *              The number of positions to rotate the elements.  If the offset is larger than the number of elements to
  //    *              rotate, than the effective offset is modulo the number of elements to rotate.
  //    * @since 3.5
  //    */
  //  def shift (array: Array[Long], offset: Int): Unit = {
  //    if (array == null) return
  //
  //    shift (array, 0, array.length, offset)
  //  }
  //
  //  /**
  //    * Shifts the order of a series of elements in the given long array.
  //    *
  //    * <p>There is no special handling for multi-dimensional arrays. This method
  //    * does nothing for {@code null} or empty input arrays.</p>
  //    *
  //    * @param array
  //    * the array to shift, may be {@code null}
  //    * @param startIndexInclusive
  //    * the starting index. Undervalue (&lt;0) is promoted to 0, overvalue (&gt;array.length) results in no
  //    * change.
  //    * @param endIndexExclusive
  //    * elements up to endIndex-1 are shifted in the array. Undervalue (&lt; start index) results in no
  //    * change. Overvalue (&gt;array.length) is demoted to array length.
  //    * @param offset
  //    * The number of positions to rotate the elements.  If the offset is larger than the number of elements to
  //    * rotate, than the effective offset is modulo the number of elements to rotate.
  //    * @since 3.5
  //    */
  //  def shift (array: Array[Long], startIndexInclusive: Int, endIndexExclusive: Int, offset: Int): Unit = {
  //    if (array == null) return
  //    if (startIndexInclusive >= array.length - 1 || endIndexExclusive <= 0) return
  //    if (startIndexInclusive < 0) startIndexInclusive = 0
  //    if (endIndexExclusive >= array.length) endIndexExclusive = array.length
  //
  //    var n: Int = endIndexExclusive - startIndexInclusive
  //    if (n <= 1) return
  //    offset %= n
  //
  //    if (offset < 0) offset += n
  //
  //    while (n > 1 && offset > 0) {
  //      val n_offset: Int = n - offset
  //      if (offset > n_offset) {
  //        swap (array, startIndexInclusive, startIndexInclusive + n - n_offset, n_offset)
  //        n = offset
  //        offset -= n_offset
  //      } else {
  //        if (offset < n_offset) {
  //          swap (array, startIndexInclusive, startIndexInclusive + n_offset, offset)
  //          startIndexInclusive += offset
  //          n = n_offset
  //        } else {
  //          swap (array, startIndexInclusive, startIndexInclusive + n_offset, offset)
  //          break //todo: break is not supported
  //
  //        }
  //      }
  //    }
  //  }
  //
  //  /**
  //    * Shifts the order of the given array.
  //    *
  //    * <p>There is no special handling for multi-dimensional arrays. This method
  //    * does nothing for {@code null} or empty input arrays.</p>
  //    *
  //    * @param array the array to shift, may be {@code null}
  //    * @param offset
  //    *              The number of positions to rotate the elements.  If the offset is larger than the number of elements to
  //    *              rotate, than the effective offset is modulo the number of elements to rotate.
  //    * @since 3.5
  //    */
  //  def shift (array: Array[AnyRef], offset: Int): Unit = {
  //    if (array == null) return
  //    shift (array, 0, array.length, offset)
  //  }
  //
  //  /**
  //    * Shifts the order of a series of elements in the given array.
  //    *
  //    * <p>There is no special handling for multi-dimensional arrays. This method
  //    * does nothing for {@code null} or empty input arrays.</p>
  //    *
  //    * @param array
  //    * the array to shift, may be {@code null}
  //    * @param startIndexInclusive
  //    * the starting index. Undervalue (&lt;0) is promoted to 0, overvalue (&gt;array.length) results in no
  //    * change.
  //    * @param endIndexExclusive
  //    * elements up to endIndex-1 are shifted in the array. Undervalue (&lt; start index) results in no
  //    * change. Overvalue (&gt;array.length) is demoted to array length.
  //    * @param offset
  //    * The number of positions to rotate the elements.  If the offset is larger than the number of elements to
  //    * rotate, than the effective offset is modulo the number of elements to rotate.
  //    * @since 3.5
  //    */
  //  def shift (array: Array[AnyRef], startIndexInclusive: Int, endIndexExclusive: Int, offset: Int): Unit = {
  //    if (array == null) return
  //    if (startIndexInclusive >= array.length - 1 || endIndexExclusive <= 0) return
  //    if (startIndexInclusive < 0) startIndexInclusive = 0
  //    if (endIndexExclusive >= array.length) endIndexExclusive = array.length
  //
  //    var n: Int = endIndexExclusive - startIndexInclusive
  //    if (n <= 1) return
  //
  //    offset %= n
  //    if (offset < 0) offset += n
  //
  //    while (n > 1 && offset > 0) {
  //      val n_offset: Int = n - offset
  //      if (offset > n_offset) {
  //        swap (array, startIndexInclusive, startIndexInclusive + n - n_offset, n_offset)
  //        n = offset
  //        offset -= n_offset
  //      } else {
  //        if (offset < n_offset) {
  //          swap (array, startIndexInclusive, startIndexInclusive + n_offset, offset)
  //          startIndexInclusive += offset
  //          n = n_offset
  //        } else {
  //          swap (array, startIndexInclusive, startIndexInclusive + n_offset, offset)
  //          break //todo: break is not supported
  //        }
  //      }
  //    }
  //  }
  //
  //  /**
  //    * Shifts the order of the given short array.
  //    *
  //    * <p>There is no special handling for multi-dimensional arrays. This method
  //    * does nothing for {@code null} or empty input arrays.</p>
  //    *
  //    * @param array the array to shift, may be {@code null}
  //    * @param offset
  //    *              The number of positions to rotate the elements.  If the offset is larger than the number of elements to
  //    *              rotate, than the effective offset is modulo the number of elements to rotate.
  //    * @since 3.5
  //    */
  //  def shift (array: Array[Short], offset: Int): Unit = {
  //    if (array == null) return
  //    shift(array, 0, array.length, offset)
  //  }
  //
  //  /**
  //    * Shifts the order of a series of elements in the given short array.
  //    *
  //    * <p>There is no special handling for multi-dimensional arrays. This method
  //    * does nothing for {@code null} or empty input arrays.</p>
  //    *
  //    * @param array
  //    * the array to shift, may be {@code null}
  //    * @param startIndexInclusive
  //    * the starting index. Undervalue (&lt;0) is promoted to 0, overvalue (&gt;array.length) results in no
  //    * change.
  //    * @param endIndexExclusive
  //    * elements up to endIndex-1 are shifted in the array. Undervalue (&lt; start index) results in no
  //    * change. Overvalue (&gt;array.length) is demoted to array length.
  //    * @param offset
  //    * The number of positions to rotate the elements.  If the offset is larger than the number of elements to
  //    * rotate, than the effective offset is modulo the number of elements to rotate.
  //    * @since 3.5
  //    */
  //  def shift (array: Array[Short], startIndexInclusive: Int, endIndexExclusive: Int, offset: Int): Unit = {
  //    if (array == null) return
  //    if (startIndexInclusive >= array.length - 1 || endIndexExclusive <= 0) return
  //    if (startIndexInclusive < 0) startIndexInclusive = 0
  //    if (endIndexExclusive >= array.length) endIndexExclusive = array.length
  //
  //    var n: Int = endIndexExclusive - startIndexInclusive
  //    if (n <= 1) return
  //
  //    offset %= n
  //    if (offset < 0) offset += n
  //
  //    while (n > 1 && offset > 0) {
  //      val n_offset: Int = n - offset
  //      if (offset > n_offset) {
  //        swap (array, startIndexInclusive, startIndexInclusive + n - n_offset, n_offset)
  //        n = offset
  //        offset -= n_offset
  //      } else {
  //        if (offset < n_offset) {
  //          swap (array, startIndexInclusive, startIndexInclusive + n_offset, offset)
  //          startIndexInclusive += offset
  //          n = n_offset
  //        }
  //        else {
  //          swap (array, startIndexInclusive, startIndexInclusive + n_offset, offset)
  //          break //todo: break is not supported
  //
  //        }
  //      }
  //    }
  //  }
  //
  //  /**
  //    * Randomly permutes the elements of the specified array using the Fisher-Yates algorithm.
  //    *
  //    * @param array the array to shuffle
  //    * @see <a href="https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle">Fisher-Yates shuffle algorithm</a>
  //    * @since 3.6
  //    */
  //  def shuffle (array: Array[Boolean] ): Unit = {
  //    shuffle (array, new Random)
  //  }
  //
  //  /**
  //    * Randomly permutes the elements of the specified array using the Fisher-Yates algorithm.
  //    *
  //    * @param array  the array to shuffle
  //    * @param random the source of randomness used to permute the elements
  //    * @see <a href="https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle">Fisher-Yates shuffle algorithm</a>
  //    * @since 3.6
  //    */
  //  def shuffle (array: Array[Boolean], random: Random): Unit = {
  //    for (i <- array.length until 1 by - 1) {
  //      swap (array, i - 1, random.nextInt (i), 1)
  //    }
  //  }
  //  def shuffle (array: Array[Byte] ): Unit = {
  //    shuffle (array, new Random)
  //  }
  //  def shuffle (array: Array[Byte], random: Random): Unit = {
  //    for (i <- array.length until 1 by - 1) {
  //      swap (array, i - 1, random.nextInt (i), 1)
  //    }
  //  }
  //  def shuffle (array: Array[Char] ): Unit = {
  //    shuffle (array, new Random)
  //  }
  //  def shuffle (array: Array[Char], random: Random): Unit = {
  //    for (i <- array.length until 1 by - 1) {
  //      swap (array, i - 1, random.nextInt (i), 1)
  //    }
  //  }
  //  def shuffle (array: Array[Double] ): Unit = {
  //    shuffle (array, new Random)
  //  }
  //  def shuffle (array: Array[Double], random: Random): Unit = {
  //    for (i <- array.length until 1 by - 1) {
  //      swap (array, i - 1, random.nextInt (i), 1)
  //    }
  //  }
  //  def shuffle (array: Array[Float] ): Unit = {
  //    shuffle (array, new Random)
  //  }
  //  def shuffle (array: Array[Float], random: Random): Unit = {
  //    for (i <- array.length until 1 by - 1) {
  //      swap (array, i - 1, random.nextInt (i), 1)
  //    }
  //  }
  //  def shuffle (array: Array[Int] ): Unit = {
  //    shuffle (array, new Random)
  //  }
  //  def shuffle (array: Array[Int], random: Random): Unit = {
  //    for (i <- array.length until 1 by - 1) {
  //      swap (array, i - 1, random.nextInt (i), 1)
  //    }
  //  }
  //  def shuffle (array: Array[Long] ): Unit = {
  //    shuffle (array, new Random)
  //  }
  //  def shuffle (array: Array[Long], random: Random): Unit = {
  //    for (i <- array.length until 1 by - 1) {
  //      swap (array, i - 1, random.nextInt (i), 1)
  //    }
  //  }
  //  def shuffle (array: Array[AnyRef] ): Unit = {
  //    shuffle (array, new Random)
  //  }
  //  def shuffle (array: Array[AnyRef], random: Random): Unit = {
  //    for (i <- array.length until 1 by - 1) {
  //      swap (array, i - 1, random.nextInt (i), 1)
  //    }
  //  }
  //  def shuffle (array: Array[Short] ): Unit = {
  //    shuffle (array, new Random)
  //  }
  //  def shuffle (array: Array[Short], random: Random): Unit = {
  //    for (i <- array.length until 1 by - 1) {
  //      swap (array, i - 1, random.nextInt (i), 1)
  //    }
  //  }
  //
  //  /**
  //    * <p>Produces a new {@code boolean} array containing the elements
  //    * between the start and end indices.
  //    *
  //    * <p>The start index is inclusive, the end index exclusive.
  //    * Null array input produces null output.
  //    *
  //    * @param array               the array
  //    * @param startIndexInclusive the starting index. Undervalue (&lt;0)
  //    *                            is promoted to 0, overvalue (&gt;array.length) results
  //    *                            in an empty array.
  //    * @param endIndexExclusive   elements up to endIndex-1 are present in the
  //    *                            returned subarray. Undervalue (&lt; startIndex) produces
  //    *                            empty array, overvalue (&gt;array.length) is demoted to
  //    *                            array length.
  //    * @return a new array containing the elements between
  //    *         the start and end indices.
  //    * @since 2.1
  //    * @see Arrays#copyOfRange(boolean[], int, int)
  //    */
  //  def subarray (array: Array[Boolean], startIndexInclusive: Int, endIndexExclusive: Int): Array[Boolean] = {
  //    if (array == null) return null
  //    if (startIndexInclusive < 0) startIndexInclusive = 0
  //    if (endIndexExclusive > array.length) endIndexExclusive = array.length
  //    val newSize: Int = endIndexExclusive - startIndexInclusive
  //
  //    if (newSize <= 0) return EMPTY_BOOLEAN_ARRAY
  //    val subarray: Array[Boolean] = new Array[Boolean] (newSize)
  //    System.arraycopy (array, startIndexInclusive, subarray, 0, newSize)
  //    subarray
  //  }
  //
  //  /**
  //    * <p>Produces a new {@code byte} array containing the elements
  //    * between the start and end indices.
  //    *
  //    * <p>The start index is inclusive, the end index exclusive.
  //    * Null array input produces null output.
  //    *
  //    * @param array               the array
  //    * @param startIndexInclusive the starting index. Undervalue (&lt;0)
  //    *                            is promoted to 0, overvalue (&gt;array.length) results
  //    *                            in an empty array.
  //    * @param endIndexExclusive   elements up to endIndex-1 are present in the
  //    *                            returned subarray. Undervalue (&lt; startIndex) produces
  //    *                            empty array, overvalue (&gt;array.length) is demoted to
  //    *                            array length.
  //    * @return a new array containing the elements between
  //    *         the start and end indices.
  //    * @since 2.1
  //    * @see Arrays#copyOfRange(byte[], int, int)
  //    */
  //  def subarray (array: Array[Byte], startIndexInclusive: Int, endIndexExclusive: Int): Array[Byte] = {
  //    if (array == null) return null
  //    if (startIndexInclusive < 0) startIndexInclusive = 0
  //    if (endIndexExclusive > array.length) endIndexExclusive = array.length
  //
  //    val newSize: Int = endIndexExclusive - startIndexInclusive
  //    if (newSize <= 0) return EMPTY_BYTE_ARRAY
  //
  //    val subarray: Array[Byte] = new Array[Byte] (newSize)
  //    System.arraycopy (array, startIndexInclusive, subarray, 0, newSize)
  //    subarray
  //  }
  //
  //  /**
  //    * <p>Produces a new {@code char} array containing the elements
  //    * between the start and end indices.
  //    *
  //    * <p>The start index is inclusive, the end index exclusive.
  //    * Null array input produces null output.
  //    *
  //    * @param array               the array
  //    * @param startIndexInclusive the starting index. Undervalue (&lt;0)
  //    *                            is promoted to 0, overvalue (&gt;array.length) results
  //    *                            in an empty array.
  //    * @param endIndexExclusive   elements up to endIndex-1 are present in the
  //    *                            returned subarray. Undervalue (&lt; startIndex) produces
  //    *                            empty array, overvalue (&gt;array.length) is demoted to
  //    *                            array length.
  //    * @return a new array containing the elements between
  //    *         the start and end indices.
  //    * @since 2.1
  //    * @see Arrays#copyOfRange(char[], int, int)
  //    */
  //  def subarray (array: Array[Char], startIndexInclusive: Int, endIndexExclusive: Int): Array[Char] = {
  //    if (array == null) return null
  //    if (startIndexInclusive < 0) startIndexInclusive = 0
  //    if (endIndexExclusive > array.length) endIndexExclusive = array.length
  //
  //    val newSize: Int = endIndexExclusive - startIndexInclusive
  //    if (newSize <= 0) return EMPTY_CHAR_ARRAY
  //    val subarray: Array[Char] = new Array[Char] (newSize)
  //    System.arraycopy (array, startIndexInclusive, subarray, 0, newSize)
  //    subarray
  //  }
  //
  //  /**
  //    * <p>Produces a new {@code double} array containing the elements
  //    * between the start and end indices.
  //    *
  //    * <p>The start index is inclusive, the end index exclusive.
  //    * Null array input produces null output.
  //    *
  //    * @param array               the array
  //    * @param startIndexInclusive the starting index. Undervalue (&lt;0)
  //    *                            is promoted to 0, overvalue (&gt;array.length) results
  //    *                            in an empty array.
  //    * @param endIndexExclusive   elements up to endIndex-1 are present in the
  //    *                            returned subarray. Undervalue (&lt; startIndex) produces
  //    *                            empty array, overvalue (&gt;array.length) is demoted to
  //    *                            array length.
  //    * @return a new array containing the elements between
  //    *         the start and end indices.
  //    * @since 2.1
  //    * @see Arrays#copyOfRange(double[], int, int)
  //    */
  //  def subarray (array: Array[Double], startIndexInclusive: Int, endIndexExclusive: Int): Array[Double] = {
  //    if (array == null) return null
  //    if (startIndexInclusive < 0) {
  //      startIndexInclusive = 0
  //    }
  //    if (endIndexExclusive > array.length) {
  //      endIndexExclusive = array.length
  //    }
  //    val newSize: Int = endIndexExclusive - startIndexInclusive
  //    if (newSize <= 0) {
  //      return EMPTY_DOUBLE_ARRAY
  //    }
  //    val subarray: Array[Double] = new Array[Double] (newSize)
  //    System.arraycopy (array, startIndexInclusive, subarray, 0, newSize)
  //    return subarray
  //  }
  //
  //  /**
  //    * <p>Produces a new {@code float} array containing the elements
  //    * between the start and end indices.
  //    *
  //    * <p>The start index is inclusive, the end index exclusive.
  //    * Null array input produces null output.
  //    *
  //    * @param array               the array
  //    * @param startIndexInclusive the starting index. Undervalue (&lt;0)
  //    *                            is promoted to 0, overvalue (&gt;array.length) results
  //    *                            in an empty array.
  //    * @param endIndexExclusive   elements up to endIndex-1 are present in the
  //    *                            returned subarray. Undervalue (&lt; startIndex) produces
  //    *                            empty array, overvalue (&gt;array.length) is demoted to
  //    *                            array length.
  //    * @return a new array containing the elements between
  //    *         the start and end indices.
  //    * @since 2.1
  //    * @see Arrays#copyOfRange(float[], int, int)
  //    */
  //  def subarray (array: Array[Float], startIndexInclusive: Int, endIndexExclusive: Int): Array[Float] = {
  //    if (array == null) return null
  //    if (startIndexInclusive < 0) {
  //      startIndexInclusive = 0
  //    }
  //    if (endIndexExclusive > array.length) {
  //      endIndexExclusive = array.length
  //    }
  //    val newSize: Int = endIndexExclusive - startIndexInclusive
  //    if (newSize <= 0) {
  //      return EMPTY_FLOAT_ARRAY
  //    }
  //    val subarray: Array[Float] = new Array[Float] (newSize)
  //    System.arraycopy (array, startIndexInclusive, subarray, 0, newSize)
  //    return subarray
  //  }
  //
  //  /**
  //    * <p>Produces a new {@code int} array containing the elements
  //    * between the start and end indices.
  //    *
  //    * <p>The start index is inclusive, the end index exclusive.
  //    * Null array input produces null output.
  //    *
  //    * @param array               the array
  //    * @param startIndexInclusive the starting index. Undervalue (&lt;0)
  //    *                            is promoted to 0, overvalue (&gt;array.length) results
  //    *                            in an empty array.
  //    * @param endIndexExclusive   elements up to endIndex-1 are present in the
  //    *                            returned subarray. Undervalue (&lt; startIndex) produces
  //    *                            empty array, overvalue (&gt;array.length) is demoted to
  //    *                            array length.
  //    * @return a new array containing the elements between
  //    *         the start and end indices.
  //    * @since 2.1
  //    * @see Arrays#copyOfRange(int[], int, int)
  //    */
  //  def subarray (array: Array[Int], startIndexInclusive: Int, endIndexExclusive: Int): Array[Int] = {
  //    if (array == null) return null
  //    if (startIndexInclusive < 0) {
  //      startIndexInclusive = 0
  //    }
  //    if (endIndexExclusive > array.length) {
  //      endIndexExclusive = array.length
  //    }
  //    val newSize: Int = endIndexExclusive - startIndexInclusive
  //    if (newSize <= 0) {
  //      return EMPTY_INT_ARRAY
  //    }
  //    val subarray: Array[Int] = new Array[Int] (newSize)
  //    System.arraycopy (array, startIndexInclusive, subarray, 0, newSize)
  //    return subarray
  //  }
  //
  //  /**
  //    * <p>Produces a new {@code long} array containing the elements
  //    * between the start and end indices.
  //    *
  //    * <p>The start index is inclusive, the end index exclusive.
  //    * Null array input produces null output.
  //    *
  //    * @param array               the array
  //    * @param startIndexInclusive the starting index. Undervalue (&lt;0)
  //    *                            is promoted to 0, overvalue (&gt;array.length) results
  //    *                            in an empty array.
  //    * @param endIndexExclusive   elements up to endIndex-1 are present in the
  //    *                            returned subarray. Undervalue (&lt; startIndex) produces
  //    *                            empty array, overvalue (&gt;array.length) is demoted to
  //    *                            array length.
  //    * @return a new array containing the elements between
  //    *         the start and end indices.
  //    * @since 2.1
  //    * @see Arrays#copyOfRange(long[], int, int)
  //    */
  //  def subarray (array: Array[Long], startIndexInclusive: Int, endIndexExclusive: Int): Array[Long] = {
  //    if (array == null) return null
  //    if (startIndexInclusive < 0) {
  //      startIndexInclusive = 0
  //    }
  //    if (endIndexExclusive > array.length) {
  //      endIndexExclusive = array.length
  //    }
  //    val newSize: Int = endIndexExclusive - startIndexInclusive
  //    if (newSize <= 0) {
  //      return EMPTY_LONG_ARRAY
  //    }
  //    val subarray: Array[Long] = new Array[Long] (newSize)
  //    System.arraycopy (array, startIndexInclusive, subarray, 0, newSize)
  //    return subarray
  //  }
  //
  //  /**
  //    * <p>Produces a new {@code short} array containing the elements
  //    * between the start and end indices.
  //    *
  //    * <p>The start index is inclusive, the end index exclusive.
  //    * Null array input produces null output.
  //    *
  //    * @param array               the array
  //    * @param startIndexInclusive the starting index. Undervalue (&lt;0)
  //    *                            is promoted to 0, overvalue (&gt;array.length) results
  //    *                            in an empty array.
  //    * @param endIndexExclusive   elements up to endIndex-1 are present in the
  //    *                            returned subarray. Undervalue (&lt; startIndex) produces
  //    *                            empty array, overvalue (&gt;array.length) is demoted to
  //    *                            array length.
  //    * @return a new array containing the elements between
  //    *         the start and end indices.
  //    * @since 2.1
  //    * @see Arrays#copyOfRange(short[], int, int)
  //    */
  //  def subarray (array: Array[Short], startIndexInclusive: Int, endIndexExclusive: Int): Array[Short] = {
  //    if (array == null) return null
  //    if (startIndexInclusive < 0) {
  //      startIndexInclusive = 0
  //    }
  //    if (endIndexExclusive > array.length) {
  //      endIndexExclusive = array.length
  //    }
  //    val newSize: Int = endIndexExclusive - startIndexInclusive
  //    if (newSize <= 0) {
  //      return EMPTY_SHORT_ARRAY
  //    }
  //    val subarray: Array[Short] = new Array[Short] (newSize)
  //    System.arraycopy (array, startIndexInclusive, subarray, 0, newSize)
  //    return subarray
  //  }
  //
  //  /**
  //    * <p>Produces a new array containing the elements between
  //    * the start and end indices.
  //    *
  //    * <p>The start index is inclusive, the end index exclusive.
  //    * Null array input produces null output.
  //    *
  //    * <p>The component type of the subarray is always the same as
  //    * that of the input array. Thus, if the input is an array of type
  //    * {@code Date}, the following usage is envisaged:
  //    *
  //    * <pre>
  //    * Date[] someDates = (Date[]) ArrayUtils.subarray(allDates, 2, 5);
  //    * </pre>
  //    *
  //    * @tparam T                  the component type of the array
  //    * @param array               the array
  //    * @param startIndexInclusive the starting index. Undervalue (&lt;0)
  //    *                            is promoted to 0, overvalue (&gt;array.length) results
  //    *                            in an empty array.
  //    * @param endIndexExclusive   elements up to endIndex-1 are present in the
  //    *                            returned subarray. Undervalue (&lt; startIndex) produces
  //    *                            empty array, overvalue (&gt;array.length) is demoted to
  //    *                            array length.
  //    * @return a new array containing the elements between
  //    *         the start and end indices.
  //    * @since 2.1
  //    * @see Arrays#copyOfRange(Object[], int, int)
  //    */
  //  def subarray[T] (array: Array[T], startIndexInclusive: Int, endIndexExclusive: Int): Array[T] = {
  //    if (array == null) return null
  //    if (startIndexInclusive < 0) {
  //      startIndexInclusive = 0
  //    }
  //    if (endIndexExclusive > array.length) {
  //      endIndexExclusive = array.length
  //    }
  //    val newSize: Int = endIndexExclusive - startIndexInclusive
  //    val `type`: Class[_] = array.getClass.getComponentType
  //    if (newSize <= 0) {
  //      @SuppressWarnings (Array ("unchecked") ) val emptyArray: Array[T] = Array.newInstance (`type`, 0).asInstanceOf[Array[T]]
  //      return emptyArray
  //    }
  //    @SuppressWarnings (Array ("unchecked") ) val subarray: Array[T] = Array.newInstance (`type`, newSize).asInstanceOf[Array[T]]
  //    System.arraycopy (array, startIndexInclusive, subarray, 0, newSize)
  //    return subarray
  //  }
  //
  //  /**
  //    * Swaps two elements in the given boolean array.
  //    *
  //    * <p>There is no special handling for multi-dimensional arrays. This method
  //    * does nothing for a {@code null} or empty input array or for overflow indices.
  //    * Negative indices are promoted to 0(zero).</p>
  //    *
  //    * Examples:
  //    * <ul>
  //    * <li>ArrayUtils.swap([1, 2, 3], 0, 2) -&gt; [3, 2, 1]</li>
  //    * <li>ArrayUtils.swap([1, 2, 3], 0, 0) -&gt; [1, 2, 3]</li>
  //    * <li>ArrayUtils.swap([1, 2, 3], 1, 0) -&gt; [2, 1, 3]</li>
  //    * <li>ArrayUtils.swap([1, 2, 3], 0, 5) -&gt; [1, 2, 3]</li>
  //    * <li>ArrayUtils.swap([1, 2, 3], -1, 1) -&gt; [2, 1, 3]</li>
  //    * </ul>
  //    *
  //    * @param array   the array to swap, may be {@code null}
  //    * @param offset1 the index of the first element to swap
  //    * @param offset2 the index of the second element to swap
  //    * @since 3.5
  //    */
  //  def swap (array: Array[Boolean], offset1: Int, offset2: Int): Unit = {
  //    if (isEmpty (array) ) {
  //      return
  //    }
  //    swap (array, offset1, offset2, 1)
  //  }
  //
  //  /**
  //    * Swaps a series of elements in the given boolean array.
  //    *
  //    * <p>This method does nothing for a {@code null} or empty input array or
  //    * for overflow indices. Negative indices are promoted to 0(zero). If any
  //    * of the sub-arrays to swap falls outside of the given array, then the
  //    * swap is stopped at the end of the array and as many as possible elements
  //    * are swapped.</p>
  //    *
  //    * Examples:
  //    * <ul>
  //    * <li>ArrayUtils.swap([true, false, true, false], 0, 2, 1) -&gt; [true, false, true, false]</li>
  //    * <li>ArrayUtils.swap([true, false, true, false], 0, 0, 1) -&gt; [true, false, true, false]</li>
  //    * <li>ArrayUtils.swap([true, false, true, false], 0, 2, 2) -&gt; [true, false, true, false]</li>
  //    * <li>ArrayUtils.swap([true, false, true, false], -3, 2, 2) -&gt; [true, false, true, false]</li>
  //    * <li>ArrayUtils.swap([true, false, true, false], 0, 3, 3) -&gt; [false, false, true, true]</li>
  //    * </ul>
  //    *
  //    * @param array   the array to swap, may be {@code null}
  //    * @param offset1 the index of the first element in the series to swap
  //    * @param offset2 the index of the second element in the series to swap
  //    * @param len     the number of elements to swap starting with the given indices
  //    * @since 3.5
  //    */
  //  def swap (array: Array[Boolean], offset1: Int, offset2: Int, len: Int): Unit = {
  //    if (isEmpty (array) || offset1 >= array.length || offset2 >= array.length) {
  //      return
  //    }
  //    if (offset1 < 0) {
  //      offset1 = 0
  //    }
  //    if (offset2 < 0) {
  //      offset2 = 0
  //    }
  //    len = Math.min (Math.min (len, array.length - offset1), array.length - offset2)
  //    var i: Int = 0
  //    while ( {
  //      i < len
  //    }) {
  //      val aux: Boolean = array (offset1)
  //      array (offset1) = array (offset2)
  //      array (offset2) = aux
  //
  //      i += 1
  //      offset1 += 1
  //      offset2 += 1
  //    }
  //  }
  //
  //  /**
  //    * Swaps two elements in the given byte array.
  //    *
  //    * <p>There is no special handling for multi-dimensional arrays. This method
  //    * does nothing for a {@code null} or empty input array or for overflow indices.
  //    * Negative indices are promoted to 0(zero).</p>
  //    *
  //    * Examples:
  //    * <ul>
  //    * <li>ArrayUtils.swap([1, 2, 3], 0, 2) -&gt; [3, 2, 1]</li>
  //    * <li>ArrayUtils.swap([1, 2, 3], 0, 0) -&gt; [1, 2, 3]</li>
  //    * <li>ArrayUtils.swap([1, 2, 3], 1, 0) -&gt; [2, 1, 3]</li>
  //    * <li>ArrayUtils.swap([1, 2, 3], 0, 5) -&gt; [1, 2, 3]</li>
  //    * <li>ArrayUtils.swap([1, 2, 3], -1, 1) -&gt; [2, 1, 3]</li>
  //    * </ul>
  //    *
  //    * @param array   the array to swap, may be {@code null}
  //    * @param offset1 the index of the first element to swap
  //    * @param offset2 the index of the second element to swap
  //    * @since 3.5
  //    */
  //  def swap (array: Array[Byte], offset1: Int, offset2: Int): Unit = {
  //    if (isEmpty (array) ) {
  //      return
  //    }
  //    swap (array, offset1, offset2, 1)
  //  }
  //
  //  /**
  //    * Swaps a series of elements in the given byte array.
  //    *
  //    * <p>This method does nothing for a {@code null} or empty input array or
  //    * for overflow indices. Negative indices are promoted to 0(zero). If any
  //    * of the sub-arrays to swap falls outside of the given array, then the
  //    * swap is stopped at the end of the array and as many as possible elements
  //    * are swapped.</p>
  //    *
  //    * Examples:
  //    * <ul>
  //    * <li>ArrayUtils.swap([1, 2, 3, 4], 0, 2, 1) -&gt; [3, 2, 1, 4]</li>
  //    * <li>ArrayUtils.swap([1, 2, 3, 4], 0, 0, 1) -&gt; [1, 2, 3, 4]</li>
  //    * <li>ArrayUtils.swap([1, 2, 3, 4], 2, 0, 2) -&gt; [3, 4, 1, 2]</li>
  //    * <li>ArrayUtils.swap([1, 2, 3, 4], -3, 2, 2) -&gt; [3, 4, 1, 2]</li>
  //    * <li>ArrayUtils.swap([1, 2, 3, 4], 0, 3, 3) -&gt; [4, 2, 3, 1]</li>
  //    * </ul>
  //    *
  //    * @param array   the array to swap, may be {@code null}
  //    * @param offset1 the index of the first element in the series to swap
  //    * @param offset2 the index of the second element in the series to swap
  //    * @param len     the number of elements to swap starting with the given indices
  //    * @since 3.5
  //    */
  //  def swap (array: Array[Byte], offset1: Int, offset2: Int, len: Int): Unit = {
  //    if (isEmpty (array) || offset1 >= array.length || offset2 >= array.length) {
  //      return
  //    }
  //    if (offset1 < 0) {
  //      offset1 = 0
  //    }
  //    if (offset2 < 0) {
  //      offset2 = 0
  //    }
  //    len = Math.min (Math.min (len, array.length - offset1), array.length - offset2)
  //    var i: Int = 0
  //    while ( {
  //      i < len
  //    }) {
  //      val aux: Byte = array (offset1)
  //      array (offset1) = array (offset2)
  //      array (offset2) = aux
  //
  //      i += 1
  //      offset1 += 1
  //      offset2 += 1
  //    }
  //  }
  //
  //  /**
  //    * Swaps two elements in the given char array.
  //    *
  //    * <p>There is no special handling for multi-dimensional arrays. This method
  //    * does nothing for a {@code null} or empty input array or for overflow indices.
  //    * Negative indices are promoted to 0(zero).</p>
  //    *
  //    * Examples:
  //    * <ul>
  //    * <li>ArrayUtils.swap([1, 2, 3], 0, 2) -&gt; [3, 2, 1]</li>
  //    * <li>ArrayUtils.swap([1, 2, 3], 0, 0) -&gt; [1, 2, 3]</li>
  //    * <li>ArrayUtils.swap([1, 2, 3], 1, 0) -&gt; [2, 1, 3]</li>
  //    * <li>ArrayUtils.swap([1, 2, 3], 0, 5) -&gt; [1, 2, 3]</li>
  //    * <li>ArrayUtils.swap([1, 2, 3], -1, 1) -&gt; [2, 1, 3]</li>
  //    * </ul>
  //    *
  //    * @param array   the array to swap, may be {@code null}
  //    * @param offset1 the index of the first element to swap
  //    * @param offset2 the index of the second element to swap
  //    * @since 3.5
  //    */
  //  def swap (array: Array[Char], offset1: Int, offset2: Int): Unit = {
  //    if (isEmpty (array) ) {
  //      return
  //    }
  //    swap (array, offset1, offset2, 1)
  //  }
  //
  //  /**
  //    * Swaps a series of elements in the given char array.
  //    *
  //    * <p>This method does nothing for a {@code null} or empty input array or
  //    * for overflow indices. Negative indices are promoted to 0(zero). If any
  //    * of the sub-arrays to swap falls outside of the given array, then the
  //    * swap is stopped at the end of the array and as many as possible elements
  //    * are swapped.</p>
  //    *
  //    * Examples:
  //    * <ul>
  //    * <li>ArrayUtils.swap([1, 2, 3, 4], 0, 2, 1) -&gt; [3, 2, 1, 4]</li>
  //    * <li>ArrayUtils.swap([1, 2, 3, 4], 0, 0, 1) -&gt; [1, 2, 3, 4]</li>
  //    * <li>ArrayUtils.swap([1, 2, 3, 4], 2, 0, 2) -&gt; [3, 4, 1, 2]</li>
  //    * <li>ArrayUtils.swap([1, 2, 3, 4], -3, 2, 2) -&gt; [3, 4, 1, 2]</li>
  //    * <li>ArrayUtils.swap([1, 2, 3, 4], 0, 3, 3) -&gt; [4, 2, 3, 1]</li>
  //    * </ul>
  //    *
  //    * @param array   the array to swap, may be {@code null}
  //    * @param offset1 the index of the first element in the series to swap
  //    * @param offset2 the index of the second element in the series to swap
  //    * @param len     the number of elements to swap starting with the given indices
  //    * @since 3.5
  //    */
  //  def swap (array: Array[Char], offset1: Int, offset2: Int, len: Int): Unit = {
  //    if (isEmpty (array) || offset1 >= array.length || offset2 >= array.length) {
  //      return
  //    }
  //    if (offset1 < 0) {
  //      offset1 = 0
  //    }
  //    if (offset2 < 0) {
  //      offset2 = 0
  //    }
  //    len = Math.min (Math.min (len, array.length - offset1), array.length - offset2)
  //    var i: Int = 0
  //    while ( {
  //      i < len
  //    }) {
  //      val aux: Char = array (offset1)
  //      array (offset1) = array (offset2)
  //      array (offset2) = aux
  //
  //      i += 1
  //      offset1 += 1
  //      offset2 += 1
  //    }
  //  }
  //
  //  /**
  //    * Swaps two elements in the given double array.
  //    *
  //    * <p>There is no special handling for multi-dimensional arrays. This method
  //    * does nothing for a {@code null} or empty input array or for overflow indices.
  //    * Negative indices are promoted to 0(zero).</p>
  //    *
  //    * Examples:
  //    * <ul>
  //    * <li>ArrayUtils.swap([1, 2, 3], 0, 2) -&gt; [3, 2, 1]</li>
  //    * <li>ArrayUtils.swap([1, 2, 3], 0, 0) -&gt; [1, 2, 3]</li>
  //    * <li>ArrayUtils.swap([1, 2, 3], 1, 0) -&gt; [2, 1, 3]</li>
  //    * <li>ArrayUtils.swap([1, 2, 3], 0, 5) -&gt; [1, 2, 3]</li>
  //    * <li>ArrayUtils.swap([1, 2, 3], -1, 1) -&gt; [2, 1, 3]</li>
  //    * </ul>
  //    *
  //    * @param array   the array to swap, may be {@code null}
  //    * @param offset1 the index of the first element to swap
  //    * @param offset2 the index of the second element to swap
  //    * @since 3.5
  //    */
  //  def swap (array: Array[Double], offset1: Int, offset2: Int): Unit = {
  //    if (isEmpty (array) ) {
  //      return
  //    }
  //    swap (array, offset1, offset2, 1)
  //  }
  //
  //  /**
  //    * Swaps a series of elements in the given double array.
  //    *
  //    * <p>This method does nothing for a {@code null} or empty input array or
  //    * for overflow indices. Negative indices are promoted to 0(zero). If any
  //    * of the sub-arrays to swap falls outside of the given array, then the
  //    * swap is stopped at the end of the array and as many as possible elements
  //    * are swapped.</p>
  //    *
  //    * Examples:
  //    * <ul>
  //    * <li>ArrayUtils.swap([1, 2, 3, 4], 0, 2, 1) -&gt; [3, 2, 1, 4]</li>
  //    * <li>ArrayUtils.swap([1, 2, 3, 4], 0, 0, 1) -&gt; [1, 2, 3, 4]</li>
  //    * <li>ArrayUtils.swap([1, 2, 3, 4], 2, 0, 2) -&gt; [3, 4, 1, 2]</li>
  //    * <li>ArrayUtils.swap([1, 2, 3, 4], -3, 2, 2) -&gt; [3, 4, 1, 2]</li>
  //    * <li>ArrayUtils.swap([1, 2, 3, 4], 0, 3, 3) -&gt; [4, 2, 3, 1]</li>
  //    * </ul>
  //    *
  //    * @param array   the array to swap, may be {@code null}
  //    * @param offset1 the index of the first element in the series to swap
  //    * @param offset2 the index of the second element in the series to swap
  //    * @param len     the number of elements to swap starting with the given indices
  //    * @since 3.5
  //    */
  //  def swap (array: Array[Double], offset1: Int, offset2: Int, len: Int): Unit = {
  //    if (isEmpty (array) || offset1 >= array.length || offset2 >= array.length) {
  //      return
  //    }
  //    if (offset1 < 0) {
  //      offset1 = 0
  //    }
  //    if (offset2 < 0) {
  //      offset2 = 0
  //    }
  //    len = Math.min (Math.min (len, array.length - offset1), array.length - offset2)
  //    var i: Int = 0
  //    while ( {
  //      i < len
  //    }) {
  //      val aux: Double = array (offset1)
  //      array (offset1) = array (offset2)
  //      array (offset2) = aux
  //
  //      i += 1
  //      offset1 += 1
  //      offset2 += 1
  //    }
  //  }
  //
  //  /**
  //    * Swaps two elements in the given float array.
  //    *
  //    * <p>There is no special handling for multi-dimensional arrays. This method
  //    * does nothing for a {@code null} or empty input array or for overflow indices.
  //    * Negative indices are promoted to 0(zero).</p>
  //    *
  //    * Examples:
  //    * <ul>
  //    * <li>ArrayUtils.swap([1, 2, 3], 0, 2) -&gt; [3, 2, 1]</li>
  //    * <li>ArrayUtils.swap([1, 2, 3], 0, 0) -&gt; [1, 2, 3]</li>
  //    * <li>ArrayUtils.swap([1, 2, 3], 1, 0) -&gt; [2, 1, 3]</li>
  //    * <li>ArrayUtils.swap([1, 2, 3], 0, 5) -&gt; [1, 2, 3]</li>
  //    * <li>ArrayUtils.swap([1, 2, 3], -1, 1) -&gt; [2, 1, 3]</li>
  //    * </ul>
  //    *
  //    * @param array   the array to swap, may be {@code null}
  //    * @param offset1 the index of the first element to swap
  //    * @param offset2 the index of the second element to swap
  //    * @since 3.5
  //    */
  //  def swap (array: Array[Float], offset1: Int, offset2: Int): Unit = {
  //    if (isEmpty (array) ) {
  //      return
  //    }
  //    swap (array, offset1, offset2, 1)
  //  }
  //
  //  /**
  //    * Swaps a series of elements in the given float array.
  //    *
  //    * <p>This method does nothing for a {@code null} or empty input array or
  //    * for overflow indices. Negative indices are promoted to 0(zero). If any
  //    * of the sub-arrays to swap falls outside of the given array, then the
  //    * swap is stopped at the end of the array and as many as possible elements
  //    * are swapped.</p>
  //    *
  //    * Examples:
  //    * <ul>
  //    * <li>ArrayUtils.swap([1, 2, 3, 4], 0, 2, 1) -&gt; [3, 2, 1, 4]</li>
  //    * <li>ArrayUtils.swap([1, 2, 3, 4], 0, 0, 1) -&gt; [1, 2, 3, 4]</li>
  //    * <li>ArrayUtils.swap([1, 2, 3, 4], 2, 0, 2) -&gt; [3, 4, 1, 2]</li>
  //    * <li>ArrayUtils.swap([1, 2, 3, 4], -3, 2, 2) -&gt; [3, 4, 1, 2]</li>
  //    * <li>ArrayUtils.swap([1, 2, 3, 4], 0, 3, 3) -&gt; [4, 2, 3, 1]</li>
  //    * </ul>
  //    *
  //    * @param array   the array to swap, may be {@code null}
  //    * @param offset1 the index of the first element in the series to swap
  //    * @param offset2 the index of the second element in the series to swap
  //    * @param len     the number of elements to swap starting with the given indices
  //    * @since 3.5
  //    */
  //  def swap (array: Array[Float], offset1: Int, offset2: Int, len: Int): Unit = {
  //    if (isEmpty (array) || offset1 >= array.length || offset2 >= array.length) {
  //      return
  //    }
  //    if (offset1 < 0) {
  //      offset1 = 0
  //    }
  //    if (offset2 < 0) {
  //      offset2 = 0
  //    }
  //    len = Math.min (Math.min (len, array.length - offset1), array.length - offset2)
  //    var i: Int = 0
  //    while ( {
  //      i < len
  //    }) {
  //      val aux: Float = array (offset1)
  //      array (offset1) = array (offset2)
  //      array (offset2) = aux
  //
  //      i += 1
  //      offset1 += 1
  //      offset2 += 1
  //    }
  //  }
  //
  //  /**
  //    * Swaps two elements in the given int array.
  //    *
  //    * <p>There is no special handling for multi-dimensional arrays. This method
  //    * does nothing for a {@code null} or empty input array or for overflow indices.
  //    * Negative indices are promoted to 0(zero).</p>
  //    *
  //    * Examples:
  //    * <ul>
  //    * <li>ArrayUtils.swap([1, 2, 3], 0, 2) -&gt; [3, 2, 1]</li>
  //    * <li>ArrayUtils.swap([1, 2, 3], 0, 0) -&gt; [1, 2, 3]</li>
  //    * <li>ArrayUtils.swap([1, 2, 3], 1, 0) -&gt; [2, 1, 3]</li>
  //    * <li>ArrayUtils.swap([1, 2, 3], 0, 5) -&gt; [1, 2, 3]</li>
  //    * <li>ArrayUtils.swap([1, 2, 3], -1, 1) -&gt; [2, 1, 3]</li>
  //    * </ul>
  //    *
  //    * @param array   the array to swap, may be {@code null}
  //    * @param offset1 the index of the first element to swap
  //    * @param offset2 the index of the second element to swap
  //    * @since 3.5
  //    */
  //  def swap (array: Array[Int], offset1: Int, offset2: Int): Unit = {
  //    if (isEmpty (array) ) {
  //      return
  //    }
  //    swap (array, offset1, offset2, 1)
  //  }
  //
  //  /**
  //    * Swaps a series of elements in the given int array.
  //    *
  //    * <p>This method does nothing for a {@code null} or empty input array or
  //    * for overflow indices. Negative indices are promoted to 0(zero). If any
  //    * of the sub-arrays to swap falls outside of the given array, then the
  //    * swap is stopped at the end of the array and as many as possible elements
  //    * are swapped.</p>
  //    *
  //    * Examples:
  //    * <ul>
  //    * <li>ArrayUtils.swap([1, 2, 3, 4], 0, 2, 1) -&gt; [3, 2, 1, 4]</li>
  //    * <li>ArrayUtils.swap([1, 2, 3, 4], 0, 0, 1) -&gt; [1, 2, 3, 4]</li>
  //    * <li>ArrayUtils.swap([1, 2, 3, 4], 2, 0, 2) -&gt; [3, 4, 1, 2]</li>
  //    * <li>ArrayUtils.swap([1, 2, 3, 4], -3, 2, 2) -&gt; [3, 4, 1, 2]</li>
  //    * <li>ArrayUtils.swap([1, 2, 3, 4], 0, 3, 3) -&gt; [4, 2, 3, 1]</li>
  //    * </ul>
  //    *
  //    * @param array   the array to swap, may be {@code null}
  //    * @param offset1 the index of the first element in the series to swap
  //    * @param offset2 the index of the second element in the series to swap
  //    * @param len     the number of elements to swap starting with the given indices
  //    * @since 3.5
  //    */
  //  def swap (array: Array[Int], offset1: Int, offset2: Int, len: Int): Unit = {
  //    if (isEmpty (array) || offset1 >= array.length || offset2 >= array.length) {
  //      return
  //    }
  //    if (offset1 < 0) {
  //      offset1 = 0
  //    }
  //    if (offset2 < 0) {
  //      offset2 = 0
  //    }
  //    len = Math.min (Math.min (len, array.length - offset1), array.length - offset2)
  //    var i: Int = 0
  //    while ( {
  //      i < len
  //    }) {
  //      val aux: Int = array (offset1)
  //      array (offset1) = array (offset2)
  //      array (offset2) = aux
  //
  //      i += 1
  //      offset1 += 1
  //      offset2 += 1
  //    }
  //  }
  //
  //  /**
  //    * Swaps two elements in the given long array.
  //    *
  //    * <p>There is no special handling for multi-dimensional arrays. This method
  //    * does nothing for a {@code null} or empty input array or for overflow indices.
  //    * Negative indices are promoted to 0(zero).</p>
  //    *
  //    * Examples:
  //    * <ul>
  //    * <li>ArrayUtils.swap([true, false, true], 0, 2) -&gt; [true, false, true]</li>
  //    * <li>ArrayUtils.swap([true, false, true], 0, 0) -&gt; [true, false, true]</li>
  //    * <li>ArrayUtils.swap([true, false, true], 1, 0) -&gt; [false, true, true]</li>
  //    * <li>ArrayUtils.swap([true, false, true], 0, 5) -&gt; [true, false, true]</li>
  //    * <li>ArrayUtils.swap([true, false, true], -1, 1) -&gt; [false, true, true]</li>
  //    * </ul>
  //    *
  //    * @param array   the array to swap, may be {@code null}
  //    * @param offset1 the index of the first element to swap
  //    * @param offset2 the index of the second element to swap
  //    * @since 3.5
  //    */
  //  def swap (array: Array[Long], offset1: Int, offset2: Int): Unit = {
  //    if (isEmpty (array) ) {
  //      return
  //    }
  //    swap (array, offset1, offset2, 1)
  //  }
  //
  //  /**
  //    * Swaps a series of elements in the given long array.
  //    *
  //    * <p>This method does nothing for a {@code null} or empty input array or
  //    * for overflow indices. Negative indices are promoted to 0(zero). If any
  //    * of the sub-arrays to swap falls outside of the given array, then the
  //    * swap is stopped at the end of the array and as many as possible elements
  //    * are swapped.</p>
  //    *
  //    * Examples:
  //    * <ul>
  //    * <li>ArrayUtils.swap([1, 2, 3, 4], 0, 2, 1) -&gt; [3, 2, 1, 4]</li>
  //    * <li>ArrayUtils.swap([1, 2, 3, 4], 0, 0, 1) -&gt; [1, 2, 3, 4]</li>
  //    * <li>ArrayUtils.swap([1, 2, 3, 4], 2, 0, 2) -&gt; [3, 4, 1, 2]</li>
  //    * <li>ArrayUtils.swap([1, 2, 3, 4], -3, 2, 2) -&gt; [3, 4, 1, 2]</li>
  //    * <li>ArrayUtils.swap([1, 2, 3, 4], 0, 3, 3) -&gt; [4, 2, 3, 1]</li>
  //    * </ul>
  //    *
  //    * @param array   the array to swap, may be {@code null}
  //    * @param offset1 the index of the first element in the series to swap
  //    * @param offset2 the index of the second element in the series to swap
  //    * @param len     the number of elements to swap starting with the given indices
  //    * @since 3.5
  //    */
  //  def swap (array: Array[Long], offset1: Int, offset2: Int, len: Int): Unit = {
  //    if (isEmpty (array) || offset1 >= array.length || offset2 >= array.length) {
  //      return
  //    }
  //    if (offset1 < 0) {
  //      offset1 = 0
  //    }
  //    if (offset2 < 0) {
  //      offset2 = 0
  //    }
  //    len = Math.min (Math.min (len, array.length - offset1), array.length - offset2)
  //    var i: Int = 0
  //    while ( {
  //      i < len
  //    }) {
  //      val aux: Long = array (offset1)
  //      array (offset1) = array (offset2)
  //      array (offset2) = aux
  //
  //      i += 1
  //      offset1 += 1
  //      offset2 += 1
  //    }
  //  }
  //
  //  /**
  //    * Swaps two elements in the given array.
  //    *
  //    * <p>There is no special handling for multi-dimensional arrays. This method
  //    * does nothing for a {@code null} or empty input array or for overflow indices.
  //    * Negative indices are promoted to 0(zero).</p>
  //    *
  //    * Examples:
  //    * <ul>
  //    * <li>ArrayUtils.swap(["1", "2", "3"], 0, 2) -&gt; ["3", "2", "1"]</li>
  //    * <li>ArrayUtils.swap(["1", "2", "3"], 0, 0) -&gt; ["1", "2", "3"]</li>
  //    * <li>ArrayUtils.swap(["1", "2", "3"], 1, 0) -&gt; ["2", "1", "3"]</li>
  //    * <li>ArrayUtils.swap(["1", "2", "3"], 0, 5) -&gt; ["1", "2", "3"]</li>
  //    * <li>ArrayUtils.swap(["1", "2", "3"], -1, 1) -&gt; ["2", "1", "3"]</li>
  //    * </ul>
  //    *
  //    * @param array   the array to swap, may be {@code null}
  //    * @param offset1 the index of the first element to swap
  //    * @param offset2 the index of the second element to swap
  //    * @since 3.5
  //    */
  //  def swap (array: Array[AnyRef], offset1: Int, offset2: Int): Unit = {
  //    if (isEmpty (array) ) {
  //      return
  //    }
  //    swap (array, offset1, offset2, 1)
  //  }
  //
  //  /**
  //    * Swaps a series of elements in the given array.
  //    *
  //    * <p>This method does nothing for a {@code null} or empty input array or
  //    * for overflow indices. Negative indices are promoted to 0(zero). If any
  //    * of the sub-arrays to swap falls outside of the given array, then the
  //    * swap is stopped at the end of the array and as many as possible elements
  //    * are swapped.</p>
  //    *
  //    * Examples:
  //    * <ul>
  //    * <li>ArrayUtils.swap(["1", "2", "3", "4"], 0, 2, 1) -&gt; ["3", "2", "1", "4"]</li>
  //    * <li>ArrayUtils.swap(["1", "2", "3", "4"], 0, 0, 1) -&gt; ["1", "2", "3", "4"]</li>
  //    * <li>ArrayUtils.swap(["1", "2", "3", "4"], 2, 0, 2) -&gt; ["3", "4", "1", "2"]</li>
  //    * <li>ArrayUtils.swap(["1", "2", "3", "4"], -3, 2, 2) -&gt; ["3", "4", "1", "2"]</li>
  //    * <li>ArrayUtils.swap(["1", "2", "3", "4"], 0, 3, 3) -&gt; ["4", "2", "3", "1"]</li>
  //    * </ul>
  //    *
  //    * @param array   the array to swap, may be {@code null}
  //    * @param offset1 the index of the first element in the series to swap
  //    * @param offset2 the index of the second element in the series to swap
  //    * @param len     the number of elements to swap starting with the given indices
  //    * @since 3.5
  //    */
  //  def swap (array: Array[AnyRef], offset1: Int, offset2: Int, len: Int): Unit = {
  //    if (isEmpty (array) || offset1 >= array.length || offset2 >= array.length) {
  //      return
  //    }
  //    if (offset1 < 0) {
  //      offset1 = 0
  //    }
  //    if (offset2 < 0) {
  //      offset2 = 0
  //    }
  //    len = Math.min (Math.min (len, array.length - offset1), array.length - offset2)
  //    var i: Int = 0
  //    while ( {
  //      i < len
  //    }) {
  //      val aux: Any = array (offset1)
  //      array (offset1) = array (offset2)
  //      array (offset2) = aux
  //
  //      i += 1
  //      offset1 += 1
  //      offset2 += 1
  //    }
  //  }
  //
  //  /**
  //    * Swaps two elements in the given short array.
  //    *
  //    * <p>There is no special handling for multi-dimensional arrays. This method
  //    * does nothing for a {@code null} or empty input array or for overflow indices.
  //    * Negative indices are promoted to 0(zero).</p>
  //    *
  //    * Examples:
  //    * <ul>
  //    * <li>ArrayUtils.swap([1, 2, 3], 0, 2) -&gt; [3, 2, 1]</li>
  //    * <li>ArrayUtils.swap([1, 2, 3], 0, 0) -&gt; [1, 2, 3]</li>
  //    * <li>ArrayUtils.swap([1, 2, 3], 1, 0) -&gt; [2, 1, 3]</li>
  //    * <li>ArrayUtils.swap([1, 2, 3], 0, 5) -&gt; [1, 2, 3]</li>
  //    * <li>ArrayUtils.swap([1, 2, 3], -1, 1) -&gt; [2, 1, 3]</li>
  //    * </ul>
  //    *
  //    * @param array   the array to swap, may be {@code null}
  //    * @param offset1 the index of the first element to swap
  //    * @param offset2 the index of the second element to swap
  //    * @since 3.5
  //    */
  //  def swap (array: Array[Short], offset1: Int, offset2: Int): Unit = {
  //    if (isEmpty (array) ) {
  //      return
  //    }
  //    swap (array, offset1, offset2, 1)
  //  }
  //
  //  /**
  //    * Swaps a series of elements in the given short array.
  //    *
  //    * <p>This method does nothing for a {@code null} or empty input array or
  //    * for overflow indices. Negative indices are promoted to 0(zero). If any
  //    * of the sub-arrays to swap falls outside of the given array, then the
  //    * swap is stopped at the end of the array and as many as possible elements
  //    * are swapped.</p>
  //    *
  //    * Examples:
  //    * <ul>
  //    * <li>ArrayUtils.swap([1, 2, 3, 4], 0, 2, 1) -&gt; [3, 2, 1, 4]</li>
  //    * <li>ArrayUtils.swap([1, 2, 3, 4], 0, 0, 1) -&gt; [1, 2, 3, 4]</li>
  //    * <li>ArrayUtils.swap([1, 2, 3, 4], 2, 0, 2) -&gt; [3, 4, 1, 2]</li>
  //    * <li>ArrayUtils.swap([1, 2, 3, 4], -3, 2, 2) -&gt; [3, 4, 1, 2]</li>
  //    * <li>ArrayUtils.swap([1, 2, 3, 4], 0, 3, 3) -&gt; [4, 2, 3, 1]</li>
  //    * </ul>
  //    *
  //    * @param array   the array to swap, may be {@code null}
  //    * @param offset1 the index of the first element in the series to swap
  //    * @param offset2 the index of the second element in the series to swap
  //    * @param len     the number of elements to swap starting with the given indices
  //    * @since 3.5
  //    */
  //  def swap (array: Array[Short], offset1: Int, offset2: Int, len: Int): Unit = {
  //    if (isEmpty (array) || offset1 >= array.length || offset2 >= array.length) {
  //      return
  //    }
  //    if (offset1 < 0) {
  //      offset1 = 0
  //    }
  //    if (offset2 < 0) {
  //      offset2 = 0
  //    }
  //    if (offset1 == offset2) {
  //      return
  //    }
  //    len = Math.min (Math.min (len, array.length - offset1), array.length - offset2)
  //    var i: Int = 0
  //    while ( {
  //      i < len
  //    }) {
  //      val aux: Short = array (offset1)
  //      array (offset1) = array (offset2)
  //      array (offset2) = aux
  //
  //      i += 1
  //      offset1 += 1
  //      offset2 += 1
  //    }
  //  }
  //
  //  /**
  //    * <p>Create a type-safe generic array.
  //    *
  //    * <p>The Java language does not allow an array to be created from a generic type:
  //    *
  //    * <pre>
  //    * public static &lt;T&gt; T[] createAnArray(int size) {
  //    * return new T[size]; // compiler error here
  //    * }
  //    * public static &lt;T&gt; T[] createAnArray(int size) {
  //    * return (T[]) new Object[size]; // ClassCastException at runtime
  //    * }
  //    * </pre>
  //    *
  //    * <p>Therefore new arrays of generic types can be created with this method.
  //    * For example, an array of Strings can be created:
  //    *
  //    * <pre>
  //    * String[] array = ArrayUtils.toArray("1", "2");
  //    * String[] emptyArray = ArrayUtils.&lt;String&gt;toArray();
  //    * </pre>
  //    *
  //    * <p>The method is typically used in scenarios, where the caller itself uses generic types
  //    * that have to be combined into an array.
  //    *
  //    * <p>Note, this method makes only sense to provide arguments of the same type so that the
  //    * compiler can deduce the type of the array itself. While it is possible to select the
  //    * type explicitly like in
  //    * {@code Number[] array = ArrayUtils.&lt;Number&gt;toArray(Integer.valueOf(42), Double.valueOf(Math.PI))},
  //    * there is no real advantage when compared to
  //    * {@code new Number[] {Integer.valueOf(42), Double.valueOf(Math.PI)}}.
  //    *
  //    * @tparam T    the array's element type
  //    * @param items the varargs array items, null allowed
  //    * @return the array, not null unless a null array is passed in
  //    * @since 3.0
  //    */
  //  def toArray[T] (@SuppressWarnings (Array ("unchecked") ) items: T *): Array[T] = {
  //    return items
  //  }
  //
  //  /**
  //    * <p>Converts the given array into a {@link java.ju.Map}. Each element of the array
  //    * must be either a {@link java.ju.Map.Entry} or an Array, containing at least two
  //    * elements, where the first element is used as key and the second as
  //    * value.
  //    *
  //    * <p>This method can be used to initialize:
  //    * <pre>
  //    * // Create a Map mapping colors.
  //    * Map colorMap = ArrayUtils.toMap(new String[][] {
  //    * {"RED", "#FF0000"},
  //    * {"GREEN", "#00FF00"},
  //    * {"BLUE", "#0000FF"}});
  //    * </pre>
  //    *
  //    * <p>This method returns {@code null} for a {@code null} input array.
  //    *
  //    * @param array an array whose elements are either a {@link java.ju.Map.Entry} or
  //    *              an Array containing at least two elements, may be {@code null}
  //    * @return a {@code Map} that was created from the array
  //    * @throws IllegalArgumentException if one element of this Array is
  //    *                                  itself an Array containing less then two elements
  //    * @throws IllegalArgumentException if the array contains elements other
  //    *                                  than {@link java.ju.Map.Entry} and an Array
  //    */
  //  def toMap (array: Array[AnyRef] ): ju.Map[AnyRef, AnyRef] = {
  //    if (array == null) return null
  //    val map: ju.Map[AnyRef, AnyRef] = new ju.HashMap[AnyRef, AnyRef] ((array.length * 1.5).toInt)
  //    for (i <- 0 until array.length) {
  //      val `object`: Any = array (i)
  //      if (`object`.isInstanceOf[ju.Map.Entry[_, _]] ) {
  //        val entry: ju.Map.Entry[_, _] = `object`.asInstanceOf[ju.Map.Entry[_, _]]
  //        map.put (entry.getKey, entry.getValue)
  //      }
  //      else {
  //        if (`object`.isInstanceOf[Array[AnyRef]] ) {
  //          val entry: Array[AnyRef] = `object`.asInstanceOf[Array[AnyRef]]
  //          if (entry.length < 2) {
  //            throw new IllegalArgumentException ("Array element " + i + ", '" + `object` + "', has a length less than 2")
  //          }
  //          map.put (entry (0), entry (1) )
  //        }
  //        else {
  //          throw new IllegalArgumentException ("Array element " + i + ", '" + `object` + "', is neither of type Map.Entry nor an Array")
  //        }
  //      }
  //    }
  //    return map
  //  }
  //
  //  /**
  //    * <p>Converts an array of primitive booleans to objects.
  //    *
  //    * <p>This method returns {@code null} for a {@code null} input array.
  //    *
  //    * @param array a {@code boolean} array
  //    * @return a {@code Boolean} array, {@code null} if null array input
  //    */
  //  def toObject (array: Array[Boolean] ): Array[Boolean] = {
  //    if (array == null) return null
  //    else {
  //      if (array.length == 0) {
  //        return EMPTY_BOOLEAN_OBJECT_ARRAY
  //      }
  //    }
  //    val result: Array[Boolean] = new Array[Boolean] (array.length)
  //    for (i <- 0 until array.length) {
  //      result (i) = (if (array (i) ) {
  //        Boolean.TRUE
  //      }
  //      else {
  //        Boolean.FALSE
  //      })
  //    }
  //    return result
  //  }
  //
  //  /**
  //    * <p>Converts an array of primitive bytes to objects.
  //    *
  //    * <p>This method returns {@code null} for a {@code null} input array.
  //    *
  //    * @param array a {@code byte} array
  //    * @return a {@code Byte} array, {@code null} if null array input
  //    */
  //  def toObject (array: Array[Byte] ): Array[Byte] = {
  //    if (array == null) return null
  //    else {
  //      if (array.length == 0) {
  //        return EMPTY_BYTE_OBJECT_ARRAY
  //      }
  //    }
  //    val result: Array[Byte] = new Array[Byte] (array.length)
  //    for (i <- 0 until array.length) {
  //      result (i) = Byte.valueOf (array (i) )
  //    }
  //    return result
  //  }
  //
  //  /**
  //    * <p>Converts an array of primitive chars to objects.
  //    *
  //    * <p>This method returns {@code null} for a {@code null} input array.
  //    *
  //    * @param array a {@code char} array
  //    * @return a {@code Character} array, {@code null} if null array input
  //    */
  //  def toObject (array: Array[Char] ): Array[Character] = {
  //    if (array == null) return null
  //    else {
  //      if (array.length == 0) {
  //        return EMPTY_CHARACTER_OBJECT_ARRAY
  //      }
  //    }
  //    val result: Array[Character] = new Array[Character] (array.length)
  //    for (i <- 0 until array.length) {
  //      result (i) = Character.valueOf (array (i) )
  //    }
  //    return result
  //  }
  //
  //  /**
  //    * <p>Converts an array of primitive doubles to objects.
  //    *
  //    * <p>This method returns {@code null} for a {@code null} input array.
  //    *
  //    * @param array a {@code double} array
  //    * @return a {@code Double} array, {@code null} if null array input
  //    */
  //  def toObject (array: Array[Double] ): Array[Double] = {
  //    if (array == null) return null
  //    else {
  //      if (array.length == 0) {
  //        return EMPTY_DOUBLE_OBJECT_ARRAY
  //      }
  //    }
  //    val result: Array[Double] = new Array[Double] (array.length)
  //    for (i <- 0 until array.length) {
  //      result (i) = Double.valueOf (array (i) )
  //    }
  //    return result
  //  }
  //
  //  /**
  //    * <p>Converts an array of primitive floats to objects.
  //    *
  //    * <p>This method returns {@code null} for a {@code null} input array.
  //    *
  //    * @param array a {@code float} array
  //    * @return a {@code Float} array, {@code null} if null array input
  //    */
  //  def toObject (array: Array[Float] ): Array[Float] = {
  //    if (array == null) return null
  //    else {
  //      if (array.length == 0) {
  //        return EMPTY_FLOAT_OBJECT_ARRAY
  //      }
  //    }
  //    val result: Array[Float] = new Array[Float] (array.length)
  //    for (i <- 0 until array.length) {
  //      result (i) = Float.valueOf (array (i) )
  //    }
  //    return result
  //  }
  //
  //  /**
  //    * <p>Converts an array of primitive ints to objects.
  //    *
  //    * <p>This method returns {@code null} for a {@code null} input array.
  //    *
  //    * @param array an {@code int} array
  //    * @return an {@code Integer} array, {@code null} if null array input
  //    */
  //  def toObject (array: Array[Int] ): Array[Integer] = {
  //    if (array == null) return null
  //    else {
  //      if (array.length == 0) {
  //        return EMPTY_INTEGER_OBJECT_ARRAY
  //      }
  //    }
  //    val result: Array[Integer] = new Array[Integer] (array.length)
  //    for (i <- 0 until array.length) {
  //      result (i) = Integer.valueOf (array (i) )
  //    }
  //    return result
  //  }
  //
  //  /**
  //    * <p>Converts an array of primitive longs to objects.
  //    *
  //    * <p>This method returns {@code null} for a {@code null} input array.
  //    *
  //    * @param array a {@code long} array
  //    * @return a {@code Long} array, {@code null} if null array input
  //    */
  //  def toObject (array: Array[Long] ): Array[Long] = {
  //    if (array == null) return null
  //    else {
  //      if (array.length == 0) {
  //        return EMPTY_LONG_OBJECT_ARRAY
  //      }
  //    }
  //    val result: Array[Long] = new Array[Long] (array.length)
  //    for (i <- 0 until array.length) {
  //      result (i) = Long.valueOf (array (i) )
  //    }
  //    return result
  //  }
  //
  //  /**
  //    * <p>Converts an array of primitive shorts to objects.
  //    *
  //    * <p>This method returns {@code null} for a {@code null} input array.
  //    *
  //    * @param array a {@code short} array
  //    * @return a {@code Short} array, {@code null} if null array input
  //    */
  //  def toObject (array: Array[Short] ): Array[Short] = {
  //    if (array == null) return null
  //    else {
  //      if (array.length == 0) {
  //        return EMPTY_SHORT_OBJECT_ARRAY
  //      }
  //    }
  //    val result: Array[Short] = new Array[Short] (array.length)
  //    for (i <- 0 until array.length) {
  //      result (i) = Short.valueOf (array (i) )
  //    }
  //    return result
  //  }
  //
  /**
    * <p>Converts an array of object Booleans to primitives.
    *
    * <p>This method returns {@code null} for a {@code null} input array.
    *
    * @param array a {@code Boolean} array, may be {@code null}
    * @return a {@code boolean} array, {@code null} if null array input
    * @throws NullPointerException if array content is {@code null}
    */
  def toPrimitive(array: Array[JavaBoolean]): Array[Boolean] = {
    if (array == null) return null
    else {
      if (array.length == 0) {
        return EMPTY_BOOLEAN_ARRAY
      }
    }
    val result: Array[Boolean] = new Array[Boolean] (array.length)
    for (i <- 0 until array.length) {
      result (i) = array (i).booleanValue
    }
    return result
  }

  /**
    * <p>Converts an array of object Booleans to primitives handling {@code null}.
    *
    * <p>This method returns {@code null} for a {@code null} input array.
    *
    * @param array        a {@code Boolean} array, may be {@code null}
    * @param valueForNull the value to insert if {@code null} found
    * @return a {@code boolean} array, {@code null} if null array input
    */
  def toPrimitive(array: Array[JavaBoolean], valueForNull: Boolean): Array[Boolean] = {
    if (array == null) return null
    if (array.length == 0) return EMPTY_BOOLEAN_ARRAY

    val result: Array[Boolean] = new Array[Boolean](array.length)
    for (i <- 0 until array.length) {
      val b: Boolean = array (i)
      result(i) =
        if (b == null) valueForNull
        else b.booleanValue
    }

    result
  }

  /**
    * <p>Converts an array of object Bytes to primitives.
    *
    * <p>This method returns {@code null} for a {@code null} input array.
    *
    * @param array a {@code Byte} array, may be {@code null}
    * @return a {@code byte} array, {@code null} if null array input
    * @throws NullPointerException if array content is {@code null}
    */
  def toPrimitive(array: Array[JavaByte]): Array[Byte] = {
    if (array == null) return null
    else {
      if (array.length == 0) {
        return EMPTY_BYTE_ARRAY
      }
    }

    val result: Array[Byte] = new Array[Byte](array.length)
    for (i <- 0 until array.length) {
      result(i) = array(i).byteValue
    }

    result
  }

  /**
    * <p>Converts an array of object Bytes to primitives handling {@code null}.
    *
    * <p>This method returns {@code null} for a {@code null} input array.
    *
    * @param array        a {@code Byte} array, may be {@code null}
    * @param valueForNull the value to insert if {@code null} found
    * @return a {@code byte} array, {@code null} if null array input
    */
  def toPrimitive(array: Array[JavaByte], valueForNull: Byte): Array[Byte] = {
    if (array == null) return null
    if (array.length == 0) return EMPTY_BYTE_ARRAY

    val result: Array[Byte] = new Array[Byte](array.length)
    for (i <- 0 until array.length) {
      val b: Byte = array (i)
      result(i) =
        if (b == null) valueForNull
        else b.byteValue
    }

    result
  }

  /**
    * <p>Converts an array of object Characters to primitives.
    *
    * <p>This method returns {@code null} for a {@code null} input array.
    *
    * @param array a {@code Character} array, may be {@code null}
    * @return a {@code char} array, {@code null} if null array input
    * @throws NullPointerException if array content is {@code null}
    */
  def toPrimitive(array: Array[Character]): Array[Char] = {
    if (array == null) return null
    if (array.length == 0) return EMPTY_CHAR_ARRAY

    val result: Array[Char] = new Array[Char](array.length)
    for (i <- 0 until array.length) {
      result(i) = array(i).charValue
    }

    result
  }

  /**
    * <p>Converts an array of object Character to primitives handling {@code null}.
    *
    * <p>This method returns {@code null} for a {@code null} input array.
    *
    * @param array        a {@code Character} array, may be {@code null}
    * @param valueForNull the value to insert if {@code null} found
    * @return a {@code char} array, {@code null} if null array input
    */
  def toPrimitive(array: Array[Character], valueForNull: Char): Array[Char] = {
    if (array == null) return null
    if (array.length == 0) return EMPTY_CHAR_ARRAY

    val result: Array[Char] = new Array[Char](array.length)
    for (i <- 0 until array.length) {
      val b: Character = array(i)
      result(i) =
        if (b == null) valueForNull
        else b.charValue
    }

    result
  }

  /**
    * <p>Converts an array of object Doubles to primitives.
    *
    * <p>This method returns {@code null} for a {@code null} input array.
    *
    * @param array a {@code Double} array, may be {@code null}
    * @return a {@code double} array, {@code null} if null array input
    * @throws NullPointerException if array content is {@code null}
    */
  def toPrimitive(array: Array[JavaDouble]): Array[Double] = {
    if (array == null) return null
    if (array.length == 0) return EMPTY_DOUBLE_ARRAY

    val result: Array[Double] = new Array[Double](array.length)
    for (i <- 0 until array.length) {
      result(i) = array (i).doubleValue
    }

    result
  }

  /**
    * <p>Converts an array of object Doubles to primitives handling {@code null}.
    *
    * <p>This method returns {@code null} for a {@code null} input array.
    *
    * @param array        a {@code Double} array, may be {@code null}
    * @param valueForNull the value to insert if {@code null} found
    * @return a {@code double} array, {@code null} if null array input
    */
  def toPrimitive(array: Array[JavaDouble], valueForNull: Double): Array[Double] = {
    if (array == null) return null
    if (array.length == 0) return EMPTY_DOUBLE_ARRAY

    val result: Array[Double] = new Array[Double](array.length)
    for (i <- 0 until array.length) {
      val b: Double = array (i)
      result(i) =
        if (b == null) valueForNull
        else b.doubleValue
    }

    result
  }

  /**
    * <p>Converts an array of object Floats to primitives.
    *
    * <p>This method returns {@code null} for a {@code null} input array.
    *
    * @param array a {@code Float} array, may be {@code null}
    * @return a {@code float} array, {@code null} if null array input
    * @throws NullPointerException if array content is {@code null}
    */
  def toPrimitive(array: Array[JavaFloat] ): Array[Float] = {
    if (array == null) return null
    if (array.length == 0) return EMPTY_FLOAT_ARRAY

    val result: Array[Float] = new Array[Float](array.length)
    for (i <- 0 until array.length) {
      result(i) = array(i).floatValue
    }

    result
  }

  /**
    * <p>Converts an array of object Floats to primitives handling {@code null}.
    *
    * <p>This method returns {@code null} for a {@code null} input array.
    *
    * @param array        a {@code Float} array, may be {@code null}
    * @param valueForNull the value to insert if {@code null} found
    * @return a {@code float} array, {@code null} if null array input
    */
  def toPrimitive(array: Array[JavaFloat], valueForNull: Float): Array[Float] = {
    if (array == null) return null
    if (array.length == 0) return EMPTY_FLOAT_ARRAY

    val result: Array[Float] = new Array[Float] (array.length)
    for (i <- 0 until array.length) {
      val b: Float = array (i)
      result(i) =
        if (b == null) valueForNull
        else b.floatValue
    }

    result
  }

  /**
    * <p>Converts an array of object Integers to primitives.
    *
    * <p>This method returns {@code null} for a {@code null} input array.
    *
    * @param array a {@code Integer} array, may be {@code null}
    * @return an {@code int} array, {@code null} if null array input
    * @throws NullPointerException if array content is {@code null}
    */
  def toPrimitive(array: Array[Integer]): Array[Int] = {
    if (array == null) return null
    if (array.length == 0) return EMPTY_INT_ARRAY

    val result: Array[Int] = new Array[Int] (array.length)
    for (i <- 0 until array.length) {
      result(i) = array(i).intValue
    }

    result
  }

  /**
    * <p>Converts an array of object Integer to primitives handling {@code null}.
    *
    * <p>This method returns {@code null} for a {@code null} input array.
    *
    * @param array        a {@code Integer} array, may be {@code null}
    * @param valueForNull the value to insert if {@code null} found
    * @return an {@code int} array, {@code null} if null array input
    */
  def toPrimitive(array: Array[Integer], valueForNull: Int): Array[Int] = {
    if (array == null) return null
    else if (array.length == 0) return EMPTY_INT_ARRAY

    val result: Array[Int] = new Array[Int](array.length)
    for (i <- 0 until array.length) {
      val b: Integer = array (i)
      result(i) =
        if (b == null) valueForNull
        else b.intValue
    }

    result
  }

  /**
    * <p>Converts an array of object Longs to primitives.
    *
    * <p>This method returns {@code null} for a {@code null} input array.
    *
    * @param array a {@code Long} array, may be {@code null}
    * @return a {@code long} array, {@code null} if null array input
    * @throws NullPointerException if array content is {@code null}
    */
  def toPrimitive(array: Array[JavaLong]): Array[Long] = {
    if (array == null) return null
    else if (array.length == 0) return EMPTY_LONG_ARRAY

    val result: Array[Long] = new Array[Long](array.length)
    for (i <- 0 until array.length) {
      result(i) = array(i).longValue
    }

    result
  }

  /**
    * <p>Converts an array of object Long to primitives handling {@code null}.
    *
    * <p>This method returns {@code null} for a {@code null} input array.
    *
    * @param array        a {@code Long} array, may be {@code null}
    * @param valueForNull the value to insert if {@code null} found
    * @return a {@code long} array, {@code null} if null array input
    */
  def toPrimitive(array: Array[JavaLong], valueForNull: Long): Array[Long] = {
    if (array == null) return null
    if (array.length == 0) return EMPTY_LONG_ARRAY

    val result: Array[Long] = new Array[Long](array.length)
    for (i <- 0 until array.length) {
      val b: Long = array(i)
      result (i) =
        if (b == null) valueForNull
        else b.longValue
    }

    result
  }

  /**
    * <p>Create an array of primitive type from an array of wrapper types.
    *
    * <p>This method returns {@code null} for a {@code null} input array.
    *
    * @param array an array of wrapper object
    * @return an array of the corresponding primitive type, or the original array
    * @since 3.5
    */
  def toPrimitive(array: Array[_]): Array[_] = {
    if (array == null) return null
    val ct: Class[_] = array.getClass.getComponentType
    val pt: Class[_] = ClassUtils.wrapperToPrimitive(ct)

    if (Integer.TYPE == pt) toPrimitive(array.asInstanceOf[Array[Integer]])
    else if (JavaLong.TYPE == pt) toPrimitive(array.asInstanceOf[Array[JavaLong]])
    else if (JavaShort.TYPE == pt) toPrimitive(array.asInstanceOf[Array[JavaShort]])
    else if (JavaDouble.TYPE == pt) toPrimitive(array.asInstanceOf[Array[JavaDouble]])
    else if (JavaFloat.TYPE == pt) toPrimitive(array.asInstanceOf[Array[JavaFloat]])
    // TODO: No Char, Bool or Byte?
    else array
  }

  /**
    * <p>Converts an array of object Shorts to primitives.
    *
    * <p>This method returns {@code null} for a {@code null} input array.
    *
    * @param array a {@code Short} array, may be {@code null}
    * @return a {@code byte} array, {@code null} if null array input
    * @throws NullPointerException if array content is {@code null}
    */
  def toPrimitive(array: Array[JavaShort]): Array[Short] = {
    if (array == null) return null
    if (array.length == 0) EMPTY_SHORT_ARRAY

    val result: Array[Short] = new Array[Short](array.length)
    for (i <- 0 until array.length) {
      result(i) = array(i).shortValue
    }

    result
  }

  /**
    * <p>Converts an array of object Short to primitives handling {@code null}.
    *
    * <p>This method returns {@code null} for a {@code null} input array.
    *
    * @param array        a {@code Short} array, may be {@code null}
    * @param valueForNull the value to insert if {@code null} found
    * @return a {@code byte} array, {@code null} if null array input
    */
  def toPrimitive (array: Array[JavaShort], valueForNull: Short): Array[Short] = {
    if (array == null) return null
    if (array.length == 0) return EMPTY_SHORT_ARRAY

    val result: Array[Short] = new Array[Short](array.length)
    for (i <- 0 until array.length) {
      val b: Short = array (i)
      result(i) =
        if (b == null) valueForNull
        else b.shortValue
    }

    result
  }

  /**
    * <p>Outputs an array as a String, treating {@code null} as an empty array.
    *
    * <p>Multi-dimensional arrays are handled correctly, including
    * multi-dimensional primitive arrays.
    *
    * <p>The format is that of Java source code, for example {@code {a,b}}.
    *
    * @param array the array to get a toString for, may be {@code null}
    * @return a String representation of the array, '{}' if null array input
    */
  def toString(array: AnyRef): String = toString(array, "{}")

  /**
    * <p>Outputs an array as a String handling {@code null}s.
    *
    * <p>Multi-dimensional arrays are handled correctly, including
    * multi-dimensional primitive arrays.
    *
    * <p>The format is that of Java source code, for example {@code {a,b}}.
    *
    * @param array        the array to get a toString for, may be {@code null}
    * @param stringIfNull the String to return if the array is {@code null}
    * @return a String representation of the array
    */
  def toString(array: AnyRef, stringIfNull: String): String = {
    if (array == null) return stringIfNull

    new ToStringBuilder(array, ToStringStyle.SIMPLE_STYLE).append(array).toString
  }
  //
  //  /**
  //    * <p>Returns an array containing the string representation of each element in the argument array.</p>
  //    *
  //    * <p>This method returns {@code null} for a {@code null} input array.</p>
  //    *
  //    * @param array the {@code Object[]} to be processed, may be null
  //    * @return {@code String[]} of the same size as the source with its element's string representation,
  //    *         {@code null} if null array input
  //    * @throws NullPointerException if array contains {@code null}
  //    * @since 3.6
  //    */
  //  def toStringArray (array: Array[AnyRef] ): Array[String] = {
  //    if (array == null) return null
  //    else if (array.length == 0) return EMPTY_STRING_ARRAY
  //
  //    val result: Array[String] = new Array[String](array.length)
  //    for (i <- 0 until array.length) {
  //      result (i) = array(i).toString
  //    }
  //
  //    result
  //  }
  //
  //  /**
  //    * <p>Returns an array containing the string representation of each element in the argument
  //    * array handling {@code null} elements.</p>
  //    *
  //    * <p>This method returns {@code null} for a {@code null} input array.</p>
  //    *
  //    * @param array                the Object[] to be processed, may be null
  //    * @param valueForNullElements the value to insert if {@code null} is found
  //    * @return a {@code String} array, {@code null} if null array input
  //    * @since 3.6
  //    */
  //  def toStringArray (array: Array[AnyRef], valueForNullElements: String): Array[String] = {
  //    if (null == array) return null
  //    else if (array.length == 0) return EMPTY_STRING_ARRAY
  //    val result: Array[String] = new Array[String] (array.length)
  //    for (i <- 0 until array.length) {
  //      val `object`: Any = array(i)
  //      result (i) =
  //        if (`object` == null) valueForNullElements
  //        else `object`.toString
  //    }
  //
  //    result
  //  }
}