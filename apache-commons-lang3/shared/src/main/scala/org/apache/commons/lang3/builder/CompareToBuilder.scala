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

import java.lang.reflect.Modifier
import java.lang.{Byte => JavaByte, Double => JavaDouble, Float => JavaFloat, Long => JavaLong, Short => JavaShort}
import java.util
import java.util.Comparator
import java.util.Objects
import org.apache.commons.lang3.ArrayUtils

/**
  * Assists in implementing {@link java.lang.Comparable# compareTo ( Object )} methods.
  *
  * <p>It is consistent with {@code equals(Object)} and
  * {@code hashcode()} built with {@link EqualsBuilder} and
  * {@link HashCodeBuilder}.</p>
  *
  * <p>Two Objects that compare equal using {@code equals(Object)} should normally
  * also compare equal using {@code compareTo(Object)}.</p>
  *
  * <p>All relevant fields should be included in the calculation of the
  * comparison. Derived fields may be ignored. The same fields, in the same
  * order, should be used in both {@code compareTo(Object)} and
  * {@code equals(Object)}.</p>
  *
  * <p>To use this class write code as follows:</p>
  *
  * <pre>
  * public class MyClass {
  * String field1;
  * int field2;
  * boolean field3;
  *
  * ...
  *
  * public int compareTo(Object o) {
  * MyClass myClass = (MyClass) o;
  * return new CompareToBuilder()
  * .appendSuper(super.compareTo(o)
  * .append(this.field1, myClass.field1)
  * .append(this.field2, myClass.field2)
  * .append(this.field3, myClass.field3)
  * .toComparison();
  * }
  * }
  * </pre>
  *
  * <p>Values are compared in the order they are appended to the builder. If any comparison returns
  * a non-zero result, then that value will be the result returned by {@code toComparison()} and all
  * subsequent comparisons are skipped.</p>
  *
  * <p>Alternatively, there are {@link #reflectionCompare ( Object, Object) reflectionCompare} methods that use
  * reflection to determine the fields to append. Because fields can be private,
  * {@code reflectionCompare} uses {@link java.lang.reflect.AccessibleObject# setAccessible ( boolean )} to
  * bypass normal access control checks. This will fail under a security manager,
  * unless the appropriate permissions are set up correctly. It is also
  * slower than appending explicitly.</p>
  *
  * <p>A typical implementation of {@code compareTo(Object)} using
  * {@code reflectionCompare} looks like:</p>
  *
  * <pre>
  * public int compareTo(Object o) {
  * return CompareToBuilder.reflectionCompare(this, o);
  * }
  * </pre>
  *
  * <p>The reflective methods compare object fields in the order returned by
  * {@link Class# getDeclaredFields ( )}. The fields of the class are compared first, followed by those
  * of its parent classes (in order from the bottom to the top of the class hierarchy).</p>
  *
  * @see java.lang.Comparable
  * @see java.lang.Object#equals(Object)
  * @see java.lang.Object#hashCode()
  * @see EqualsBuilder
  * @see HashCodeBuilder
  * @since 1.0
  */
object CompareToBuilder {
  /**
    * <p>Compares two {@code Object}s via reflection.</p>
    *
    * <p>Fields can be private, thus {@code AccessibleObject.setAccessible}
    * is used to bypass normal access control checks. This will fail under a
    * security manager unless the appropriate permissions are set.</p>
    *
    * <ul>
    * <li>Static fields will not be compared</li>
    * <li>Transient members will be not be compared, as they are likely derived
    * fields</li>
    * <li>Superclass fields will be compared</li>
    * </ul>
    *
    * <p>If both {@code lhs} and {@code rhs} are {@code null},
    * they are considered equal.</p>
    *
    * @param lhs left-hand object
    * @param rhs right-hand object
    * @return a negative integer, zero, or a positive integer as {@code lhs}
    *         is less than, equal to, or greater than {@code rhs}
    * @throws NullPointerException if either (but not both) parameters are
    *                              {@code null}
    * @throws ClassCastException   if {@code rhs} is not assignment-compatible
    *                              with {@code lhs}
    */
  def reflectionCompare(lhs: Any, rhs: Any): Int = reflectionCompare(lhs, rhs, false, null)

  /**
    * <p>Compares two {@code Object}s via reflection.</p>
    *
    * <p>Fields can be private, thus {@code AccessibleObject.setAccessible}
    * is used to bypass normal access control checks. This will fail under a
    * security manager unless the appropriate permissions are set.</p>
    *
    * <ul>
    * <li>Static fields will not be compared</li>
    * <li>If {@code compareTransients} is {@code true},
    * compares transient members.  Otherwise ignores them, as they
    * are likely derived fields.</li>
    * <li>Superclass fields will be compared</li>
    * </ul>
    *
    * <p>If both {@code lhs} and {@code rhs} are {@code null},
    * they are considered equal.</p>
    *
    * @param lhs               left-hand object
    * @param rhs               right-hand object
    * @param compareTransients whether to compare transient fields
    * @return a negative integer, zero, or a positive integer as {@code lhs}
    *         is less than, equal to, or greater than {@code rhs}
    * @throws NullPointerException if either {@code lhs} or {@code rhs}
    *                              (but not both) is {@code null}
    * @throws ClassCastException   if {@code rhs} is not assignment-compatible
    *                              with {@code lhs}
    */
  def reflectionCompare(lhs: Any, rhs: Any, compareTransients: Boolean): Int = reflectionCompare(lhs, rhs, compareTransients, null)

  /**
    * <p>Compares two {@code Object}s via reflection.</p>
    *
    * <p>Fields can be private, thus {@code AccessibleObject.setAccessible}
    * is used to bypass normal access control checks. This will fail under a
    * security manager unless the appropriate permissions are set.</p>
    *
    * <ul>
    * <li>Static fields will not be compared</li>
    * <li>If {@code compareTransients} is {@code true},
    * compares transient members.  Otherwise ignores them, as they
    * are likely derived fields.</li>
    * <li>Superclass fields will be compared</li>
    * </ul>
    *
    * <p>If both {@code lhs} and {@code rhs} are {@code null},
    * they are considered equal.</p>
    *
    * @param lhs           left-hand object
    * @param rhs           right-hand object
    * @param excludeFields Collection of String fields to exclude
    * @return a negative integer, zero, or a positive integer as {@code lhs}
    *         is less than, equal to, or greater than {@code rhs}
    * @throws NullPointerException if either {@code lhs} or {@code rhs}
    *                              (but not both) is {@code null}
    * @throws ClassCastException   if {@code rhs} is not assignment-compatible
    *                              with {@code lhs}
    * @since 2.2
    */
  def reflectionCompare(lhs: Any, rhs: Any, excludeFields: util.Collection[String]): Int =
    reflectionCompare(lhs, rhs, ReflectionToStringBuilder.toNoNullStringArray(excludeFields):_*)

  /**
    * <p>Compares two {@code Object}s via reflection.</p>
    *
    * <p>Fields can be private, thus {@code AccessibleObject.setAccessible}
    * is used to bypass normal access control checks. This will fail under a
    * security manager unless the appropriate permissions are set.</p>
    *
    * <ul>
    * <li>Static fields will not be compared</li>
    * <li>If {@code compareTransients} is {@code true},
    * compares transient members.  Otherwise ignores them, as they
    * are likely derived fields.</li>
    * <li>Superclass fields will be compared</li>
    * </ul>
    *
    * <p>If both {@code lhs} and {@code rhs} are {@code null},
    * they are considered equal.</p>
    *
    * @param lhs           left-hand object
    * @param rhs           right-hand object
    * @param excludeFields array of fields to exclude
    * @return a negative integer, zero, or a positive integer as {@code lhs}
    *         is less than, equal to, or greater than {@code rhs}
    * @throws NullPointerException if either {@code lhs} or {@code rhs}
    *                              (but not both) is {@code null}
    * @throws ClassCastException   if {@code rhs} is not assignment-compatible
    *                              with {@code lhs}
    * @since 2.2
    */
  def reflectionCompare(lhs: Any, rhs: Any, excludeFields: String*): Int =
    reflectionCompare(lhs, rhs, false, null, excludeFields:_*)

  /**
    * <p>Compares two {@code Object}s via reflection.</p>
    *
    * <p>Fields can be private, thus {@code AccessibleObject.setAccessible}
    * is used to bypass normal access control checks. This will fail under a
    * security manager unless the appropriate permissions are set.</p>
    *
    * <ul>
    * <li>Static fields will not be compared</li>
    * <li>If the {@code compareTransients} is {@code true},
    * compares transient members.  Otherwise ignores them, as they
    * are likely derived fields.</li>
    * <li>Compares superclass fields up to and including {@code reflectUpToClass}.
    * If {@code reflectUpToClass} is {@code null}, compares all superclass fields.</li>
    * </ul>
    *
    * <p>If both {@code lhs} and {@code rhs} are {@code null},
    * they are considered equal.</p>
    *
    * @param lhs               left-hand object
    * @param rhs               right-hand object
    * @param compareTransients whether to compare transient fields
    * @param reflectUpToClass  last superclass for which fields are compared
    * @param excludeFields     fields to exclude
    * @return a negative integer, zero, or a positive integer as {@code lhs}
    *         is less than, equal to, or greater than {@code rhs}
    * @throws NullPointerException if either {@code lhs} or {@code rhs}
    *                              (but not both) is {@code null}
    * @throws ClassCastException   if {@code rhs} is not assignment-compatible
    *                              with {@code lhs}
    * @since 2.2 (2.0 as {@code reflectionCompare(Object, Object, boolean, Class)})
    */
  def reflectionCompare(lhs: Any, rhs: Any, compareTransients: Boolean, reflectUpToClass: Class[_], excludeFields: String*): Int = {
    //if (lhs eq rhs) return 0
    assert(false,"unimplemented")
    Objects.requireNonNull(lhs, "lhs")
    Objects.requireNonNull(rhs, "rhs")
    var lhsClazz = lhs.getClass
    if (!lhsClazz.isInstance(rhs)) throw new ClassCastException
    val compareToBuilder = new CompareToBuilder
    reflectionAppend(lhs, rhs, lhsClazz, compareToBuilder, compareTransients, excludeFields.toArray)

    while(lhsClazz.getSuperclass != null && (lhsClazz ne reflectUpToClass)) {
      lhsClazz = lhsClazz.getSuperclass
      reflectionAppend(lhs, rhs, lhsClazz, compareToBuilder, compareTransients, excludeFields.toArray)
    }
    compareToBuilder.toComparison
  }

  /**
    * <p>Appends to {@code builder} the comparison of {@code lhs}
    * to {@code rhs} using the fields defined in {@code clazz}.</p>
    *
    * @param lhs           left-hand object
    * @param rhs           right-hand object
    * @param clazz         {@code Class} that defines fields to be compared
    * @param builder       {@code CompareToBuilder} to append to
    * @param useTransients whether to compare transient fields
    * @param excludeFields fields to exclude
    */
  private def reflectionAppend(lhs: Any, rhs: Any, clazz: Class[_], builder: CompareToBuilder, useTransients: Boolean, excludeFields: Array[String]): Unit = {
    val fields = clazz.getDeclaredFields
    fields.foreach { _.setAccessible(true) }

    var i = 0
    while ( {
      i < fields.length && builder.comparison == 0
    }) {
      val f = fields(i)
      if (!ArrayUtils.contains(excludeFields, f.getName) &&
          !f.getName.contains("$") &&
          (useTransients || !Modifier.isTransient(f.getModifiers)) &&
          !Modifier.isStatic(f.getModifiers)
      ) try {
        builder.append(f.get(lhs), f.get(rhs))
      } catch {
        case _: IllegalAccessException =>
          // This can't happen. Would get a Security exception instead.
          // Throw a runtime exception in case the impossible happens.
          throw new InternalError("Unexpected IllegalAccessException")
      }

      i += 1
    }
  }
}

class CompareToBuilder()

/**
  * <p>Constructor for CompareToBuilder.</p>
  *
  * <p>Starts off assuming that the objects are equal. Multiple calls are
  * then made to the various append methods, followed by a call to
  * {@link #toComparison} to get the result.</p>
  */
  extends Builder[Integer] {
  /**
    * Current state of the comparison as appended fields are checked.
    */
  private var comparison: Int = 0

  /**
    * <p>Appends to the {@code builder} the {@code compareTo(Object)}
    * result of the superclass.</p>
    *
    * @param superCompareTo result of calling {@code super.compareTo(Object)}
    * @return this - used to chain append calls
    * @since 2.0
    */
  def appendSuper(superCompareTo: Int): CompareToBuilder = {
    if (comparison != 0) return this
    comparison = superCompareTo
    this
  }

  /**
    * <p>Appends to the {@code builder} the comparison of
    * two {@code Object}s.</p>
    *
    * <ol>
    * <li>Check if {@code lhs == rhs}</li>
    * <li>Check if either {@code lhs} or {@code rhs} is {@code null},
    * a {@code null} object is less than a non-{@code null} object</li>
    * <li>Check the object contents</li>
    * </ol>
    *
    * <p>{@code lhs} must either be an array or implement {@link Comparable}.</p>
    *
    * @param lhs left-hand object
    * @param rhs right-hand object
    * @return this - used to chain append calls
    * @throws ClassCastException if {@code rhs} is not assignment-compatible
    *                            with {@code lhs}
    */
  def append(lhs: Any, rhs: Any): CompareToBuilder =
    append(lhs, rhs, null)

  /**
    * <p>Appends to the {@code builder} the comparison of
    * two {@code Object}s.</p>
    *
    * <ol>
    * <li>Check if {@code lhs == rhs}</li>
    * <li>Check if either {@code lhs} or {@code rhs} is {@code null},
    * a {@code null} object is less than a non-{@code null} object</li>
    * <li>Check the object contents</li>
    * </ol>
    *
    * <p>If {@code lhs} is an array, array comparison methods will be used.
    * Otherwise {@code comparator} will be used to compare the objects.
    * If {@code comparator} is {@code null}, {@code lhs} must
    * implement {@link Comparable} instead.</p>
    *
    * @param lhs        left-hand object
    * @param rhs        right-hand object
    * @param comparator {@code Comparator} used to compare the objects,
    *                   {@code null} means treat lhs as {@code Comparable}
    * @return this - used to chain append calls
    * @throws ClassCastException if {@code rhs} is not assignment-compatible
    *                            with {@code lhs}
    * @since 2.0
    */
  def append(lhs: Any, rhs: Any, comparator: Comparator[_]): CompareToBuilder = {
    if (comparison != 0) return this

    (lhs, rhs) match {
      case (null, _) => comparison = -1; return this
      case (_, null) => comparison = 1; return this
      case (l: AnyRef, _) if l.getClass.isArray => appendArray(lhs, rhs, comparator) // factor out array case in order to keep method small enough to be inlined
      case (l: AnyRef, r: AnyRef) =>
        if (l eq r) return this

        if (comparator == null) {
          @SuppressWarnings(Array("unchecked")) // assume this can be done; if not throw CCE as per Javadoc
          val comparable: Comparable[AnyRef] = lhs.asInstanceOf[Comparable[AnyRef]]
          comparison = comparable.compareTo(r)
        }
        else {
          @SuppressWarnings(Array("unchecked")) val comparator2 = comparator.asInstanceOf[Comparator[AnyRef]]
          comparison = comparator2.compare(l, r)
        }
    }

    this
  }

  private def appendArray(lhs: Any, rhs: Any, comparator: Comparator[_]): Unit = { // switch on type of array, to dispatch to the correct handler
    // handles multi dimensional arrays
    // throws a ClassCastException if rhs is not the correct array type
    if (lhs.isInstanceOf[Array[Long]]) append(lhs.asInstanceOf[Array[Long]], rhs.asInstanceOf[Array[Long]])
    else if (lhs.isInstanceOf[Array[Int]]) append(lhs.asInstanceOf[Array[Int]], rhs.asInstanceOf[Array[Int]])
    else if (lhs.isInstanceOf[Array[Short]]) append(lhs.asInstanceOf[Array[Short]], rhs.asInstanceOf[Array[Short]])
    else if (lhs.isInstanceOf[Array[Char]]) append(lhs.asInstanceOf[Array[Char]], rhs.asInstanceOf[Array[Char]])
    else if (lhs.isInstanceOf[Array[Byte]]) append(lhs.asInstanceOf[Array[Byte]], rhs.asInstanceOf[Array[Byte]])
    else if (lhs.isInstanceOf[Array[Double]]) append(lhs.asInstanceOf[Array[Double]], rhs.asInstanceOf[Array[Double]])
    else if (lhs.isInstanceOf[Array[Float]]) append(lhs.asInstanceOf[Array[Float]], rhs.asInstanceOf[Array[Float]])
    else if (lhs.isInstanceOf[Array[Boolean]]) append(lhs.asInstanceOf[Array[Boolean]], rhs.asInstanceOf[Array[Boolean]])
    else { // not an array of primitives
      // throws a ClassCastException if rhs is not an array
      append(lhs.asInstanceOf[Array[AnyRef]], rhs.asInstanceOf[Array[AnyRef]], comparator)
    }
    ()
  }

  /**
    * Appends to the {@code builder} the comparison of
    * two {@code long}s.
    *
    * @param lhs left-hand value
    * @param rhs right-hand value
    * @return this - used to chain append calls
    */
  def append(lhs: Long, rhs: Long): CompareToBuilder = {
    if (comparison != 0) return this
    comparison = JavaLong.compare(lhs, rhs)
    this
  }

  /**
    * Appends to the {@code builder} the comparison of
    * two {@code int}s.
    *
    * @param lhs left-hand value
    * @param rhs right-hand value
    * @return this - used to chain append calls
    */
  def append(lhs: Int, rhs: Int): CompareToBuilder = {
    if (comparison != 0) return this
    comparison = Integer.compare(lhs, rhs)
    this
  }

  /**
    * Appends to the {@code builder} the comparison of
    * two {@code short}s.
    *
    * @param lhs left-hand value
    * @param rhs right-hand value
    * @return this - used to chain append calls
    */
  def append(lhs: Short, rhs: Short): CompareToBuilder = {
    if (comparison != 0) return this
    comparison = JavaShort.compare(lhs, rhs)
    this
  }

  /**
    * Appends to the {@code builder} the comparison of
    * two {@code char}s.
    *
    * @param lhs left-hand value
    * @param rhs right-hand value
    * @return this - used to chain append calls
    */
  def append(lhs: Char, rhs: Char): CompareToBuilder = {
    if (comparison != 0) return this
    comparison = Character.compare(lhs, rhs)
    this
  }

  /**
    * Appends to the {@code builder} the comparison of
    * two {@code byte}s.
    *
    * @param lhs left-hand value
    * @param rhs right-hand value
    * @return this - used to chain append calls
    */
  def append(lhs: Byte, rhs: Byte): CompareToBuilder = {
    if (comparison != 0) return this
    comparison = JavaByte.compare(lhs, rhs)
    this
  }

  /**
    * <p>Appends to the {@code builder} the comparison of
    * two {@code double}s.</p>
    *
    * <p>This handles NaNs, Infinities, and {@code -0.0}.</p>
    *
    * <p>It is compatible with the hash code generated by
    * {@code HashCodeBuilder}.</p>
    *
    * @param lhs left-hand value
    * @param rhs right-hand value
    * @return this - used to chain append calls
    */
  def append(lhs: Double, rhs: Double): CompareToBuilder = {
    if (comparison != 0) return this
    comparison = JavaDouble.compare(lhs, rhs)
    this
  }

  /**
    * <p>Appends to the {@code builder} the comparison of
    * two {@code float}s.</p>
    *
    * <p>This handles NaNs, Infinities, and {@code -0.0}.</p>
    *
    * <p>It is compatible with the hash code generated by
    * {@code HashCodeBuilder}.</p>
    *
    * @param lhs left-hand value
    * @param rhs right-hand value
    * @return this - used to chain append calls
    */
  def append(lhs: Float, rhs: Float): CompareToBuilder = {
    if (comparison != 0) return this
    comparison = JavaFloat.compare(lhs, rhs)
    this
  }

  /**
    * Appends to the {@code builder} the comparison of
    * two {@code booleans}s.
    *
    * @param lhs left-hand value
    * @param rhs right-hand value
    * @return this - used to chain append calls
    */
  def append(lhs: Boolean, rhs: Boolean): CompareToBuilder = {
    if (comparison != 0) return this
    if (lhs == rhs) return this
    if (lhs) comparison = 1
    else comparison = -1
    this
  }

  /**
    * <p>Appends to the {@code builder} the deep comparison of
    * two {@code Object} arrays.</p>
    *
    * <ol>
    * <li>Check if arrays are the same using {@code ==}</li>
    * <li>Check if for {@code null}, {@code null} is less than non-{@code null}</li>
    * <li>Check array length, a short length array is less than a long length array</li>
    * <li>Check array contents element by element using {@link #append ( Object, Object, Comparator)}</li>
    * </ol>
    *
    * <p>This method will also will be called for the top level of multi-dimensional,
    * ragged, and multi-typed arrays.</p>
    *
    * @param lhs left-hand array
    * @param rhs right-hand array
    * @return this - used to chain append calls
    * @throws ClassCastException if {@code rhs} is not assignment-compatible
    *                            with {@code lhs}
    */
  def append(lhs: Array[AnyRef], rhs: Array[AnyRef]): CompareToBuilder = append(lhs, rhs, null)

  /**
    * <p>Appends to the {@code builder} the deep comparison of
    * two {@code Object} arrays.</p>
    *
    * <ol>
    * <li>Check if arrays are the same using {@code ==}</li>
    * <li>Check if for {@code null}, {@code null} is less than non-{@code null}</li>
    * <li>Check array length, a short length array is less than a long length array</li>
    * <li>Check array contents element by element using {@link #append ( Object, Object, Comparator)}</li>
    * </ol>
    *
    * <p>This method will also will be called for the top level of multi-dimensional,
    * ragged, and multi-typed arrays.</p>
    *
    * @param lhs        left-hand array
    * @param rhs        right-hand array
    * @param comparator {@code Comparator} to use to compare the array elements,
    *                   {@code null} means to treat {@code lhs} elements as {@code Comparable}.
    * @return this - used to chain append calls
    * @throws ClassCastException if {@code rhs} is not assignment-compatible
    *                            with {@code lhs}
    * @since 2.0
    */
  def append(lhs: Array[AnyRef], rhs: Array[AnyRef], comparator: Comparator[_]): CompareToBuilder = {
    if (comparison != 0) return this
    if (lhs eq rhs) return this
    if (lhs == null) {
      comparison = -1
      return this
    }
    if (rhs == null) {
      comparison = 1
      return this
    }
    if (lhs.length != rhs.length) {
      comparison = if (lhs.length < rhs.length) -1
      else 1
      return this
    }
    var i = 0
    while ( {
      i < lhs.length && comparison == 0
    }) {
      append(lhs(i), rhs(i), comparator)

      i += 1
    }
    this
  }

  /**
    * <p>Appends to the {@code builder} the deep comparison of
    * two {@code long} arrays.</p>
    *
    * <ol>
    * <li>Check if arrays are the same using {@code ==}</li>
    * <li>Check if for {@code null}, {@code null} is less than non-{@code null}</li>
    * <li>Check array length, a shorter length array is less than a longer length array</li>
    * <li>Check array contents element by element using {@link #append ( long, long)}</li>
    * </ol>
    *
    * @param lhs left-hand array
    * @param rhs right-hand array
    * @return this - used to chain append calls
    */
  def append(lhs: Array[Long], rhs: Array[Long]): CompareToBuilder = {
    if (comparison != 0) return this
    if (lhs eq rhs) return this
    if (lhs == null) {
      comparison = -1
      return this
    }
    if (rhs == null) {
      comparison = 1
      return this
    }
    if (lhs.length != rhs.length) {
      comparison = if (lhs.length < rhs.length) -1
      else 1
      return this
    }
    var i = 0
    while ( {
      i < lhs.length && comparison == 0
    }) {
      append(lhs(i), rhs(i))

      i += 1
    }
    this
  }

  /**
    * <p>Appends to the {@code builder} the deep comparison of
    * two {@code int} arrays.</p>
    *
    * <ol>
    * <li>Check if arrays are the same using {@code ==}</li>
    * <li>Check if for {@code null}, {@code null} is less than non-{@code null}</li>
    * <li>Check array length, a shorter length array is less than a longer length array</li>
    * <li>Check array contents element by element using {@link #append ( int, int)}</li>
    * </ol>
    *
    * @param lhs left-hand array
    * @param rhs right-hand array
    * @return this - used to chain append calls
    */
  def append(lhs: Array[Int], rhs: Array[Int]): CompareToBuilder = {
    if (comparison != 0) return this
    if (lhs eq rhs) return this
    if (lhs == null) {
      comparison = -1
      return this
    }
    if (rhs == null) {
      comparison = 1
      return this
    }
    if (lhs.length != rhs.length) {
      comparison = if (lhs.length < rhs.length) -1
      else 1
      return this
    }
    var i = 0
    while ( {
      i < lhs.length && comparison == 0
    }) {
      append(lhs(i), rhs(i))

      i += 1
    }
    this
  }

  /**
    * <p>Appends to the {@code builder} the deep comparison of
    * two {@code short} arrays.</p>
    *
    * <ol>
    * <li>Check if arrays are the same using {@code ==}</li>
    * <li>Check if for {@code null}, {@code null} is less than non-{@code null}</li>
    * <li>Check array length, a shorter length array is less than a longer length array</li>
    * <li>Check array contents element by element using {@link #append ( short, short)}</li>
    * </ol>
    *
    * @param lhs left-hand array
    * @param rhs right-hand array
    * @return this - used to chain append calls
    */
  def append(lhs: Array[Short], rhs: Array[Short]): CompareToBuilder = {
    if (comparison != 0) return this
    if (lhs eq rhs) return this
    if (lhs == null) {
      comparison = -1
      return this
    }
    if (rhs == null) {
      comparison = 1
      return this
    }
    if (lhs.length != rhs.length) {
      comparison = if (lhs.length < rhs.length) -1
      else 1
      return this
    }
    var i = 0
    while ( {
      i < lhs.length && comparison == 0
    }) {
      append(lhs(i), rhs(i))

      i += 1
    }
    this
  }

  /**
    * <p>Appends to the {@code builder} the deep comparison of
    * two {@code char} arrays.</p>
    *
    * <ol>
    * <li>Check if arrays are the same using {@code ==}</li>
    * <li>Check if for {@code null}, {@code null} is less than non-{@code null}</li>
    * <li>Check array length, a shorter length array is less than a longer length array</li>
    * <li>Check array contents element by element using {@link #append ( char, char)}</li>
    * </ol>
    *
    * @param lhs left-hand array
    * @param rhs right-hand array
    * @return this - used to chain append calls
    */
  def append(lhs: Array[Char], rhs: Array[Char]): CompareToBuilder = {
    if (comparison != 0) return this
    if (lhs eq rhs) return this
    if (lhs == null) {
      comparison = -1
      return this
    }
    if (rhs == null) {
      comparison = 1
      return this
    }
    if (lhs.length != rhs.length) {
      comparison = if (lhs.length < rhs.length) -1
      else 1
      return this
    }
    var i = 0
    while ( {
      i < lhs.length && comparison == 0
    }) {
      append(lhs(i), rhs(i))

      i += 1
    }
    this
  }

  /**
    * <p>Appends to the {@code builder} the deep comparison of
    * two {@code byte} arrays.</p>
    *
    * <ol>
    * <li>Check if arrays are the same using {@code ==}</li>
    * <li>Check if for {@code null}, {@code null} is less than non-{@code null}</li>
    * <li>Check array length, a shorter length array is less than a longer length array</li>
    * <li>Check array contents element by element using {@link #append ( byte, byte)}</li>
    * </ol>
    *
    * @param lhs left-hand array
    * @param rhs right-hand array
    * @return this - used to chain append calls
    */
  def append(lhs: Array[Byte], rhs: Array[Byte]): CompareToBuilder = {
    if (comparison != 0) return this
    if (lhs eq rhs) return this
    if (lhs == null) {
      comparison = -1
      return this
    }
    if (rhs == null) {
      comparison = 1
      return this
    }
    if (lhs.length != rhs.length) {
      comparison = if (lhs.length < rhs.length) -1
      else 1
      return this
    }
    var i = 0
    while ( {
      i < lhs.length && comparison == 0
    }) {
      append(lhs(i), rhs(i))

      i += 1
    }
    this
  }

  /**
    * <p>Appends to the {@code builder} the deep comparison of
    * two {@code double} arrays.</p>
    *
    * <ol>
    * <li>Check if arrays are the same using {@code ==}</li>
    * <li>Check if for {@code null}, {@code null} is less than non-{@code null}</li>
    * <li>Check array length, a shorter length array is less than a longer length array</li>
    * <li>Check array contents element by element using {@link #append ( double, double)}</li>
    * </ol>
    *
    * @param lhs left-hand array
    * @param rhs right-hand array
    * @return this - used to chain append calls
    */
  def append(lhs: Array[Double], rhs: Array[Double]): CompareToBuilder = {
    if (comparison != 0) return this
    if (lhs eq rhs) return this
    if (lhs == null) {
      comparison = -1
      return this
    }
    if (rhs == null) {
      comparison = 1
      return this
    }
    if (lhs.length != rhs.length) {
      comparison = if (lhs.length < rhs.length) -1
      else 1
      return this
    }
    var i = 0
    while ( {
      i < lhs.length && comparison == 0
    }) {
      append(lhs(i), rhs(i))

      i += 1
    }
    this
  }

  /**
    * <p>Appends to the {@code builder} the deep comparison of
    * two {@code float} arrays.</p>
    *
    * <ol>
    * <li>Check if arrays are the same using {@code ==}</li>
    * <li>Check if for {@code null}, {@code null} is less than non-{@code null}</li>
    * <li>Check array length, a shorter length array is less than a longer length array</li>
    * <li>Check array contents element by element using {@link #append ( float, float)}</li>
    * </ol>
    *
    * @param lhs left-hand array
    * @param rhs right-hand array
    * @return this - used to chain append calls
    */
  def append(lhs: Array[Float], rhs: Array[Float]): CompareToBuilder = {
    if (comparison != 0) return this
    if (lhs eq rhs) return this
    if (lhs == null) {
      comparison = -1
      return this
    }
    if (rhs == null) {
      comparison = 1
      return this
    }
    if (lhs.length != rhs.length) {
      comparison = if (lhs.length < rhs.length) -1
      else 1
      return this
    }
    var i = 0
    while ( {
      i < lhs.length && comparison == 0
    }) {
      append(lhs(i), rhs(i))

      i += 1
    }
    this
  }

  /**
    * <p>Appends to the {@code builder} the deep comparison of
    * two {@code boolean} arrays.</p>
    *
    * <ol>
    * <li>Check if arrays are the same using {@code ==}</li>
    * <li>Check if for {@code null}, {@code null} is less than non-{@code null}</li>
    * <li>Check array length, a shorter length array is less than a longer length array</li>
    * <li>Check array contents element by element using {@link #append ( boolean, boolean)}</li>
    * </ol>
    *
    * @param lhs left-hand array
    * @param rhs right-hand array
    * @return this - used to chain append calls
    */
  def append(lhs: Array[Boolean], rhs: Array[Boolean]): CompareToBuilder = {
    if (comparison != 0) return this
    if (lhs eq rhs) return this
    if (lhs == null) {
      comparison = -1
      return this
    }
    if (rhs == null) {
      comparison = 1
      return this
    }
    if (lhs.length != rhs.length) {
      comparison = if (lhs.length < rhs.length) -1
      else 1
      return this
    }
    var i = 0
    while ( {
      i < lhs.length && comparison == 0
    }) {
      append(lhs(i), rhs(i))

      i += 1
    }
    this
  }

  /**
    * Returns a negative integer, a positive integer, or zero as
    * the {@code builder} has judged the "left-hand" side
    * as less than, greater than, or equal to the "right-hand"
    * side.
    *
    * @return final comparison result
    * @see #build()
    */
  def toComparison: Int = comparison

  /**
    * Returns a negative Integer, a positive Integer, or zero as
    * the {@code builder} has judged the "left-hand" side
    * as less than, greater than, or equal to the "right-hand"
    * side.
    *
    * @return final comparison result as an Integer
    * @see #toComparison()
    * @since 3.0
    */
  override def build: Integer = Integer.valueOf(toComparison)
}
