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

import java.lang.{Byte => JavaByte, Double => JavaDouble, Float => JavaFloat, Short => JavaShort}
import java.lang.reflect.AccessibleObject
import java.lang.reflect.Field
import java.lang.reflect.Modifier
import java.util
import java.util.Comparator
import org.apache.commons.lang3.ArrayUtils
import org.apache.commons.lang3.Validate

/**
  * <p>
  * Assists in implementing {@link Object# hashCode ( )} methods.
  * </p>
  *
  * <p>
  * This class enables a good {@code hashCode} method to be built for any class. It follows the rules laid out in
  * the book <a href="http://www.oracle.com/technetwork/java/effectivejava-136174.html">Effective Java</a> by Joshua Bloch. Writing a
  * good {@code hashCode} method is actually quite difficult. This class aims to simplify the process.
  * </p>
  *
  * <p>
  * The following is the approach taken. When appending a data field, the current total is multiplied by the
  * multiplier then a relevant value
  * for that data type is added. For example, if the current hashCode is 17, and the multiplier is 37, then
  * appending the integer 45 will create a hash code of 674, namely 17 * 37 + 45.
  * </p>
  *
  * <p>
  * All relevant fields from the object should be included in the {@code hashCode} method. Derived fields may be
  * excluded. In general, any field used in the {@code equals} method must be used in the {@code hashCode}
  * method.
  * </p>
  *
  * <p>
  * To use this class write code as follows:
  * </p>
  *
  * <pre>
  * public class Person {
  * String name;
  * int age;
  * boolean smoker;
  * ...
  *
  * public int hashCode() {
  * // you pick a hard-coded, randomly chosen, non-zero, odd number
  * // ideally different for each class
  * return new HashCodeBuilder(17, 37).
  * append(name).
  * append(age).
  * append(smoker).
  * toHashCode();
  * }
  * }
  * </pre>
  *
  * <p>
  * If required, the superclass {@code hashCode()} can be added using {@link #appendSuper}.
  * </p>
  *
  * <p>
  * Alternatively, there is a method that uses reflection to determine the fields to test. Because these fields are
  * usually private, the method, {@code reflectionHashCode}, uses {@code AccessibleObject.setAccessible}
  * to change the visibility of the fields. This will fail under a security manager, unless the appropriate permissions
  * are set up correctly. It is also slower than testing explicitly.
  * </p>
  *
  * <p>
  * A typical invocation for this method would look like:
  * </p>
  *
  * <pre>
  * public int hashCode() {
  * return HashCodeBuilder.reflectionHashCode(this);
  * }
  * </pre>
  *
  * <p>The {@link HashCodeExclude} annotation can be used to exclude fields from being
  * used by the {@code reflectionHashCode} methods.</p>
  *
  * @since 1.0
  */
object HashCodeBuilder {
  /**
    * The default initial value to use in reflection hash code building.
    */
  private val DEFAULT_INITIAL_VALUE = 17
  /**
    * The default multiplier value to use in reflection hash code building.
    */
  private val DEFAULT_MULTIPLIER_VALUE = 37
  /**
    * <p>
    * A registry of objects used by reflection methods to detect cyclical object references and avoid infinite loops.
    * </p>
    *
    * @since 2.3
    */
  private val REGISTRY = new ThreadLocal[util.Set[IDKey]]

  /**
    * <p>
    * Returns the registry of objects being traversed by the reflection methods in the current thread.
    * </p>
    *
    * @return Set the registry of objects being traversed
    * @since 2.3
    */
  private[builder] def getRegistry = REGISTRY.get

  /**
    * <p>
    * Returns {@code true} if the registry contains the given object. Used by the reflection methods to avoid
    * infinite loops.
    * </p>
    *
    * @param value
    * The object to lookup in the registry.
    * @return boolean {@code true} if the registry contains the given object.
    * @since 2.3
    */
  private[builder] def isRegistered(value: Any) = {
    val registry = getRegistry
    registry != null && registry.contains(new IDKey(value))
  }

  /**
    * <p>
    * Appends the fields and values defined by the given object of the given {@code Class}.
    * </p>
    *
    * @param object
    * the object to append details of
    * @param clazz
    * the class to append details of
    * @param builder
    * the builder to append to
    * @param useTransients
    * whether to use transient fields
    * @param excludeFields
    * Collection of String field names to exclude from use in calculation of hash code
    */
  private def reflectionAppend(`object`: Any, clazz: Class[_], builder: HashCodeBuilder, useTransients: Boolean, excludeFields: Array[String]): Unit = {
    if (isRegistered(`object`)) return
    try {
      register(`object`)
      // The elements in the returned array are not sorted and are not in any particular order.
      val fields: Array[Field] = clazz.getDeclaredFields
      fields.foreach{ _.setAccessible(true) }

      for (field <- fields.sortBy(_.getName)) {
        if (
          !ArrayUtils.contains(excludeFields, field.getName) &&
          !field.getName.contains("$") &&
          (useTransients || !Modifier.isTransient(field.getModifiers)) &&
          !Modifier.isStatic(field.getModifiers) &&
          !field.isAnnotationPresent(classOf[HashCodeExclude])
        ) try {
          val fieldValue = field.get(`object`)
          builder.append(fieldValue)
        } catch {
          case e: IllegalAccessException =>
            // this can't happen. Would get a Security exception instead
            // throw a runtime exception in case the impossible happens.
            throw new InternalError("Unexpected IllegalAccessException")
        }
      }
    } finally unregister(`object`)
  }

  /**
    * <p>
    * Uses reflection to build a valid hash code from the fields of {@code object}.
    * </p>
    *
    * <p>
    * It uses {@code AccessibleObject.setAccessible} to gain access to private fields. This means that it will
    * throw a security exception if run under a security manager, if the permissions are not set up correctly. It is
    * also not as efficient as testing explicitly.
    * </p>
    *
    * <p>
    * Transient members will be not be used, as they are likely derived fields, and not part of the value of the
    * {@code Object}.
    * </p>
    *
    * <p>
    * Static fields will not be tested. Superclass fields will be included.
    * </p>
    *
    * <p>
    * Two randomly chosen, non-zero, odd numbers must be passed in. Ideally these should be different for each class,
    * however this is not vital. Prime numbers are preferred, especially for the multiplier.
    * </p>
    *
    * @param initialNonZeroOddNumber
    * a non-zero, odd number used as the initial value. This will be the returned
    * value if no fields are found to include in the hash code
    * @param multiplierNonZeroOddNumber
    * a non-zero, odd number used as the multiplier
    * @param object
    * the Object to create a {@code hashCode} for
    * @return int hash code
    * @throws IllegalArgumentException
    * if the Object is {@code null}
    * @throws IllegalArgumentException
    * if the number is zero or even
    * @see HashCodeExclude
    */
  def reflectionHashCode(initialNonZeroOddNumber: Int, multiplierNonZeroOddNumber: Int, `object`: Any): Int =
    reflectionHashCode(initialNonZeroOddNumber, multiplierNonZeroOddNumber, `object`, false, null)

  /**
    * <p>
    * Uses reflection to build a valid hash code from the fields of {@code object}.
    * </p>
    *
    * <p>
    * It uses {@code AccessibleObject.setAccessible} to gain access to private fields. This means that it will
    * throw a security exception if run under a security manager, if the permissions are not set up correctly. It is
    * also not as efficient as testing explicitly.
    * </p>
    *
    * <p>
    * If the TestTransients parameter is set to {@code true}, transient members will be tested, otherwise they
    * are ignored, as they are likely derived fields, and not part of the value of the {@code Object}.
    * </p>
    *
    * <p>
    * Static fields will not be tested. Superclass fields will be included.
    * </p>
    *
    * <p>
    * Two randomly chosen, non-zero, odd numbers must be passed in. Ideally these should be different for each class,
    * however this is not vital. Prime numbers are preferred, especially for the multiplier.
    * </p>
    *
    * @param initialNonZeroOddNumber
    * a non-zero, odd number used as the initial value. This will be the returned
    * value if no fields are found to include in the hash code
    * @param multiplierNonZeroOddNumber
    * a non-zero, odd number used as the multiplier
    * @param object
    * the Object to create a {@code hashCode} for
    * @param testTransients
    * whether to include transient fields
    * @return int hash code
    * @throws IllegalArgumentException
    * if the Object is {@code null}
    * @throws IllegalArgumentException
    * if the number is zero or even
    * @see HashCodeExclude
    */
  def reflectionHashCode(initialNonZeroOddNumber: Int, multiplierNonZeroOddNumber: Int, `object`: Any, testTransients: Boolean): Int =
    reflectionHashCode(initialNonZeroOddNumber, multiplierNonZeroOddNumber, `object`, testTransients, null)

  /**
    * <p>
    * Uses reflection to build a valid hash code from the fields of {@code object}.
    * </p>
    *
    * <p>
    * It uses {@code AccessibleObject.setAccessible} to gain access to private fields. This means that it will
    * throw a security exception if run under a security manager, if the permissions are not set up correctly. It is
    * also not as efficient as testing explicitly.
    * </p>
    *
    * <p>
    * If the TestTransients parameter is set to {@code true}, transient members will be tested, otherwise they
    * are ignored, as they are likely derived fields, and not part of the value of the {@code Object}.
    * </p>
    *
    * <p>
    * Static fields will not be included. Superclass fields will be included up to and including the specified
    * superclass. A null superclass is treated as java.lang.Object.
    * </p>
    *
    * <p>
    * Two randomly chosen, non-zero, odd numbers must be passed in. Ideally these should be different for each class,
    * however this is not vital. Prime numbers are preferred, especially for the multiplier.
    * </p>
    *
    * @param < T>
    *          the type of the object involved
    * @param initialNonZeroOddNumber
    *          a non-zero, odd number used as the initial value. This will be the returned
    *          value if no fields are found to include in the hash code
    * @param multiplierNonZeroOddNumber
    *          a non-zero, odd number used as the multiplier
    * @param object
    *          the Object to create a {@code hashCode} for
    * @param testTransients
    *          whether to include transient fields
    * @param reflectUpToClass
    *          the superclass to reflect up to (inclusive), may be {@code null}
    * @param excludeFields
    *          array of field names to exclude from use in calculation of hash code
    * @return int hash code
    * @throws IllegalArgumentException
    * if the Object is {@code null}
    * @throws IllegalArgumentException
    * if the number is zero or even
    * @see HashCodeExclude
    * @since 2.0
    */
  def reflectionHashCode[T](initialNonZeroOddNumber: Int, multiplierNonZeroOddNumber: Int, `object`: T, testTransients: Boolean, reflectUpToClass: Class[_ >: T], excludeFields: String*): Int = {
    Validate.notNull(`object`, "The object to build a hash code for must not be null")

    val builder = new HashCodeBuilder(initialNonZeroOddNumber, multiplierNonZeroOddNumber)
    var clazz: Class[_] = `object`.getClass
    reflectionAppend(`object`, clazz, builder, testTransients, excludeFields.toArray)
    while (clazz.getSuperclass != null && (clazz ne reflectUpToClass)) {
      clazz = clazz.getSuperclass
      reflectionAppend(`object`, clazz, builder, testTransients, excludeFields.toArray)
    }
    builder.toHashCode
  }

  /**
    * <p>
    * Uses reflection to build a valid hash code from the fields of {@code object}.
    * </p>
    *
    * <p>
    * This constructor uses two hard coded choices for the constants needed to build a hash code.
    * </p>
    *
    * <p>
    * It uses {@code AccessibleObject.setAccessible} to gain access to private fields. This means that it will
    * throw a security exception if run under a security manager, if the permissions are not set up correctly. It is
    * also not as efficient as testing explicitly.
    * </p>
    *
    * <P>
    * If the TestTransients parameter is set to {@code true}, transient members will be tested, otherwise they
    * are ignored, as they are likely derived fields, and not part of the value of the {@code Object}.
    * </p>
    *
    * <p>
    * Static fields will not be tested. Superclass fields will be included. If no fields are found to include
    * in the hash code, the result of this method will be constant.
    * </p>
    *
    * @param object
    * the Object to create a {@code hashCode} for
    * @param testTransients
    * whether to include transient fields
    * @return int hash code
    * @throws IllegalArgumentException
    * if the object is {@code null}
    * @see HashCodeExclude
    */
  def reflectionHashCode(`object`: Any, testTransients: Boolean): Int =
    reflectionHashCode(DEFAULT_INITIAL_VALUE, DEFAULT_MULTIPLIER_VALUE, `object`, testTransients, null)

  /**
    * <p>
    * Uses reflection to build a valid hash code from the fields of {@code object}.
    * </p>
    *
    * <p>
    * This constructor uses two hard coded choices for the constants needed to build a hash code.
    * </p>
    *
    * <p>
    * It uses {@code AccessibleObject.setAccessible} to gain access to private fields. This means that it will
    * throw a security exception if run under a security manager, if the permissions are not set up correctly. It is
    * also not as efficient as testing explicitly.
    * </p>
    *
    * <p>
    * Transient members will be not be used, as they are likely derived fields, and not part of the value of the
    * {@code Object}.
    * </p>
    *
    * <p>
    * Static fields will not be tested. Superclass fields will be included. If no fields are found to include
    * in the hash code, the result of this method will be constant.
    * </p>
    *
    * @param object
    * the Object to create a {@code hashCode} for
    * @param excludeFields
    * Collection of String field names to exclude from use in calculation of hash code
    * @return int hash code
    * @throws IllegalArgumentException
    * if the object is {@code null}
    * @see HashCodeExclude
    */
  def reflectionHashCode(`object`: Any, excludeFields: util.Collection[String]): Int =
    reflectionHashCode(`object`, ReflectionToStringBuilder.toNoNullStringArray(excludeFields):_*)

  /**
    * <p>
    * Uses reflection to build a valid hash code from the fields of {@code object}.
    * </p>
    *
    * <p>
    * This constructor uses two hard coded choices for the constants needed to build a hash code.
    * </p>
    *
    * <p>
    * It uses {@code AccessibleObject.setAccessible} to gain access to private fields. This means that it will
    * throw a security exception if run under a security manager, if the permissions are not set up correctly. It is
    * also not as efficient as testing explicitly.
    * </p>
    *
    * <p>
    * Transient members will be not be used, as they are likely derived fields, and not part of the value of the
    * {@code Object}.
    * </p>
    *
    * <p>
    * Static fields will not be tested. Superclass fields will be included. If no fields are found to include
    * in the hash code, the result of this method will be constant.
    * </p>
    *
    * @param object
    * the Object to create a {@code hashCode} for
    * @param excludeFields
    * array of field names to exclude from use in calculation of hash code
    * @return int hash code
    * @throws IllegalArgumentException
    * if the object is {@code null}
    * @see HashCodeExclude
    */
  def reflectionHashCode(`object`: Any, excludeFields: String*): Int =
    reflectionHashCode(DEFAULT_INITIAL_VALUE, DEFAULT_MULTIPLIER_VALUE, `object`, false, null, excludeFields:_*)

  /**
    * <p>
    * Registers the given object. Used by the reflection methods to avoid infinite loops.
    * </p>
    *
    * @param value
    * The object to register.
    */
  private def register(value: Any): Unit = {
    var registry = getRegistry
    if (registry == null) {
      registry = new util.HashSet[IDKey]
      REGISTRY.set(registry)
    }
    registry.add(new IDKey(value))
  }

  /**
    * <p>
    * Unregisters the given object.
    * </p>
    *
    * <p>
    * Used by the reflection methods to avoid infinite loops.
    *
    * @param value
    * The object to unregister.
    * @since 2.3
    */
  private def unregister(value: Any): Unit = {
    val registry = getRegistry
    if (registry != null) {
      registry.remove(new IDKey(value))
      if (registry.isEmpty) REGISTRY.remove()
    }
  }
}

class HashCodeBuilder()

/**
  * <p>
  * Uses two hard coded choices for the constants needed to build a {@code hashCode}.
  * </p>
  */
  extends Builder[Integer] {
  /**
    * Constant to use in building the hashCode.
    */
  final private var iConstant = 37
  /**
    * Running total of the hashCode.
    */
  private var iTotal = 17

  /**
    * <p>
    * Two randomly chosen, odd numbers must be passed in. Ideally these should be different for each class,
    * however this is not vital.
    * </p>
    *
    * <p>
    * Prime numbers are preferred, especially for the multiplier.
    * </p>
    *
    * @param initialOddNumber
    * an odd number used as the initial value
    * @param multiplierOddNumber
    * an odd number used as the multiplier
    * @throws IllegalArgumentException
    * if the number is even
    */
  def this(initialOddNumber: Int, multiplierOddNumber: Int) {
    this()
    Validate.isTrue(initialOddNumber % 2 != 0, "HashCodeBuilder requires an odd initial value")
    Validate.isTrue(multiplierOddNumber % 2 != 0, "HashCodeBuilder requires an odd multiplier")
    iConstant = multiplierOddNumber
    iTotal = initialOddNumber
  }

  /**
    * <p>
    * Append a {@code hashCode} for a {@code boolean}.
    * </p>
    * <p>
    * This adds {@code 1} when true, and {@code 0} when false to the {@code hashCode}.
    * </p>
    * <p>
    * This is in contrast to the standard {@code java.lang.Boolean.hashCode} handling, which computes
    * a {@code hashCode} value of {@code 1231} for {@code java.lang.Boolean} instances
    * that represent {@code true} or {@code 1237} for {@code java.lang.Boolean} instances
    * that represent {@code false}.
    * </p>
    * <p>
    * This is in accordance with the <i>Effective Java</i> design.
    * </p>
    *
    * @param value
    * the boolean to add to the {@code hashCode}
    * @return this
    */
  def append(value: Boolean): HashCodeBuilder = {
    iTotal = iTotal * iConstant + (if (value) 0 else 1)
    this
  }

  /**
    * <p>
    * Append a {@code hashCode} for a {@code boolean} array.
    * </p>
    *
    * @param array
    * the array to add to the {@code hashCode}
    * @return this
    */
  def append(array: Array[Boolean]): HashCodeBuilder = {
    if (array == null) iTotal = iTotal * iConstant
    else for (element <- array) {
      append(element)
    }
    this
  }

  /**
    * <p>
    * Append a {@code hashCode} for a {@code byte}.
    * </p>
    *
    * @param value
    * the byte to add to the {@code hashCode}
    * @return this
    */
  def append(value: Byte): HashCodeBuilder = {
    iTotal = iTotal * iConstant + value
    this
  }

  /**
    * <p>
    * Append a {@code hashCode} for a {@code byte} array.
    * </p>
    *
    * @param array
    * the array to add to the {@code hashCode}
    * @return this
    */
  def append(array: Array[Byte]): HashCodeBuilder = {
    if (array == null) iTotal = iTotal * iConstant
    else for (element <- array) {
      append(element)
    }
    this
  }

  /**
    * <p>
    * Append a {@code hashCode} for a {@code char}.
    * </p>
    *
    * @param value
    * the char to add to the {@code hashCode}
    * @return this
    */
  def append(value: Char): HashCodeBuilder = {
    iTotal = iTotal * iConstant + value
    this
  }

  /**
    * <p>
    * Append a {@code hashCode} for a {@code char} array.
    * </p>
    *
    * @param array
    * the array to add to the {@code hashCode}
    * @return this
    */
  def append(array: Array[Char]): HashCodeBuilder = {
    if (array == null) iTotal = iTotal * iConstant
    else for (element <- array) {
      append(element)
    }
    this
  }

  /**
    * <p>
    * Append a {@code hashCode} for a {@code double}.
    * </p>
    *
    * @param value
    * the double to add to the {@code hashCode}
    * @return this
    */
  def append(value: Double): HashCodeBuilder = append(JavaDouble.doubleToLongBits(value))

  /**
    * <p>
    * Append a {@code hashCode} for a {@code double} array.
    * </p>
    *
    * @param array
    * the array to add to the {@code hashCode}
    * @return this
    */
  def append(array: Array[Double]): HashCodeBuilder = {
    if (array == null) iTotal = iTotal * iConstant
    else for (element <- array) {
      append(element)
    }
    this
  }

  /**
    * <p>
    * Append a {@code hashCode} for a {@code float}.
    * </p>
    *
    * @param value
    * the float to add to the {@code hashCode}
    * @return this
    */
  def append(value: Float): HashCodeBuilder = {
    iTotal = iTotal * iConstant + JavaFloat.floatToIntBits(value)
    this
  }

  /**
    * <p>
    * Append a {@code hashCode} for a {@code float} array.
    * </p>
    *
    * @param array
    * the array to add to the {@code hashCode}
    * @return this
    */
  def append(array: Array[Float]): HashCodeBuilder = {
    if (array == null) iTotal = iTotal * iConstant
    else for (element <- array) {
      append(element)
    }
    this
  }

  /**
    * <p>
    * Append a {@code hashCode} for an {@code int}.
    * </p>
    *
    * @param value
    * the int to add to the {@code hashCode}
    * @return this
    */
  def append(value: Int): HashCodeBuilder = {
    iTotal = iTotal * iConstant + value
    this
  }

  /**
    * <p>
    * Append a {@code hashCode} for an {@code int} array.
    * </p>
    *
    * @param array
    * the array to add to the {@code hashCode}
    * @return this
    */
  def append(array: Array[Int]): HashCodeBuilder = {
    if (array == null) iTotal = iTotal * iConstant
    else for (element <- array) {
      append(element)
    }
    this
  }

  /**
    * <p>
    * Append a {@code hashCode} for a {@code long}.
    * </p>
    *
    * @param value
    * the long to add to the {@code hashCode}
    * @return this
    */
  // NOTE: This method uses >> and not >>> as Effective Java and
  //       Long.hashCode do. Ideally we should switch to >>> at
  //       some stage. There are backwards compat issues, so
  //       that will have to wait for the time being. cf LANG-342.
  def append(value: Long): HashCodeBuilder = {
    iTotal = iTotal * iConstant + (value ^ (value >> 32)).toInt
    this
  }

  /**
    * <p>
    * Append a {@code hashCode} for a {@code long} array.
    * </p>
    *
    * @param array
    * the array to add to the {@code hashCode}
    * @return this
    */
  def append(array: Array[Long]): HashCodeBuilder = {
    if (array == null) iTotal = iTotal * iConstant
    else for (element <- array) {
      append(element)
    }
    this
  }

  /**
    * <p>
    * Append a {@code hashCode} for an {@code Object}.
    * </p>
    *
    * @param object
    * the Object to add to the {@code hashCode}
    * @return this
    */
  def append(`object`: Any): HashCodeBuilder = {
    if (`object` == null) iTotal = iTotal * iConstant
    else if (`object`.getClass.isArray) { // factor out array case in order to keep method small enough
      // to be inlined
      appendArray(`object`)
    }
    else iTotal = iTotal * iConstant + `object`.hashCode
    this
  }

  /**
    * <p>
    * Append a {@code hashCode} for an array.
    * </p>
    *
    * @param object
    * the array to add to the {@code hashCode}
    */
  private def appendArray(`object`: Any): Unit = { // 'Switch' on type of array, to dispatch to the correct handler
    // This handles multi dimensional arrays
    if (`object`.isInstanceOf[Array[Long]]) append(`object`.asInstanceOf[Array[Long]])
    else if (`object`.isInstanceOf[Array[Int]]) append(`object`.asInstanceOf[Array[Int]])
    else if (`object`.isInstanceOf[Array[Short]]) append(`object`.asInstanceOf[Array[Short]])
    else if (`object`.isInstanceOf[Array[Char]]) append(`object`.asInstanceOf[Array[Char]])
    else if (`object`.isInstanceOf[Array[Byte]]) append(`object`.asInstanceOf[Array[Byte]])
    else if (`object`.isInstanceOf[Array[Double]]) append(`object`.asInstanceOf[Array[Double]])
    else if (`object`.isInstanceOf[Array[Float]]) append(`object`.asInstanceOf[Array[Float]])
    else if (`object`.isInstanceOf[Array[Boolean]]) append(`object`.asInstanceOf[Array[Boolean]])
    else { // Not an array of primitives
      append(`object`.asInstanceOf[Array[AnyRef]])
    }
  }

  /**
    * <p>
    * Append a {@code hashCode} for an {@code Object} array.
    * </p>
    *
    * @param array
    * the array to add to the {@code hashCode}
    * @return this
    */
  def append(array: Array[AnyRef]): HashCodeBuilder = {
    if (array == null) iTotal = iTotal * iConstant
    else for (element <- array) {
      append(element)
    }
    this
  }

  /**
    * <p>
    * Append a {@code hashCode} for a {@code short}.
    * </p>
    *
    * @param value
    * the short to add to the {@code hashCode}
    * @return this
    */
  def append(value: Short): HashCodeBuilder = {
    iTotal = iTotal * iConstant + value
    this
  }

  /**
    * <p>
    * Append a {@code hashCode} for a {@code short} array.
    * </p>
    *
    * @param array
    * the array to add to the {@code hashCode}
    * @return this
    */
  def append(array: Array[Short]): HashCodeBuilder = {
    if (array == null) iTotal = iTotal * iConstant
    else for (element <- array) {
      append(element)
    }
    this
  }

  /**
    * <p>
    * Adds the result of super.hashCode() to this builder.
    * </p>
    *
    * @param superHashCode
    * the result of calling {@code super.hashCode()}
    * @return this HashCodeBuilder, used to chain calls.
    * @since 2.0
    */
  def appendSuper(superHashCode: Int): HashCodeBuilder = {
    iTotal = iTotal * iConstant + superHashCode
    this
  }

  /**
    * <p>
    * Returns the computed {@code hashCode}.
    * </p>
    *
    * @return {@code hashCode} based on the fields appended
    */
  def toHashCode: Int = iTotal

  /**
    * Returns the computed {@code hashCode}.
    *
    * @return {@code hashCode} based on the fields appended
    * @since 3.0
    */
  override def build: Integer = Integer.valueOf(toHashCode)

  /**
    * <p>
    * The computed {@code hashCode} from toHashCode() is returned due to the likelihood
    * of bugs in mis-calling toHashCode() and the unlikeliness of it mattering what the hashCode for
    * HashCodeBuilder itself is.</p>
    *
    * @return {@code hashCode} based on the fields appended
    * @since 2.5
    */
  override def hashCode: Int = toHashCode
}