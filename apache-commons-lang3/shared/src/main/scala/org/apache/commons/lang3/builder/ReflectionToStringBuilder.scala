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

import java.lang.reflect.AccessibleObject
import java.lang.reflect.Field
import java.lang.reflect.Modifier
import java.util
import java.util.Comparator
import org.apache.commons.lang3.ArrayUtils
import org.apache.commons.lang3.ClassUtils
import org.apache.commons.lang3.Validate
import scala.collection.JavaConverters._

/**
  * <p>
  * Assists in implementing {@link Object# toString ( )} methods using reflection.
  * </p>
  * <p>
  * This class uses reflection to determine the fields to append. Because these fields are usually private, the class
  * uses {@link java.lang.reflect.AccessibleObject# setAccessible ( java.lang.reflect.AccessibleObject[ ], boolean)} to
  * change the visibility of the fields. This will fail under a security manager, unless the appropriate permissions are
  * set up correctly.
  * </p>
  * <p>
  * Using reflection to access (private) fields circumvents any synchronization protection guarding access to these
  * fields. If a toString method cannot safely read a field, you should exclude it from the toString method, or use
  * synchronization consistent with the class' lock management around the invocation of the method. Take special care to
  * exclude non-thread-safe collection classes, because these classes may throw ConcurrentModificationException if
  * modified while the toString method is executing.
  * </p>
  * <p>
  * A typical invocation for this method would look like:
  * </p>
  * <pre>
  * public String toString() {
  * return ReflectionToStringBuilder.toString(this);
  * }
  * </pre>
  * <p>
  * You can also use the builder to debug 3rd party objects:
  * </p>
  * <pre>
  * System.out.println(&quot;An object: &quot; + ReflectionToStringBuilder.toString(anObject));
  * </pre>
  * <p>
  * A subclass can control field output by overriding the methods:
  * </p>
  * <ul>
  * <li>{@link #accept ( java.lang.reflect.Field )}</li>
  * <li>{@link #getValue ( java.lang.reflect.Field )}</li>
  * </ul>
  * <p>
  * For example, this method does <i>not</i> include the {@code password} field in the returned {@code String}:
  * </p>
  * <pre>
  * public String toString() {
  * return (new ReflectionToStringBuilder(this) {
  * protected boolean accept(Field f) {
  * return super.accept(f) &amp;&amp; !f.getName().equals(&quot;password&quot;);
  * }
  * }).toString();
  * }
  * </pre>
  * <p>
  * Alternatively the {@link ToStringExclude} annotation can be used to exclude fields from being incorporated in the
  * result.
  * </p>
  * <p>
  * It is also possible to use the {@link ToStringSummary} annotation to output the summary information instead of the
  * detailed information of a field.
  * </p>
  * <p>
  * The exact format of the {@code toString} is determined by the {@link ToStringStyle} passed into the constructor.
  * </p>
  *
  * <p>
  * <b>Note:</b> the default {@link ToStringStyle} will only do a "shallow" formatting, i.e. composed objects are not
  * further traversed. To get "deep" formatting, use an instance of {@link RecursiveToStringStyle}.
  * </p>
  *
  * @since 2.0
  */
object ReflectionToStringBuilder {
  /**
    * <p>
    * Builds a {@code toString} value using the default {@code ToStringStyle} through reflection.
    * </p>
    *
    * <p>
    * It uses {@code AccessibleObject.setAccessible} to gain access to private fields. This means that it will
    * throw a security exception if run under a security manager, if the permissions are not set up correctly. It is
    * also not as efficient as testing explicitly.
    * </p>
    *
    * <p>
    * Transient members will be not be included, as they are likely derived. Static fields will not be included.
    * Superclass fields will be appended.
    * </p>
    *
    * @param object
    * the Object to be output
    * @return the String result
    * @throws IllegalArgumentException
    * if the Object is {@code null}
    * @see ToStringExclude
    * @see ToStringSummary
    */
  def toString(`object`: AnyRef): String = toString(`object`, null, false, false, null)

  /**
    * <p>
    * Builds a {@code toString} value through reflection.
    * </p>
    *
    * <p>
    * It uses {@code AccessibleObject.setAccessible} to gain access to private fields. This means that it will
    * throw a security exception if run under a security manager, if the permissions are not set up correctly. It is
    * also not as efficient as testing explicitly.
    * </p>
    *
    * <p>
    * Transient members will be not be included, as they are likely derived. Static fields will not be included.
    * Superclass fields will be appended.
    * </p>
    *
    * <p>
    * If the style is {@code null}, the default {@code ToStringStyle} is used.
    * </p>
    *
    * @param object
    * the Object to be output
    * @param style
    * the style of the {@code toString} to create, may be {@code null}
    * @return the String result
    * @throws IllegalArgumentException
    * if the Object or {@code ToStringStyle} is {@code null}
    * @see ToStringExclude
    * @see ToStringSummary
    */
  def toString(`object`: AnyRef, style: ToStringStyle): String = toString(`object`, style, false, false, null)

  /**
    * <p>
    * Builds a {@code toString} value through reflection.
    * </p>
    *
    * <p>
    * It uses {@code AccessibleObject.setAccessible} to gain access to private fields. This means that it will
    * throw a security exception if run under a security manager, if the permissions are not set up correctly. It is
    * also not as efficient as testing explicitly.
    * </p>
    *
    * <p>
    * If the {@code outputTransients} is {@code true}, transient members will be output, otherwise they
    * are ignored, as they are likely derived fields, and not part of the value of the Object.
    * </p>
    *
    * <p>
    * Static fields will not be included. Superclass fields will be appended.
    * </p>
    *
    * <p>
    * If the style is {@code null}, the default {@code ToStringStyle} is used.
    * </p>
    *
    * @param object
    * the Object to be output
    * @param style
    * the style of the {@code toString} to create, may be {@code null}
    * @param outputTransients
    * whether to include transient fields
    * @return the String result
    * @throws IllegalArgumentException
    * if the Object is {@code null}
    * @see ToStringExclude
    * @see ToStringSummary
    */
  def toString(`object`: AnyRef, style: ToStringStyle, outputTransients: Boolean): String = toString(`object`, style, outputTransients, false, null)

  /**
    * <p>
    * Builds a {@code toString} value through reflection.
    * </p>
    *
    * <p>
    * It uses {@code AccessibleObject.setAccessible} to gain access to private fields. This means that it will
    * throw a security exception if run under a security manager, if the permissions are not set up correctly. It is
    * also not as efficient as testing explicitly.
    * </p>
    *
    * <p>
    * If the {@code outputTransients} is {@code true}, transient fields will be output, otherwise they
    * are ignored, as they are likely derived fields, and not part of the value of the Object.
    * </p>
    *
    * <p>
    * If the {@code outputStatics} is {@code true}, static fields will be output, otherwise they are
    * ignored.
    * </p>
    *
    * <p>
    * Static fields will not be included. Superclass fields will be appended.
    * </p>
    *
    * <p>
    * If the style is {@code null}, the default {@code ToStringStyle} is used.
    * </p>
    *
    * @param object
    * the Object to be output
    * @param style
    * the style of the {@code toString} to create, may be {@code null}
    * @param outputTransients
    * whether to include transient fields
    * @param outputStatics
    * whether to include static fields
    * @return the String result
    * @throws IllegalArgumentException
    * if the Object is {@code null}
    * @see ToStringExclude
    * @see ToStringSummary
    * @since 2.1
    */
  def toString(`object`: AnyRef, style: ToStringStyle, outputTransients: Boolean, outputStatics: Boolean): String = toString(`object`, style, outputTransients, outputStatics, null)

  /**
    * <p>
    * Builds a {@code toString} value through reflection.
    * </p>
    *
    * <p>
    * It uses {@code AccessibleObject.setAccessible} to gain access to private fields. This means that it will
    * throw a security exception if run under a security manager, if the permissions are not set up correctly. It is
    * also not as efficient as testing explicitly.
    * </p>
    *
    * <p>
    * If the {@code outputTransients} is {@code true}, transient fields will be output, otherwise they
    * are ignored, as they are likely derived fields, and not part of the value of the Object.
    * </p>
    *
    * <p>
    * If the {@code outputStatics} is {@code true}, static fields will be output, otherwise they are
    * ignored.
    * </p>
    *
    * <p>
    * Superclass fields will be appended up to and including the specified superclass. A null superclass is treated as
    * {@code java.lang.Object}.
    * </p>
    *
    * <p>
    * If the style is {@code null}, the default {@code ToStringStyle} is used.
    * </p>
    *
    * @param < T>
    *          the type of the object
    * @param object
    *          the Object to be output
    * @param style
    *          the style of the {@code toString} to create, may be {@code null}
    * @param outputTransients
    *          whether to include transient fields
    * @param outputStatics
    *          whether to include static fields
    * @param reflectUpToClass
    *          the superclass to reflect up to (inclusive), may be {@code null}
    * @return the String result
    * @throws IllegalArgumentException
    * if the Object is {@code null}
    * @see ToStringExclude
    * @see ToStringSummary
    * @since 2.1
    */
  def toString[T <: AnyRef](`object`: T, style: ToStringStyle, outputTransients: Boolean, outputStatics: Boolean, reflectUpToClass: Class[_ >: T]): String = new ReflectionToStringBuilder(`object`, style, null, reflectUpToClass, outputTransients, outputStatics).toString

  /**
    * <p>
    * Builds a {@code toString} value through reflection.
    * </p>
    *
    * <p>
    * It uses {@code AccessibleObject.setAccessible} to gain access to private fields. This means that it will
    * throw a security exception if run under a security manager, if the permissions are not set up correctly. It is
    * also not as efficient as testing explicitly.
    * </p>
    *
    * <p>
    * If the {@code outputTransients} is {@code true}, transient fields will be output, otherwise they
    * are ignored, as they are likely derived fields, and not part of the value of the Object.
    * </p>
    *
    * <p>
    * If the {@code outputStatics} is {@code true}, static fields will be output, otherwise they are
    * ignored.
    * </p>
    *
    * <p>
    * Superclass fields will be appended up to and including the specified superclass. A null superclass is treated as
    * {@code java.lang.Object}.
    * </p>
    *
    * <p>
    * If the style is {@code null}, the default {@code ToStringStyle} is used.
    * </p>
    *
    * @param < T>
    *          the type of the object
    * @param object
    *          the Object to be output
    * @param style
    *          the style of the {@code toString} to create, may be {@code null}
    * @param outputTransients
    *          whether to include transient fields
    * @param outputStatics
    *          whether to include static fields
    * @param excludeNullValues
    *          whether to exclude fields whose values are null
    * @param reflectUpToClass
    *          the superclass to reflect up to (inclusive), may be {@code null}
    * @return the String result
    * @throws IllegalArgumentException
    * if the Object is {@code null}
    * @see ToStringExclude
    * @see ToStringSummary
    * @since 3.6
    */
  def toString[T <: AnyRef](`object`: T, style: ToStringStyle, outputTransients: Boolean, outputStatics: Boolean, excludeNullValues: Boolean, reflectUpToClass: Class[_ >: T]): String =
    new ReflectionToStringBuilder(`object`, style, null, reflectUpToClass, outputTransients, outputStatics, excludeNullValues).toString

  /**
    * Builds a String for a toString method excluding the given field names.
    *
    * @param object
    * The object to "toString".
    * @param excludeFieldNames
    * The field names to exclude. Null excludes nothing.
    * @return The toString value.
    */
  def toStringExclude(`object`: AnyRef, excludeFieldNames: util.Collection[String]): String =
    toStringExclude(`object`, toNoNullStringArray(excludeFieldNames):_*)

  /**
    * Converts the given Collection into an array of Strings. The returned array does not contain {@code null}
    * entries. Note that {@link Arrays# sort ( Object[ ] )} will throw an {@link NullPointerException} if an array element
    * is {@code null}.
    *
    * @param collection
    * The collection to convert
    * @return A new array of Strings.
    */
  private[builder] def toNoNullStringArray(collection: util.Collection[String]): Array[String] = {
    if (collection == null) return ArrayUtils.EMPTY_STRING_ARRAY
    toNoNullStringArray(collection.toArray)
  }

  /**
    * Returns a new array of Strings without null elements. Internal method used to normalize exclude lists
    * (arrays and collections). Note that {@link Arrays# sort ( Object[ ] )} will throw an {@link NullPointerException}
    * if an array element is {@code null}.
    *
    * @param array
    * The array to check
    * @return The given array or a new array without null.
    */
  private[builder] def toNoNullStringArray(array: Array[AnyRef]): Array[String] = {
    val list = new util.ArrayList[String](array.length)
    for (e <- array) {
      if (e != null) list.add(e.toString)
    }
    list.toArray(ArrayUtils.EMPTY_STRING_ARRAY)
  }

  /**
    * Builds a String for a toString method excluding the given field names.
    *
    * @param object
    * The object to "toString".
    * @param excludeFieldNames
    * The field names to exclude
    * @return The toString value.
    */
  def toStringExclude(`object`: AnyRef, excludeFieldNames: String*): String =
    new ReflectionToStringBuilder(`object`)
      .setExcludeFieldNames(excludeFieldNames:_*)
      .toString

  private def checkNotNull(obj: AnyRef): AnyRef = Validate.notNull(obj, "The Object passed in should not be null.")
}

/**
  * <p>
  * Constructor.
  * </p>
  *
  * <p>
  * If the style is {@code null}, the default style is used.
  * </p>
  *
  * <p>
  * If the buffer is {@code null}, a new one is created.
  * </p>
  *
  * @tparam T
  * the type of the object
  * @param object
  * the Object to build a {@code toString} for
  * @param style
  * the style of the {@code toString} to create, may be {@code null}
  * @param buffer
  * the {@code StringBuffer} to populate, may be {@code null}
  * @throws IllegalArgumentException
  * if the Object passed in is {@code null}
  */
class ReflectionToStringBuilder[T <: AnyRef](`object`: T, style: ToStringStyle, buffer: StringBuffer)
  extends ToStringBuilder(ReflectionToStringBuilder.checkNotNull(`object`), style, buffer) {
  /**
    * Whether or not to append static fields.
    */
  private var appendStatics = false
  /**
    * Whether or not to append transient fields.
    */
  private var appendTransients = false
  /**
    * Whether or not to append fields that are null.
    */
  private var excludeNullValues = false
  /**
    * Which field names to exclude from output. Intended for fields like {@code "password"}.
    *
    * @since 3.0 this is protected instead of private
    */
  protected var excludeFieldNames: Array[String] = null
  /**
    * The last super class to stop appending fields for.
    */
  private var upToClass: Class[_] = null

  def this() {
    this(null.asInstanceOf[Nothing], null, null)
  }

  /**
    * <p>
    * Constructor.
    * </p>
    *
    * <p>
    * This constructor outputs using the default style set with {@code setDefaultStyle}.
    * </p>
    *
    * @param object
    * the Object to build a {@code toString} for, must not be {@code null}
    * @throws IllegalArgumentException
    * if the Object passed in is {@code null}
    */
  def this(`object`: T) {
    this(`object`, null, null)
  }

  /**
    * <p>
    * Constructor.
    * </p>
    *
    * <p>
    * If the style is {@code null}, the default style is used.
    * </p>
    *
    * @param object
    * the Object to build a {@code toString} for, must not be {@code null}
    * @param style
    * the style of the {@code toString} to create, may be {@code null}
    * @throws IllegalArgumentException
    * if the Object passed in is {@code null}
    */
  def this(`object`: T, style: ToStringStyle) {
    this(`object`, style, null)
  }

  /**
    * Constructor.
    *
    * @param object
    *          the Object to build a {@code toString} for
    * @param style
    *          the style of the {@code toString} to create, may be {@code null}
    * @param buffer
    *          the {@code StringBuffer} to populate, may be {@code null}
    * @param reflectUpToClass
    *          the superclass to reflect up to (inclusive), may be {@code null}
    * @param outputTransients
    *          whether to include transient fields
    * @param outputStatics
    *          whether to include static fields
    * @since 2.1
    */
  def this(
    `object`: T,
    style: ToStringStyle,
    buffer: StringBuffer,
    reflectUpToClass: Class[_ >: T],
    outputTransients: Boolean,
    outputStatics: Boolean
  ) {
    this(`object`, style, buffer)
    this.setUpToClass(reflectUpToClass)
    this.setAppendTransients(outputTransients)
    this.setAppendStatics(outputStatics)
  }

  /**
    * Constructor.
    *
    * @param object
    *          the Object to build a {@code toString} for
    * @param style
    *          the style of the {@code toString} to create, may be {@code null}
    * @param buffer
    *          the {@code StringBuffer} to populate, may be {@code null}
    * @param reflectUpToClass
    *          the superclass to reflect up to (inclusive), may be {@code null}
    * @param outputTransients
    *          whether to include transient fields
    * @param outputStatics
    *          whether to include static fields
    * @param excludeNullValues
    *          whether to exclude fields who value is null
    * @since 3.6
    */
  def this(`object`: T, style: ToStringStyle, buffer: StringBuffer, reflectUpToClass: Class[_ >: T], outputTransients: Boolean, outputStatics: Boolean, excludeNullValues: Boolean) {
    this(`object`, style, buffer)
    this.setUpToClass(reflectUpToClass)
    this.setAppendTransients(outputTransients)
    this.setAppendStatics(outputStatics)
    this.setExcludeNullValues(excludeNullValues)
  }

  /**
    * Returns whether or not to append the given {@code Field}.
    * <ul>
    * <li>Transient fields are appended only if {@link #isAppendTransients ( )} returns {@code true}.
    * <li>Static fields are appended only if {@link #isAppendStatics ( )} returns {@code true}.
    * <li>Inner class fields are not appended.</li>
    * </ul>
    *
    * @param field
    * The Field to test.
    * @return Whether or not to append the given {@code Field}.
    */
  protected def accept(field: Field): Boolean = {
    if (field.getName.indexOf(ClassUtils.INNER_CLASS_SEPARATOR_CHAR) != -1) false // Reject field from inner class.
    else if (Modifier.isTransient(field.getModifiers) && !this.isAppendTransients) false // Reject transient fields.
    else if (Modifier.isStatic(field.getModifiers) && !this.isAppendStatics) false // Reject static fields.
    else if (this.excludeFieldNames != null && util.Arrays.binarySearch(this.excludeFieldNames.map{ s: String => s: Object }, field.getName) >= 0) false // Reject fields from the getExcludeFieldNames list.
    else !field.isAnnotationPresent(classOf[ToStringExclude])
  }

  /**
    * <p>
    * Appends the fields and values defined by the given object of the given Class.
    * </p>
    *
    * <p>
    * If a cycle is detected as an object is &quot;toString()'ed&quot;, such an object is rendered as if
    * {@code Object.toString()} had been called and not implemented by the object.
    * </p>
    *
    * @param clazz
    * The class of object parameter
    */
  protected def appendFieldsIn(clazz: Class[AnyRef]): Unit = {
    if (clazz.isArray) {
      this.reflectionAppendArray(this.getObject)
      return
    }
    // The elements in the returned array are not sorted and are not in any particular order.
    val fields = clazz.getDeclaredFields
    fields.foreach{ _.setAccessible(true) }

    for (field <- fields.sortBy{ _.getName }) {
      val fieldName = field.getName
      if (this.accept(field)) try { // Warning: Field.get(Object) creates wrappers objects
        // for primitive types.
        val fieldValue = this.getValue(field).asInstanceOf[AnyRef]
        if (!excludeNullValues || fieldValue != null) this.append(fieldName, fieldValue, !field.isAnnotationPresent(classOf[ToStringSummary]))
      } catch {
        case ex: IllegalAccessException =>
          //this can't happen. Would get a Security exception instead
          //throw a runtime exception in case the impossible happens.
          throw new InternalError("Unexpected IllegalAccessException: " + ex.getMessage)
      }
    }
  }

  /**
    * @return Returns the excludeFieldNames.
    */
  def getExcludeFieldNames: Array[String] = this.excludeFieldNames.clone

  /**
    * <p>
    * Gets the last super class to stop appending fields for.
    * </p>
    *
    * @return The last super class to stop appending fields for.
    */
  def getUpToClass: Class[_] = this.upToClass

  /**
    * <p>
    * Calls {@code java.lang.reflect.Field.get(Object)}.
    * </p>
    *
    * @param field
    * The Field to query.
    * @return The Object from the given Field.
    * @throws IllegalArgumentException
    * see {@link java.lang.reflect.Field# get ( Object )}
    * @throws IllegalAccessException
    * see {@link java.lang.reflect.Field# get ( Object )}
    * @see java.lang.reflect.Field#get(Object)
    */
  @throws[IllegalAccessException]
  protected def getValue(field: Field): Any = field.get(this.getObject)

  /**
    * <p>
    * Gets whether or not to append static fields.
    * </p>
    *
    * @return Whether or not to append static fields.
    * @since 2.1
    */
  def isAppendStatics: Boolean = this.appendStatics

  /**
    * <p>
    * Gets whether or not to append transient fields.
    * </p>
    *
    * @return Whether or not to append transient fields.
    */
  def isAppendTransients: Boolean = this.appendTransients

  /**
    * <p>
    * Gets whether or not to append fields whose values are null.
    * </p>
    *
    * @return Whether or not to append fields whose values are null.
    * @since 3.6
    */
  def isExcludeNullValues: Boolean = this.excludeNullValues

  /**
    * <p>
    * Append to the {@code toString} an {@code Object} array.
    * </p>
    *
    * @param array
    * the array to add to the {@code toString}
    * @return this
    */
  def reflectionAppendArray(array: AnyRef): ReflectionToStringBuilder[_] = {
    this.getStyle.reflectionAppendArrayDetail(this.getStringBuffer, null, array)
    this
  }

  /**
    * <p>
    * Sets whether or not to append static fields.
    * </p>
    *
    * @param appendStatics
    * Whether or not to append static fields.
    * @since 2.1
    */
  def setAppendStatics(appendStatics: Boolean): Unit = {
    this.appendStatics = appendStatics
  }

  /**
    * <p>
    * Sets whether or not to append transient fields.
    * </p>
    *
    * @param appendTransients
    * Whether or not to append transient fields.
    */
  def setAppendTransients(appendTransients: Boolean): Unit = {
    this.appendTransients = appendTransients
  }

  /**
    * <p>
    * Sets whether or not to append fields whose values are null.
    * </p>
    *
    * @param excludeNullValues
    * Whether or not to append fields whose values are null.
    * @since 3.6
    */
  def setExcludeNullValues(excludeNullValues: Boolean): Unit = {
    this.excludeNullValues = excludeNullValues
  }

  /**
    * Sets the field names to exclude.
    *
    * @param excludeFieldNamesParam
    * The excludeFieldNames to excluding from toString or {@code null}.
    * @return {@code this}
    */
  def setExcludeFieldNames(excludeFieldNamesParam: String*): ReflectionToStringBuilder[_] = {
    if (excludeFieldNamesParam == null) this.excludeFieldNames = null
    else { //clone and remove nulls
      this.excludeFieldNames = ReflectionToStringBuilder.toNoNullStringArray(excludeFieldNamesParam.toArray: Array[AnyRef]).sorted
    }
    this
  }

  /**
    * <p>
    * Sets the last super class to stop appending fields for.
    * </p>
    *
    * @param clazz
    * The last super class to stop appending fields for.
    */
  def setUpToClass(clazz: Class[_]): Unit = {
    if (clazz != null) {
      val `object` = getObject
      if (`object` != null && !clazz.isInstance(`object`)) throw new IllegalArgumentException("Specified class is not a superclass of the object")
    }
    this.upToClass = clazz
  }

  /**
    * <p>
    * Gets the String built by this builder.
    * </p>
    *
    * @return the built string
    */
  override def toString: String = {
    if (this.getObject == null) return this.getStyle.getNullText
    var clazz = this.getObject.getClass.asInstanceOf[Class[AnyRef]]
    this.appendFieldsIn(clazz)
    while ( {
      clazz.getSuperclass != null && (clazz ne this.getUpToClass)
    }) {
      clazz = clazz.getSuperclass.asInstanceOf[Class[AnyRef]]
      this.appendFieldsIn(clazz)
    }
    super.toString
  }
}