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

import org.apache.commons.lang3.ObjectUtils
import org.apache.commons.lang3.Validate
import java.lang.{Boolean => JavaBoolean}

/**
  * <p>Assists in implementing {@link Object# toString ( )} methods.</p>
  *
  * <p>This class enables a good and consistent {@code toString()} to be built for any
  * class or object. This class aims to simplify the process by:</p>
  * <ul>
  * <li>allowing field names</li>
  * <li>handling all types consistently</li>
  * <li>handling nulls consistently</li>
  * <li>outputting arrays and multi-dimensional arrays</li>
  * <li>enabling the detail level to be controlled for Objects and Collections</li>
  * <li>handling class hierarchies</li>
  * </ul>
  *
  * <p>To use this class write code as follows:</p>
  *
  * <pre>
  * public class Person {
  *   String name;
  *   int age;
  *   boolean smoker;
  *
  *   ...
  *
  *   public String toString() {
  *     return new ToStringBuilder(this).
  *       append("name", name).
  *       append("age", age).
  *       append("smoker", smoker).
  *       toString();
  *   }
  * }
  * </pre>
  *
  * <p>This will produce a toString of the format:
  * {@code Person@7f54[name=Stephen,age=29,smoker=false]}</p>
  *
  * <p>To add the superclass {@code toString}, use {@link #appendSuper}.
  * To append the {@code toString} from an object that is delegated
  * to (or any other object), use {@link #appendToString}.</p>
  *
  * <p>Alternatively, there is a method that uses reflection to determine
  * the fields to test. Because these fields are usually private, the method,
  * {@code reflectionToString}, uses {@code AccessibleObject.setAccessible} to
  * change the visibility of the fields. This will fail under a security manager,
  * unless the appropriate permissions are set up correctly. It is also
  * slower than testing explicitly.</p>
  *
  * <p>A typical invocation for this method would look like:</p>
  *
  * <pre>
  * public String toString() {
  *   return ToStringBuilder.reflectionToString(this);
  * }
  * </pre>
  *
  * <p>You can also use the builder to debug 3rd party objects:</p>
  *
  * <pre>
  * System.out.println("An object: " + ToStringBuilder.reflectionToString(anObject));
  * </pre>
  *
  * <p>The exact format of the {@code toString} is determined by
  * the {@link ToStringStyle} passed into the constructor.</p>
  *
  * @since 1.0
  */
object ToStringBuilder {
  /**
    * The default style of output to use, not null.
    */
  private var defaultStyle: ToStringStyle = ToStringStyle.DEFAULT_STYLE

  /**
    * <p>Gets the default {@code ToStringStyle} to use.</p>
    *
    * <p>This method gets a singleton default value, typically for the whole JVM.
    * Changing this default should generally only be done during application startup.
    * It is recommended to pass a {@code ToStringStyle} to the constructor instead
    * of using this global default.</p>
    *
    * <p>This method can be used from multiple threads.
    * Internally, a {@code volatile} variable is used to provide the guarantee
    * that the latest value set using {@link #setDefaultStyle} is the value returned.
    * It is strongly recommended that the default style is only changed during application startup.</p>
    *
    * <p>One reason for changing the default could be to have a verbose style during
    * development and a compact style in production.</p>
    *
    * @return the default {@code ToStringStyle}, never null
    */
  def getDefaultStyle: ToStringStyle = defaultStyle

  /**
    * <p>Sets the default {@code ToStringStyle} to use.</p>
    *
    * <p>This method sets a singleton default value, typically for the whole JVM.
    * Changing this default should generally only be done during application startup.
    * It is recommended to pass a {@code ToStringStyle} to the constructor instead
    * of changing this global default.</p>
    *
    * <p>This method is not intended for use from multiple threads.
    * Internally, a {@code volatile} variable is used to provide the guarantee
    * that the latest value set is the value returned from {@link #getDefaultStyle}.</p>
    *
    * @param style the default {@code ToStringStyle}
    * @throws IllegalArgumentException if the style is {@code null}
    */
  def setDefaultStyle(style: ToStringStyle): Unit = {
    defaultStyle = Validate.notNull(style, "The style must not be null")
  }

  /**
    * <p>Uses {@code ReflectionToStringBuilder} to generate a
    * {@code toString} for the specified object.</p>
    *
    * @param object the Object to be output
    * @return the String result
    * @see ReflectionToStringBuilder#toString(Object)
    */
  def reflectionToString(`object`: Any): String = ReflectionToStringBuilder.toString(`object`)

  /**
    * <p>Uses {@code ReflectionToStringBuilder} to generate a
    * {@code toString} for the specified object.</p>
    *
    * @param object the Object to be output
    * @param style  the style of the {@code toString} to create, may be {@code null}
    * @return the String result
    * @see ReflectionToStringBuilder#toString(Object,ToStringStyle)
    */
  def reflectionToString(`object`: Any, style: ToStringStyle): String = ReflectionToStringBuilder.toString(`object`, style)

  /**
    * <p>Uses {@code ReflectionToStringBuilder} to generate a
    * {@code toString} for the specified object.</p>
    *
    * @param object           the Object to be output
    * @param style            the style of the {@code toString} to create, may be {@code null}
    * @param outputTransients whether to include transient fields
    * @return the String result
    * @see ReflectionToStringBuilder#toString(Object,ToStringStyle,boolean)
    */
  def reflectionToString(`object`: Any, style: ToStringStyle, outputTransients: Boolean): String = ReflectionToStringBuilder.toString(`object`, style, outputTransients, false, null)

  /**
    * <p>Uses {@code ReflectionToStringBuilder} to generate a
    * {@code toString} for the specified object.</p>
    *
    * @param <                T> the type of the object
    * @param object           the Object to be output
    * @param style            the style of the {@code toString} to create, may be {@code null}
    * @param outputTransients whether to include transient fields
    * @param reflectUpToClass the superclass to reflect up to (inclusive), may be {@code null}
    * @return the String result
    * @see ReflectionToStringBuilder#toString(Object,ToStringStyle,boolean,boolean,Class)
    * @since 2.0
    */
  def reflectionToString[T](`object`: T, style: ToStringStyle, outputTransients: Boolean, reflectUpToClass: Class[_ >: T]): String = ReflectionToStringBuilder.toString(`object`, style, outputTransients, false, reflectUpToClass)
}


/**
  * <p>Constructs a builder for the specified object.</p>
  *
  * <p>If the style is {@code null}, the default style is used.</p>
  *
  * <p>If the buffer is {@code null}, a new one is created.</p>
  *
  * @param object the Object to build a {@code toString} for, not recommended to be null
  * @param style  the style of the {@code toString} to create, null uses the default style
  * @param buffer the {@code StringBuffer} to populate, may be null
  */
class ToStringBuilder(`object`: Any, private var style: ToStringStyle, private var buffer: StringBuffer)
  extends Builder[String] {
  if (style == null) style = ToStringBuilder.getDefaultStyle
  if (buffer == null) buffer = new StringBuffer(512)
  style.appendStart(buffer, `object`)

  /**
    * <p>Constructs a builder for the specified object using the default output style.</p>
    *
    * <p>This default style is obtained from {@link #getDefaultStyle ( )}.</p>
    *
    * @param object the Object to build a {@code toString} for, not recommended to be null
    */
  def this(`object`: Any) {
    this(`object`, null, null)
  }

  /**
    * <p>Constructs a builder for the specified object using the defined output style.</p>
    *
    * <p>If the style is {@code null}, the default style is used.</p>
    *
    * @param object the Object to build a {@code toString} for, not recommended to be null
    * @param style  the style of the {@code toString} to create, null uses the default style
    */
  def this(`object`: Any, style: ToStringStyle) {
    this(`object`, style, null)
  }

  /**
    * <p>Append to the {@code toString} a {@code boolean}
    * value.</p>
    *
    * @param value the value to add to the {@code toString}
    * @return this
    */
  def append(value: Boolean): ToStringBuilder = {
    style.append(buffer, null, value)
    this
  }

  /**
    * <p>Append to the {@code toString} a {@code boolean}
    * array.</p>
    *
    * @param array the array to add to the {@code toString}
    * @return this
    */
  def append(array: Array[Boolean]): ToStringBuilder = {
    style.append(buffer, null, array, null.asInstanceOf[Boolean])
    this
  }

  /**
    * <p>Append to the {@code toString} a {@code byte}
    * value.</p>
    *
    * @param value the value to add to the {@code toString}
    * @return this
    */
  def append(value: Byte): ToStringBuilder = {
    style.append(buffer, null, value)
    this
  }

  /**
    * <p>Append to the {@code toString} a {@code byte}
    * array.</p>
    *
    * @param array the array to add to the {@code toString}
    * @return this
    */
  def append(array: Array[Byte]): ToStringBuilder = {
    style.append(buffer, null, array, null.asInstanceOf[Boolean])
    this
  }

  /**
    * <p>Append to the {@code toString} a {@code char}
    * value.</p>
    *
    * @param value the value to add to the {@code toString}
    * @return this
    */
  def append(value: Char): ToStringBuilder = {
    style.append(buffer, null, value)
    this
  }

  /**
    * <p>Append to the {@code toString} a {@code char}
    * array.</p>
    *
    * @param array the array to add to the {@code toString}
    * @return this
    */
  def append(array: Array[Char]): ToStringBuilder = {
    style.append(buffer, null, array, null.asInstanceOf[Boolean])
    this
  }

  /**
    * <p>Append to the {@code toString} a {@code double}
    * value.</p>
    *
    * @param value the value to add to the {@code toString}
    * @return this
    */
  def append(value: Double): ToStringBuilder = {
    style.append(buffer, null, value)
    this
  }

  /**
    * <p>Append to the {@code toString} a {@code double}
    * array.</p>
    *
    * @param array the array to add to the {@code toString}
    * @return this
    */
  def append(array: Array[Double]): ToStringBuilder = {
    style.append(buffer, null, array, null.asInstanceOf[Boolean])
    this
  }

  /**
    * <p>Append to the {@code toString} a {@code float}
    * value.</p>
    *
    * @param value the value to add to the {@code toString}
    * @return this
    */
  def append(value: Float): ToStringBuilder = {
    style.append(buffer, null, value)
    this
  }

  /**
    * <p>Append to the {@code toString} a {@code float}
    * array.</p>
    *
    * @param array the array to add to the {@code toString}
    * @return this
    */
  def append(array: Array[Float]): ToStringBuilder = {
    style.append(buffer, null, array, null.asInstanceOf[Boolean])
    this
  }

  /**
    * <p>Append to the {@code toString} an {@code int}
    * value.</p>
    *
    * @param value the value to add to the {@code toString}
    * @return this
    */
  def append(value: Int): ToStringBuilder = {
    style.append(buffer, null, value)
    this
  }

  /**
    * <p>Append to the {@code toString} an {@code int}
    * array.</p>
    *
    * @param array the array to add to the {@code toString}
    * @return this
    */
  def append(array: Array[Int]): ToStringBuilder = {
    style.append(buffer, null, array, null.asInstanceOf[Boolean])
    this
  }

  /**
    * <p>Append to the {@code toString} a {@code long}
    * value.</p>
    *
    * @param value the value to add to the {@code toString}
    * @return this
    */
  def append(value: Long): ToStringBuilder = {
    style.append(buffer, null, value)
    this
  }

  /**
    * <p>Append to the {@code toString} a {@code long}
    * array.</p>
    *
    * @param array the array to add to the {@code toString}
    * @return this
    */
  def append(array: Array[Long]): ToStringBuilder = {
    style.append(buffer, null, array, null.asInstanceOf[Boolean])
    this
  }

  /**
    * <p>Append to the {@code toString} an {@code Object}
    * value.</p>
    *
    * @param obj the value to add to the {@code toString}
    * @return this
    */
  def append(obj: Any): ToStringBuilder = {
    style.append(buffer, null, obj, null.asInstanceOf[Boolean])
    this
  }

  /**
    * <p>Append to the {@code toString} an {@code Object}
    * array.</p>
    *
    * @param array the array to add to the {@code toString}
    * @return this
    */
  def append(array: Array[AnyRef]): ToStringBuilder = {
    style.append(buffer, null, array, null.asInstanceOf[Boolean])
    this
  }

  /**
    * <p>Append to the {@code toString} a {@code short}
    * value.</p>
    *
    * @param value the value to add to the {@code toString}
    * @return this
    */
  def append(value: Short): ToStringBuilder = {
    style.append(buffer, null, value)
    this
  }

  /**
    * <p>Append to the {@code toString} a {@code short}
    * array.</p>
    *
    * @param array the array to add to the {@code toString}
    * @return this
    */
  def append(array: Array[Short]): ToStringBuilder = {
    style.append(buffer, null, array, null.asInstanceOf[Boolean])
    this
  }

  /**
    * <p>Append to the {@code toString} a {@code boolean}
    * value.</p>
    *
    * @param fieldName the field name
    * @param value     the value to add to the {@code toString}
    * @return this
    */
  def append(fieldName: String, value: Boolean): ToStringBuilder = {
    style.append(buffer, fieldName, value)
    this
  }

  /**
    * <p>Append to the {@code toString} a {@code boolean}
    * array.</p>
    *
    * @param fieldName the field name
    * @param array     the array to add to the {@code hashCode}
    * @return this
    */
  def append(fieldName: String, array: Array[Boolean]): ToStringBuilder = {
    style.append(buffer, fieldName, array, null.asInstanceOf[Boolean])
    this
  }

  /**
    * <p>Append to the {@code toString} a {@code boolean}
    * array.</p>
    *
    * <p>A boolean parameter controls the level of detail to show.
    * Setting {@code true} will output the array in full. Setting
    * {@code false} will output a summary, typically the size of
    * the array.</p>
    *
    * @param fieldName  the field name
    * @param array      the array to add to the {@code toString}
    * @param fullDetail {@code true} for detail, {@code false}
    *                   for summary info
    * @return this
    */
  def append(fieldName: String, array: Array[Boolean], fullDetail: Boolean): ToStringBuilder = {
    style.append(buffer, fieldName, array, JavaBoolean.valueOf(fullDetail))
    this
  }

  /**
    * <p>Append to the {@code toString} an {@code byte}
    * value.</p>
    *
    * @param fieldName the field name
    * @param value     the value to add to the {@code toString}
    * @return this
    */
  def append(fieldName: String, value: Byte): ToStringBuilder = {
    style.append(buffer, fieldName, value)
    this
  }

  /**
    * <p>Append to the {@code toString} a {@code byte} array.</p>
    *
    * @param fieldName the field name
    * @param array     the array to add to the {@code toString}
    * @return this
    */
  def append(fieldName: String, array: Array[Byte]): ToStringBuilder = {
    style.append(buffer, fieldName, array, null.asInstanceOf[Boolean])
    this
  }

  /**
    * <p>Append to the {@code toString} a {@code byte}
    * array.</p>
    *
    * <p>A boolean parameter controls the level of detail to show.
    * Setting {@code true} will output the array in full. Setting
    * {@code false} will output a summary, typically the size of
    * the array.
    *
    * @param fieldName  the field name
    * @param array      the array to add to the {@code toString}
    * @param fullDetail {@code true} for detail, {@code false}
    *                   for summary info
    * @return this
    */
  def append(fieldName: String, array: Array[Byte], fullDetail: Boolean): ToStringBuilder = {
    style.append(buffer, fieldName, array, JavaBoolean.valueOf(fullDetail))
    this
  }

  /**
    * <p>Append to the {@code toString} a {@code char}
    * value.</p>
    *
    * @param fieldName the field name
    * @param value     the value to add to the {@code toString}
    * @return this
    */
  def append(fieldName: String, value: Char): ToStringBuilder = {
    style.append(buffer, fieldName, value)
    this
  }

  /**
    * <p>Append to the {@code toString} a {@code char}
    * array.</p>
    *
    * @param fieldName the field name
    * @param array     the array to add to the {@code toString}
    * @return this
    */
  def append(fieldName: String, array: Array[Char]): ToStringBuilder = {
    style.append(buffer, fieldName, array, null.asInstanceOf[Boolean])
    this
  }

  /**
    * <p>Append to the {@code toString} a {@code char}
    * array.</p>
    *
    * <p>A boolean parameter controls the level of detail to show.
    * Setting {@code true} will output the array in full. Setting
    * {@code false} will output a summary, typically the size of
    * the array.</p>
    *
    * @param fieldName  the field name
    * @param array      the array to add to the {@code toString}
    * @param fullDetail {@code true} for detail, {@code false}
    *                   for summary info
    * @return this
    */
  def append(fieldName: String, array: Array[Char], fullDetail: Boolean): ToStringBuilder = {
    style.append(buffer, fieldName, array, JavaBoolean.valueOf(fullDetail))
    this
  }

  /**
    * <p>Append to the {@code toString} a {@code double}
    * value.</p>
    *
    * @param fieldName the field name
    * @param value     the value to add to the {@code toString}
    * @return this
    */
  def append(fieldName: String, value: Double): ToStringBuilder = {
    style.append(buffer, fieldName, value)
    this
  }

  /**
    * <p>Append to the {@code toString} a {@code double}
    * array.</p>
    *
    * @param fieldName the field name
    * @param array     the array to add to the {@code toString}
    * @return this
    */
  def append(fieldName: String, array: Array[Double]): ToStringBuilder = {
    style.append(buffer, fieldName, array, null.asInstanceOf[Boolean])
    this
  }

  /**
    * <p>Append to the {@code toString} a {@code double}
    * array.</p>
    *
    * <p>A boolean parameter controls the level of detail to show.
    * Setting {@code true} will output the array in full. Setting
    * {@code false} will output a summary, typically the size of
    * the array.</p>
    *
    * @param fieldName  the field name
    * @param array      the array to add to the {@code toString}
    * @param fullDetail {@code true} for detail, {@code false}
    *                   for summary info
    * @return this
    */
  def append(fieldName: String, array: Array[Double], fullDetail: Boolean): ToStringBuilder = {
    style.append(buffer, fieldName, array, JavaBoolean.valueOf(fullDetail))
    this
  }

  /**
    * <p>Append to the {@code toString} an {@code float}
    * value.</p>
    *
    * @param fieldName the field name
    * @param value     the value to add to the {@code toString}
    * @return this
    */
  def append(fieldName: String, value: Float): ToStringBuilder = {
    style.append(buffer, fieldName, value)
    this
  }

  /**
    * <p>Append to the {@code toString} a {@code float}
    * array.</p>
    *
    * @param fieldName the field name
    * @param array     the array to add to the {@code toString}
    * @return this
    */
  def append(fieldName: String, array: Array[Float]): ToStringBuilder = {
    style.append(buffer, fieldName, array, null.asInstanceOf[Boolean])
    this
  }

  /**
    * <p>Append to the {@code toString} a {@code float}
    * array.</p>
    *
    * <p>A boolean parameter controls the level of detail to show.
    * Setting {@code true} will output the array in full. Setting
    * {@code false} will output a summary, typically the size of
    * the array.</p>
    *
    * @param fieldName  the field name
    * @param array      the array to add to the {@code toString}
    * @param fullDetail {@code true} for detail, {@code false}
    *                   for summary info
    * @return this
    */
  def append(fieldName: String, array: Array[Float], fullDetail: Boolean): ToStringBuilder = {
    style.append(buffer, fieldName, array, JavaBoolean.valueOf(fullDetail))
    this
  }

  /**
    * <p>Append to the {@code toString} an {@code int}
    * value.</p>
    *
    * @param fieldName the field name
    * @param value     the value to add to the {@code toString}
    * @return this
    */
  def append(fieldName: String, value: Int): ToStringBuilder = {
    style.append(buffer, fieldName, value)
    this
  }

  /**
    * <p>Append to the {@code toString} an {@code int}
    * array.</p>
    *
    * @param fieldName the field name
    * @param array     the array to add to the {@code toString}
    * @return this
    */
  def append(fieldName: String, array: Array[Int]): ToStringBuilder = {
    style.append(buffer, fieldName, array, null.asInstanceOf[Boolean])
    this
  }

  /**
    * <p>Append to the {@code toString} an {@code int}
    * array.</p>
    *
    * <p>A boolean parameter controls the level of detail to show.
    * Setting {@code true} will output the array in full. Setting
    * {@code false} will output a summary, typically the size of
    * the array.</p>
    *
    * @param fieldName  the field name
    * @param array      the array to add to the {@code toString}
    * @param fullDetail {@code true} for detail, {@code false}
    *                   for summary info
    * @return this
    */
  def append(fieldName: String, array: Array[Int], fullDetail: Boolean): ToStringBuilder = {
    style.append(buffer, fieldName, array, JavaBoolean.valueOf(fullDetail))
    this
  }

  /**
    * <p>Append to the {@code toString} a {@code long}
    * value.</p>
    *
    * @param fieldName the field name
    * @param value     the value to add to the {@code toString}
    * @return this
    */
  def append(fieldName: String, value: Long): ToStringBuilder = {
    style.append(buffer, fieldName, value)
    this
  }

  /**
    * <p>Append to the {@code toString} a {@code long}
    * array.</p>
    *
    * @param fieldName the field name
    * @param array     the array to add to the {@code toString}
    * @return this
    */
  def append(fieldName: String, array: Array[Long]): ToStringBuilder = {
    style.append(buffer, fieldName, array, null.asInstanceOf[Boolean])
    this
  }

  /**
    * <p>Append to the {@code toString} a {@code long}
    * array.</p>
    *
    * <p>A boolean parameter controls the level of detail to show.
    * Setting {@code true} will output the array in full. Setting
    * {@code false} will output a summary, typically the size of
    * the array.</p>
    *
    * @param fieldName  the field name
    * @param array      the array to add to the {@code toString}
    * @param fullDetail {@code true} for detail, {@code false}
    *                   for summary info
    * @return this
    */
  def append(fieldName: String, array: Array[Long], fullDetail: Boolean): ToStringBuilder = {
    style.append(buffer, fieldName, array, JavaBoolean.valueOf(fullDetail))
    this
  }

  /**
    * <p>Append to the {@code toString} an {@code Object}
    * value.</p>
    *
    * @param fieldName the field name
    * @param obj       the value to add to the {@code toString}
    * @return this
    */
  def append(fieldName: String, obj: Any): ToStringBuilder = {
    style.append(buffer, fieldName, obj, null.asInstanceOf[Boolean])
    this
  }

  /**
    * <p>Append to the {@code toString} an {@code Object}
    * value.</p>
    *
    * @param fieldName  the field name
    * @param obj        the value to add to the {@code toString}
    * @param fullDetail {@code true} for detail,
    *                   {@code false} for summary info
    * @return this
    */
  def append(fieldName: String, obj: Any, fullDetail: Boolean): ToStringBuilder = {
    style.append(buffer, fieldName, obj, JavaBoolean.valueOf(fullDetail))
    this
  }

  /**
    * <p>Append to the {@code toString} an {@code Object}
    * array.</p>
    *
    * @param fieldName the field name
    * @param array     the array to add to the {@code toString}
    * @return this
    */
  def append(fieldName: String, array: Array[AnyRef]): ToStringBuilder = {
    style.append(buffer, fieldName, array, null.asInstanceOf[Boolean])
    this
  }

  /**
    * <p>Append to the {@code toString} an {@code Object}
    * array.</p>
    *
    * <p>A boolean parameter controls the level of detail to show.
    * Setting {@code true} will output the array in full. Setting
    * {@code false} will output a summary, typically the size of
    * the array.</p>
    *
    * @param fieldName  the field name
    * @param array      the array to add to the {@code toString}
    * @param fullDetail {@code true} for detail, {@code false}
    *                   for summary info
    * @return this
    */
  def append(fieldName: String, array: Array[AnyRef], fullDetail: Boolean): ToStringBuilder = {
    style.append(buffer, fieldName, array, JavaBoolean.valueOf(fullDetail))
    this
  }

  /**
    * <p>Append to the {@code toString} an {@code short}
    * value.</p>
    *
    * @param fieldName the field name
    * @param value     the value to add to the {@code toString}
    * @return this
    */
  def append(fieldName: String, value: Short): ToStringBuilder = {
    style.append(buffer, fieldName, value)
    this
  }

  /**
    * <p>Append to the {@code toString} a {@code short}
    * array.</p>
    *
    * @param fieldName the field name
    * @param array     the array to add to the {@code toString}
    * @return this
    */
  def append(fieldName: String, array: Array[Short]): ToStringBuilder = {
    style.append(buffer, fieldName, array, null.asInstanceOf[Boolean])
    this
  }

  /**
    * <p>Append to the {@code toString} a {@code short}
    * array.</p>
    *
    * <p>A boolean parameter controls the level of detail to show.
    * Setting {@code true} will output the array in full. Setting
    * {@code false} will output a summary, typically the size of
    * the array.
    *
    * @param fieldName  the field name
    * @param array      the array to add to the {@code toString}
    * @param fullDetail {@code true} for detail, {@code false}
    *                   for summary info
    * @return this
    */
  def append(fieldName: String, array: Array[Short], fullDetail: Boolean): ToStringBuilder = {
    style.append(buffer, fieldName, array, JavaBoolean.valueOf(fullDetail))
    this
  }

  /**
    * <p>Appends with the same format as the default <code>Object toString()
    * </code> method. Appends the class name followed by
    * {@link System# identityHashCode ( java.lang.Object )}.</p>
    *
    * @param srcObject the {@code Object} whose class name and id to output
    * @return this
    * @since 2.0
    */
  def appendAsObjectToString(srcObject: Any): ToStringBuilder = {
    ObjectUtils.identityToString(this.getStringBuffer, srcObject)
    this
  }

  /**
    * <p>Append the {@code toString} from the superclass.</p>
    *
    * <p>This method assumes that the superclass uses the same {@code ToStringStyle}
    * as this one.</p>
    *
    * <p>If {@code superToString} is {@code null}, no change is made.</p>
    *
    * @param superToString the result of {@code super.toString()}
    * @return this
    * @since 2.0
    */
  def appendSuper(superToString: String): ToStringBuilder = {
    if (superToString != null) style.appendSuper(buffer, superToString)
    this
  }

  /**
    * <p>Append the {@code toString} from another object.</p>
    *
    * <p>This method is useful where a class delegates most of the implementation of
    * its properties to another class. You can then call {@code toString()} on
    * the other class and pass the result into this method.</p>
    *
    * <pre>
    * private AnotherObject delegate;
    * private String fieldInThisClass;
    *
    * public String toString() {
    * return new ToStringBuilder(this).
    * appendToString(delegate.toString()).
    * append(fieldInThisClass).
    * toString();
    * }</pre>
    *
    * <p>This method assumes that the other object uses the same {@code ToStringStyle}
    * as this one.</p>
    *
    * <p>If the {@code toString} is {@code null}, no change is made.</p>
    *
    * @param toString the result of {@code toString()} on another object
    * @return this
    * @since 2.0
    */
  def appendToString(toString: String): ToStringBuilder = {
    if (toString != null) style.appendToString(buffer, toString)
    this
  }

  /**
    * <p>Returns the {@code Object} being output.</p>
    *
    * @return The object being output.
    * @since 2.0
    */
  def getObject: Any = `object`

  /**
    * <p>Gets the {@code StringBuffer} being populated.</p>
    *
    * @return the {@code StringBuffer} being populated
    */
  def getStringBuffer: StringBuffer = buffer

  /**
    * <p>Gets the {@code ToStringStyle} being used.</p>
    *
    * @return the {@code ToStringStyle} being used
    * @since 2.0
    */
  def getStyle: ToStringStyle = style

  /**
    * <p>Returns the built {@code toString}.</p>
    *
    * <p>This method appends the end of data indicator, and can only be called once.
    * Use {@link #getStringBuffer} to get the current string state.</p>
    *
    * <p>If the object is {@code null}, return the style's {@code nullText}</p>
    *
    * @return the String {@code toString}
    */
  override def toString: String = {
    if (this.getObject == null) this.getStringBuffer.append(this.getStyle.getNullText)
    else style.appendEnd(this.getStringBuffer, this.getObject)
    this.getStringBuffer.toString
  }

  /**
    * Returns the String that was build as an object representation. The
    * default implementation utilizes the {@link #toString ( )} implementation.
    *
    * @return the String {@code toString}
    * @see #toString()
    * @since 3.0
    */
  override def build: String = toString
}