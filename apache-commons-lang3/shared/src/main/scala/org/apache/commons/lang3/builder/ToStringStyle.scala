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

import java.lang.reflect
import java.io.Serializable
import java.util
import java.util.Objects
import org.apache.commons.lang3.ClassUtils
import org.apache.commons.lang3.ObjectUtils
import org.apache.commons.lang3.StringEscapeUtils
import org.apache.commons.lang3.StringUtils
import scala.collection.JavaConverters._

/**
  * <p>Controls {@code String} formatting for {@link ToStringBuilder}.
  * The main public interface is always via {@code ToStringBuilder}.</p>
  *
  * <p>These classes are intended to be used as {@code Singletons}.
  * There is no need to instantiate a new style each time. A program
  * will generally use one of the predefined constants on this class.
  * Alternatively, the {@link StandardToStringStyle} class can be used
  * to set the individual settings. Thus most styles can be achieved
  * without subclassing.</p>
  *
  * <p>If required, a subclass can override as many or as few of the
  * methods as it requires. Each object type (from {@code boolean}
  * to {@code long} to {@code Object} to {@code int[]}) has
  * its own methods to output it. Most have two versions, detail and summary.
  *
  * <p>For example, the detail version of the array based methods will
  * output the whole array, whereas the summary method will just output
  * the array length.</p>
  *
  * <p>If you want to format the output of certain objects, such as dates, you
  * must create a subclass and override a method.
  * </p>
  * <pre>
  * public class MyStyle extends ToStringStyle {
  * protected void appendDetail(StringBuffer buffer, String fieldName, Object value) {
  * if (value instanceof Date) {
  * value = new SimpleDateFormat("yyyy-MM-dd").format(value);
  * }
  * buffer.append(value);
  * }
  * }
  * </pre>
  *
  * @since 1.0
  */
@SuppressWarnings(Array("deprecation")) // StringEscapeUtils @SerialVersionUID(-(2587890625525655916L))
object ToStringStyle {
  /**
    * The default toString style. Using the {@code Person}
    * example from {@link ToStringBuilder}, the output would look like this:
    *
    * <pre>
    * Person@182f0db[name=John Doe,age=33,smoker=false]
    * </pre>
    */
  val DEFAULT_STYLE: ToStringStyle = new ToStringStyle.DefaultToStringStyle
  /**
    * The multi line toString style. Using the {@code Person}
    * example from {@link ToStringBuilder}, the output would look like this:
    *
    * <pre>
    * Person@182f0db[
    * name=John Doe
    * age=33
    * smoker=false
    * ]
    * </pre>
    */
  val MULTI_LINE_STYLE: ToStringStyle = new ToStringStyle.MultiLineToStringStyle
  /**
    * The no field names toString style. Using the
    * {@code Person} example from {@link ToStringBuilder}, the output
    * would look like this:
    *
    * <pre>
    * Person@182f0db[John Doe,33,false]
    * </pre>
    */
  val NO_FIELD_NAMES_STYLE: ToStringStyle = new ToStringStyle.NoFieldNameToStringStyle
  /**
    * The short prefix toString style. Using the {@code Person} example
    * from {@link ToStringBuilder}, the output would look like this:
    *
    * <pre>
    * Person[name=John Doe,age=33,smoker=false]
    * </pre>
    *
    * @since 2.1
    */
  val SHORT_PREFIX_STYLE: ToStringStyle = new ToStringStyle.ShortPrefixToStringStyle
  /**
    * The simple toString style. Using the {@code Person}
    * example from {@link ToStringBuilder}, the output would look like this:
    *
    * <pre>
    * John Doe,33,false
    * </pre>
    */
  val SIMPLE_STYLE: ToStringStyle = new ToStringStyle.SimpleToStringStyle
  /**
    * The no class name toString style. Using the {@code Person}
    * example from {@link ToStringBuilder}, the output would look like this:
    *
    * <pre>
    * [name=John Doe,age=33,smoker=false]
    * </pre>
    *
    * @since 3.4
    */
  val NO_CLASS_NAME_STYLE: ToStringStyle = new ToStringStyle.NoClassNameToStringStyle
  /**
    * The JSON toString style. Using the {@code Person} example from
    * {@link ToStringBuilder}, the output would look like this:
    *
    * <pre>
    * {"name": "John Doe", "age": 33, "smoker": true}
    * </pre>
    *
    * <strong>Note:</strong> Since field names are mandatory in JSON, this
    * ToStringStyle will throw an {@link UnsupportedOperationException} if no
    * field name is passed in while appending. Furthermore This ToStringStyle
    * will only generate valid JSON if referenced objects also produce JSON
    * when calling {@code toString()} on them.
    *
    * @since 3.4
    * @see <a href="http://json.org">json.org</a>
    */
  val JSON_STYLE: ToStringStyle = new ToStringStyle.JsonToStringStyle
  /**
    * <p>
    * A registry of objects used by {@code reflectionToString} methods
    * to detect cyclical object references and avoid infinite loops.
    * </p>
    */
  private val REGISTRY = new ThreadLocal[util.WeakHashMap[Any, Any]]

  /**
    * <p>
    * Returns the registry of objects being traversed by the {@code reflectionToString}
    * methods in the current thread.
    * </p>
    *
    * @return Set the registry of objects being traversed
    */
  private[builder] def getRegistry = REGISTRY.get

  /**
    * <p>
    * Returns {@code true} if the registry contains the given object.
    * Used by the reflection methods to avoid infinite loops.
    * </p>
    *
    * @param value
    * The object to lookup in the registry.
    * @return boolean {@code true} if the registry contains the given
    *         object.
    */
  private[builder] def isRegistered(value: Any) = {
    val m = getRegistry
    m != null && m.containsKey(value)
  }

  /**
    * <p>
    * Registers the given object. Used by the reflection methods to avoid
    * infinite loops.
    * </p>
    *
    * @param value
    * The object to register.
    */
  private[builder] def register(value: Any): Unit = {
    if (value != null) {
      val m = getRegistry
      if (m == null) REGISTRY.set(new util.WeakHashMap[Any, Any])
      getRegistry.put(value, null)
    }
    ()
  }

  /**
    * <p>
    * Unregisters the given object.
    * </p>
    *
    * <p>
    * Used by the reflection methods to avoid infinite loops.
    * </p>
    *
    * @param value
    * The object to unregister.
    */
  private[builder] def unregister(value: Any): Unit = {
    if (value != null) {
      val m = getRegistry
      if (m != null) {
        m.remove(value)
        if (m.isEmpty) REGISTRY.remove()
      }
    }
  }

  /**
    * <p>Default {@code ToStringStyle}.</p>
    *
    * <p>This is an inner class rather than using
    * {@code StandardToStringStyle} to ensure its immutability.</p>
    */
  @SerialVersionUID(1L)
  final private class DefaultToStringStyle private[builder]()

  /**
    * <p>Constructor.</p>
    *
    * <p>Use the static constant rather than instantiating.</p>
    */
    extends ToStringStyle {
    /**
      * <p>Ensure {@code Singleton} after serialization.</p>
      *
      * @return the singleton
      */
    private def readResolve = DEFAULT_STYLE
  }

  /**
    * <p>{@code ToStringStyle} that does not print out
    * the field names.</p>
    *
    * <p>This is an inner class rather than using
    * {@code StandardToStringStyle} to ensure its immutability.
    */
  @SerialVersionUID(1L)
  final private class NoFieldNameToStringStyle private[builder]() extends ToStringStyle {
    this.setUseFieldNames(false)

    private def readResolve = NO_FIELD_NAMES_STYLE
  }

  /**
    * <p>{@code ToStringStyle} that prints out the short
    * class name and no identity hashcode.</p>
    *
    * <p>This is an inner class rather than using
    * {@code StandardToStringStyle} to ensure its immutability.</p>
    */
  @SerialVersionUID(1L)
  final private class ShortPrefixToStringStyle private[builder]() extends ToStringStyle {
    this.setUseShortClassName(true)
    this.setUseIdentityHashCode(false)

    /**
      * <p>Ensure <code>Singleton</ode> after serialization.</p>
      *
      * @return the singleton
      */
    private def readResolve = SHORT_PREFIX_STYLE
  }

  /**
    * <p>{@code ToStringStyle} that does not print out the
    * classname, identity hashcode, content start or field name.</p>
    *
    * <p>This is an inner class rather than using
    * {@code StandardToStringStyle} to ensure its immutability.</p>
    */
  @SerialVersionUID(1L)
  final private class SimpleToStringStyle private[builder]() extends ToStringStyle {
    this.setUseClassName(false)
    this.setUseIdentityHashCode(false)
    this.setUseFieldNames(false)
    this.setContentStart(StringUtils.EMPTY)
    this.setContentEnd(StringUtils.EMPTY)

    private def readResolve = SIMPLE_STYLE
  }

  /**
    * <p>{@code ToStringStyle} that outputs on multiple lines.</p>
    *
    * <p>This is an inner class rather than using
    * {@code StandardToStringStyle} to ensure its immutability.</p>
    */
  @SerialVersionUID(1L)
  final private class MultiLineToStringStyle private[builder]() extends ToStringStyle {
    this.setContentStart("[")
    this.setFieldSeparator(System.lineSeparator + "  ")
    this.setFieldSeparatorAtStart(true)
    this.setContentEnd(System.lineSeparator + "]")

    private def readResolve = MULTI_LINE_STYLE
  }

  /**
    * <p>{@code ToStringStyle} that does not print out the classname
    * and identity hash code but prints content start and field names.</p>
    *
    * <p>This is an inner class rather than using
    * {@code StandardToStringStyle} to ensure its immutability.</p>
    */
  @SerialVersionUID(1L)
  final private class NoClassNameToStringStyle private[builder]() extends ToStringStyle {
    this.setUseClassName(false)
    this.setUseIdentityHashCode(false)

    private def readResolve = NO_CLASS_NAME_STYLE
  }

  /**
    * <p>
    * {@code ToStringStyle} that outputs with JSON format.
    * </p>
    *
    * <p>
    * This is an inner class rather than using
    * {@code StandardToStringStyle} to ensure its immutability.
    * </p>
    *
    * @since 3.4
    * @see <a href="http://json.org">json.org</a>
    */
  @SerialVersionUID(1L)
  private object JsonToStringStyle {
    private val FIELD_NAME_QUOTE = "\""
  }

  @SerialVersionUID(1L)
  final private class JsonToStringStyle private[builder]()

  /**
    * <p>
    * Constructor.
    * </p>
    *
    * <p>
    * Use the static constant rather than instantiating.
    * </p>
    */
    extends ToStringStyle {
    this.setUseClassName(false)
    this.setUseIdentityHashCode(false)
    this.setContentStart("{")
    this.setContentEnd("}")
    this.setArrayStart("[")
    this.setArrayEnd("]")
    this.setFieldSeparator(",")
    this.setFieldNameValueSeparator(":")
    this.setNullText("null")
    this.setSummaryObjectStartText("\"<")
    this.setSummaryObjectEndText(">\"")
    this.setSizeStartText("\"<size=")
    this.setSizeEndText(">\"")

    override def append(buffer: StringBuffer, fieldName: String, array: Array[AnyRef], fullDetail: Boolean): Unit = {
      if (fieldName == null) throw new UnsupportedOperationException("Field names are mandatory when using JsonToStringStyle")
      if (!isFullDetail(fullDetail)) throw new UnsupportedOperationException("FullDetail must be true when using JsonToStringStyle")
      super.append(buffer, fieldName, array, fullDetail)
    }

    override def append(buffer: StringBuffer, fieldName: String, array: Array[Long], fullDetail: Boolean): Unit = {
      if (fieldName == null) throw new UnsupportedOperationException("Field names are mandatory when using JsonToStringStyle")
      if (!isFullDetail(fullDetail)) throw new UnsupportedOperationException("FullDetail must be true when using JsonToStringStyle")
      super.append(buffer, fieldName, array, fullDetail)
    }

    override def append(buffer: StringBuffer, fieldName: String, array: Array[Int], fullDetail: Boolean): Unit = {
      if (fieldName == null) throw new UnsupportedOperationException("Field names are mandatory when using JsonToStringStyle")
      if (!isFullDetail(fullDetail)) throw new UnsupportedOperationException("FullDetail must be true when using JsonToStringStyle")
      super.append(buffer, fieldName, array, fullDetail)
    }

    override def append(buffer: StringBuffer, fieldName: String, array: Array[Short], fullDetail: Boolean): Unit = {
      if (fieldName == null) throw new UnsupportedOperationException("Field names are mandatory when using JsonToStringStyle")
      if (!isFullDetail(fullDetail)) throw new UnsupportedOperationException("FullDetail must be true when using JsonToStringStyle")
      super.append(buffer, fieldName, array, fullDetail)
    }

    override def append(buffer: StringBuffer, fieldName: String, array: Array[Byte], fullDetail: Boolean): Unit = {
      if (fieldName == null) throw new UnsupportedOperationException("Field names are mandatory when using JsonToStringStyle")
      if (!isFullDetail(fullDetail)) throw new UnsupportedOperationException("FullDetail must be true when using JsonToStringStyle")
      super.append(buffer, fieldName, array, fullDetail)
    }

    override def append(buffer: StringBuffer, fieldName: String, array: Array[Char], fullDetail: Boolean): Unit = {
      if (fieldName == null) throw new UnsupportedOperationException("Field names are mandatory when using JsonToStringStyle")
      if (!isFullDetail(fullDetail)) throw new UnsupportedOperationException("FullDetail must be true when using JsonToStringStyle")
      super.append(buffer, fieldName, array, fullDetail)
    }

    override def append(buffer: StringBuffer, fieldName: String, array: Array[Double], fullDetail: Boolean): Unit = {
      if (fieldName == null) throw new UnsupportedOperationException("Field names are mandatory when using JsonToStringStyle")
      if (!isFullDetail(fullDetail)) throw new UnsupportedOperationException("FullDetail must be true when using JsonToStringStyle")
      super.append(buffer, fieldName, array, fullDetail)
    }

    override def append(buffer: StringBuffer, fieldName: String, array: Array[Float], fullDetail: Boolean): Unit = {
      if (fieldName == null) throw new UnsupportedOperationException("Field names are mandatory when using JsonToStringStyle")
      if (!isFullDetail(fullDetail)) throw new UnsupportedOperationException("FullDetail must be true when using JsonToStringStyle")
      super.append(buffer, fieldName, array, fullDetail)
    }

    override def append(buffer: StringBuffer, fieldName: String, array: Array[Boolean], fullDetail: Boolean): Unit = {
      if (fieldName == null) throw new UnsupportedOperationException("Field names are mandatory when using JsonToStringStyle")
      if (!isFullDetail(fullDetail)) throw new UnsupportedOperationException("FullDetail must be true when using JsonToStringStyle")
      super.append(buffer, fieldName, array, fullDetail)
    }

    override def append(buffer: StringBuffer, fieldName: String, value: AnyRef, fullDetail: Boolean): Unit = {
      if (fieldName == null) throw new UnsupportedOperationException("Field names are mandatory when using JsonToStringStyle")
      if (!isFullDetail(fullDetail)) throw new UnsupportedOperationException("FullDetail must be true when using JsonToStringStyle")
      super.append(buffer, fieldName, value, fullDetail)
    }

    override protected def appendDetail(buffer: StringBuffer, fieldName: String, value: Char): Unit = {
      appendValueAsString(buffer, String.valueOf(value))
    }

    override protected def appendDetail(buffer: StringBuffer, fieldName: String, value: AnyRef): Unit = {
      if (value == null) {
        appendNullText(buffer, fieldName)
        return
      }
      if (value.isInstanceOf[String] || value.isInstanceOf[Character]) {
        appendValueAsString(buffer, value.toString)
        return
      }
      if (value.isInstanceOf[Number] || value.isInstanceOf[Boolean]) {
        buffer.append(value)
        return
      }
      val valueAsString = value.toString
      if (isJsonObject(valueAsString) || isJsonArray(valueAsString)) {
        buffer.append(value)
        return
      }
      appendDetail(buffer, fieldName, valueAsString)
    }

    override protected def appendDetail(buffer: StringBuffer, fieldName: String, map: util.Map[AnyRef, AnyRef]): Unit = {
      if (map != null && !map.isEmpty) {
        buffer.append(getContentStart)
        var firstItem = true

        for (entry <- map.entrySet.asScala) {
          val keyStr = Objects.toString(entry.getKey, null)
          if (keyStr != null) {
            if (firstItem) firstItem = false
            else appendFieldEnd(buffer, keyStr)
            appendFieldStart(buffer, keyStr)
            val value = entry.getValue
            if (value == null) appendNullText(buffer, keyStr)
            else appendInternal(buffer, keyStr, value, true)
          }
        }
        buffer.append(getContentEnd)
        return
      }
      buffer.append(map)
      ()
    }

    private def isJsonArray(valueAsString: String) = valueAsString.startsWith(getArrayStart) && valueAsString.endsWith(getArrayEnd)

    private def isJsonObject(valueAsString: String) = valueAsString.startsWith(getContentStart) && valueAsString.endsWith(getContentEnd)

    /**
      * Appends the given String enclosed in double-quotes to the given StringBuffer.
      *
      * @param buffer the StringBuffer to append the value to.
      * @param value  the value to append.
      */
    private def appendValueAsString(buffer: StringBuffer, value: String): Unit = {
      buffer.append('"').append(StringEscapeUtils.escapeJson(value)).append('"')
      ()
    }

    override protected def appendFieldStart(buffer: StringBuffer, fieldName: String): Unit = {
      if (fieldName == null) throw new UnsupportedOperationException("Field names are mandatory when using JsonToStringStyle")
      super.appendFieldStart(buffer, JsonToStringStyle.FIELD_NAME_QUOTE + StringEscapeUtils.escapeJson(fieldName) + JsonToStringStyle.FIELD_NAME_QUOTE)
    }

    /**
      * <p>
      * Ensure {@code Singleton} after serialization.
      * </p>
      *
      * @return the singleton
      */
    private def readResolve = JSON_STYLE
  }

}

@SuppressWarnings(Array("deprecation"))
@SerialVersionUID(-2587890625525655916L)
abstract class ToStringStyle protected()

/**
  * <p>Constructor.</p>
  */
  extends Serializable {
  /**
    * Whether to use the field names, the default is {@code true}.
    */
  private var useFieldNames = true
  /**
    * Whether to use the class name, the default is {@code true}.
    */
  private var useClassName = true
  /**
    * Whether to use short class names, the default is {@code false}.
    */
  private var useShortClassName = false
  /**
    * Whether to use the identity hash code, the default is {@code true}.
    */
  private var useIdentityHashCode = true
  /**
    * The content start {@code '['}.
    */
  private var contentStart = "["
  /**
    * The content end {@code ']'}.
    */
  private var contentEnd = "]"
  /**
    * The field name value separator {@code '='}.
    */
  private var fieldNameValueSeparator = "="
  /**
    * Whether the field separator should be added before any other fields.
    */
  private var fieldSeparatorAtStart = false
  /**
    * Whether the field separator should be added after any other fields.
    */
  private var fieldSeparatorAtEnd = false
  /**
    * The field separator {@code ','}.
    */
  private var fieldSeparator = ","
  /**
    * The array start <code>'{'</code>.
    */
  private var arrayStart = "{"
  /**
    * The array separator {@code ','}.
    */
  private var arraySeparator = ","
  /**
    * The detail for array content.
    */
  private var arrayContentDetail = true
  /**
    * The array end {@code '}'}.
    */
  private var arrayEnd = "}"
  /**
    * The value to use when fullDetail is {@code null},
    * the default value is {@code true}.
    */
  private var defaultFullDetail = true
  /**
    * The {@code null} text {@code '&lt;null&gt;'}.
    */
  private var nullText = "<null>"
  /**
    * The summary size text start {@code '&lt;size'}.
    */
  private var sizeStartText = "<size="
  /**
    * The summary size text start {@code '&gt;'}.
    */
  private var sizeEndText = ">"
  /**
    * The summary object text start {@code '&lt;'}.
    */
  private var summaryObjectStartText = "<"
  /**
    * The summary object text start {@code '&gt;'}.
    */
  private var summaryObjectEndText = ">"

  /**
    * <p>Append to the {@code toString} the superclass toString.</p>
    * <p>NOTE: It assumes that the toString has been created from the same ToStringStyle. </p>
    *
    * <p>A {@code null} {@code superToString} is ignored.</p>
    *
    * @param buffer        the {@code StringBuffer} to populate
    * @param superToString the {@code super.toString()}
    * @since 2.0
    */
  def appendSuper(buffer: StringBuffer, superToString: String): Unit = {
    appendToString(buffer, superToString)
  }

  /**
    * <p>Append to the {@code toString} another toString.</p>
    * <p>NOTE: It assumes that the toString has been created from the same ToStringStyle. </p>
    *
    * <p>A {@code null} {@code toString} is ignored.</p>
    *
    * @param buffer   the {@code StringBuffer} to populate
    * @param toString the additional {@code toString}
    * @since 2.0
    */
  def appendToString(buffer: StringBuffer, toString: String): Unit = {
    if (toString != null) {
      val pos1 = toString.indexOf(contentStart) + contentStart.length
      val pos2 = toString.lastIndexOf(contentEnd)
      if (pos1 != pos2 && pos1 >= 0 && pos2 >= 0) {
        if (fieldSeparatorAtStart) removeLastFieldSeparator(buffer)
        buffer.append(toString, pos1, pos2)
        appendFieldSeparator(buffer)
      }
    }
  }

  /**
    * <p>Append to the {@code toString} the start of data indicator.</p>
    *
    * @param buffer the {@code StringBuffer} to populate
    * @param object the {@code Object} to build a {@code toString} for
    */
  def appendStart(buffer: StringBuffer, `object`: Any): Unit = {
    if (`object` != null) {
      appendClassName(buffer, `object`)
      appendIdentityHashCode(buffer, `object`)
      appendContentStart(buffer)
      if (fieldSeparatorAtStart) appendFieldSeparator(buffer)
    }
  }

  /**
    * <p>Append to the {@code toString} the end of data indicator.</p>
    *
    * @param buffer the {@code StringBuffer} to populate
    * @param object the {@code Object} to build a
    *               {@code toString} for.
    */
  def appendEnd(buffer: StringBuffer, `object`: Any): Unit = {
    if (!this.fieldSeparatorAtEnd) removeLastFieldSeparator(buffer)
    appendContentEnd(buffer)
    ToStringStyle.unregister(`object`)
  }

  /**
    * <p>Remove the last field separator from the buffer.</p>
    *
    * @param buffer the {@code StringBuffer} to populate
    * @since 2.0
    */
  protected def removeLastFieldSeparator(buffer: StringBuffer): Unit = {
    if (StringUtils.endsWith(buffer, fieldSeparator)) buffer.setLength(buffer.length - fieldSeparator.length)
  }

  /**
    * <p>Append to the {@code toString} an {@code Object}
    * value, printing the full {@code toString} of the
    * {@code Object} passed in.</p>
    *
    * @param buffer     the {@code StringBuffer} to populate
    * @param fieldName  the field name
    * @param value      the value to add to the {@code toString}
    * @param fullDetail {@code true} for detail, {@code false}
    *                   for summary info, {@code null} for style decides
    */
  def append(buffer: StringBuffer, fieldName: String, value: AnyRef, fullDetail: Boolean): Unit = {
    appendFieldStart(buffer, fieldName)
    if (value == null) appendNullText(buffer, fieldName)
    else appendInternal(buffer, fieldName, value, isFullDetail(fullDetail))
    appendFieldEnd(buffer, fieldName)
  }

  /**
    * <p>Append to the {@code toString} an {@code Object},
    * correctly interpreting its type.</p>
    *
    * <p>This method performs the main lookup by Class type to correctly
    * route arrays, {@code Collections}, {@code Maps} and
    * {@code Objects} to the appropriate method.</p>
    *
    * <p>Either detail or summary views can be specified.</p>
    *
    * <p>If a cycle is detected, an object will be appended with the
    * {@code Object.toString()} format.</p>
    *
    * @param buffer    the {@code StringBuffer} to populate
    * @param fieldName the field name, typically not used as already appended
    * @param value     the value to add to the {@code toString},
    *                  not {@code null}
    * @param detail    output detail or not
    */
  protected def appendInternal(buffer: StringBuffer, fieldName: String, value: AnyRef, detail: Boolean): Unit = {
    if (ToStringStyle.isRegistered(value) && !(value.isInstanceOf[Number] || value.isInstanceOf[Boolean] || value.isInstanceOf[Character])) {
      appendCyclicObject(buffer, fieldName, value)
      return
    }
    ToStringStyle.register(value)
    try if (value.isInstanceOf[util.Collection[_]]) if (detail) appendDetail(buffer, fieldName, value.asInstanceOf[util.Collection[_]])
    else appendSummarySize(buffer, fieldName, value.asInstanceOf[util.Collection[_]].size)
    else if (value.isInstanceOf[util.Map[_, _]]) if (detail) appendDetail(buffer, fieldName, value.asInstanceOf[util.Map[_, _]])
    else appendSummarySize(buffer, fieldName, value.asInstanceOf[util.Map[_, _]].size)
    else if (value.isInstanceOf[Array[Long]]) if (detail) appendDetail(buffer, fieldName, value.asInstanceOf[Array[Long]])
    else appendSummary(buffer, fieldName, value.asInstanceOf[Array[Long]])
    else if (value.isInstanceOf[Array[Int]]) if (detail) appendDetail(buffer, fieldName, value.asInstanceOf[Array[Int]])
    else appendSummary(buffer, fieldName, value.asInstanceOf[Array[Int]])
    else if (value.isInstanceOf[Array[Short]]) if (detail) appendDetail(buffer, fieldName, value.asInstanceOf[Array[Short]])
    else appendSummary(buffer, fieldName, value.asInstanceOf[Array[Short]])
    else if (value.isInstanceOf[Array[Byte]]) if (detail) appendDetail(buffer, fieldName, value.asInstanceOf[Array[Byte]])
    else appendSummary(buffer, fieldName, value.asInstanceOf[Array[Byte]])
    else if (value.isInstanceOf[Array[Char]]) if (detail) appendDetail(buffer, fieldName, value.asInstanceOf[Array[Char]])
    else appendSummary(buffer, fieldName, value.asInstanceOf[Array[Char]])
    else if (value.isInstanceOf[Array[Double]]) if (detail) appendDetail(buffer, fieldName, value.asInstanceOf[Array[Double]])
    else appendSummary(buffer, fieldName, value.asInstanceOf[Array[Double]])
    else if (value.isInstanceOf[Array[Float]]) if (detail) appendDetail(buffer, fieldName, value.asInstanceOf[Array[Float]])
    else appendSummary(buffer, fieldName, value.asInstanceOf[Array[Float]])
    else if (value.isInstanceOf[Array[Boolean]]) if (detail) appendDetail(buffer, fieldName, value.asInstanceOf[Array[Boolean]])
    else appendSummary(buffer, fieldName, value.asInstanceOf[Array[Boolean]])
    else if (value.getClass.isArray) if (detail) appendDetail(buffer, fieldName, value.asInstanceOf[Array[AnyRef]])
    else appendSummary(buffer, fieldName, value.asInstanceOf[Array[AnyRef]])
    else if (detail) appendDetail(buffer, fieldName, value)
    else appendSummary(buffer, fieldName, value)
    finally ToStringStyle.unregister(value)
  }

  /**
    * <p>Append to the {@code toString} an {@code Object}
    * value that has been detected to participate in a cycle. This
    * implementation will print the standard string value of the value.</p>
    *
    * @param buffer    the {@code StringBuffer} to populate
    * @param fieldName the field name, typically not used as already appended
    * @param value     the value to add to the {@code toString},
    *                  not {@code null}
    * @since 2.2
    */
  protected def appendCyclicObject(buffer: StringBuffer, fieldName: String, value: AnyRef): Unit = {
    ObjectUtils.identityToString(buffer, value)
  }

  /**
    * <p>Append to the {@code toString} an {@code Object}
    * value, printing the full detail of the {@code Object}.</p>
    *
    * @param buffer    the {@code StringBuffer} to populate
    * @param fieldName the field name, typically not used as already appended
    * @param value     the value to add to the {@code toString},
    *                  not {@code null}
    */
  protected def appendDetail(buffer: StringBuffer, fieldName: String, value: AnyRef): Unit = {
    buffer.append(value)
    ()
  }

  /**
    * <p>Append to the {@code toString} a {@code Collection}.</p>
    *
    * @param buffer    the {@code StringBuffer} to populate
    * @param fieldName the field name, typically not used as already appended
    * @param coll      the {@code Collection} to add to the
    *                  {@code toString}, not {@code null}
    */
  protected def appendDetail(buffer: StringBuffer, fieldName: String, coll: util.Collection[AnyRef]): Unit = {
    if (coll != null && !coll.isEmpty) {
      buffer.append(arrayStart)
      var i = 0

      for (item <- coll.asScala) {
        appendDetail(buffer, fieldName, {
          i += 1; i - 1
        }, item)
      }
      buffer.append(arrayEnd)
      return
    }
    buffer.append(coll)
    ()
  }

  /**
    * <p>Append to the {@code toString} a {@code Map}.</p>
    *
    * @param buffer    the {@code StringBuffer} to populate
    * @param fieldName the field name, typically not used as already appended
    * @param map       the {@code Map} to add to the {@code toString},
    *                  not {@code null}
    */
  protected def appendDetail(buffer: StringBuffer, fieldName: String, map: util.Map[AnyRef, AnyRef]): Unit = {
    buffer.append(map)
    ()
  }

  /**
    * <p>Append to the {@code toString} an {@code Object}
    * value, printing a summary of the {@code Object}.</P>
    *
    * @param buffer    the {@code StringBuffer} to populate
    * @param fieldName the field name, typically not used as already appended
    * @param value     the value to add to the {@code toString},
    *                  not {@code null}
    */
  protected def appendSummary(buffer: StringBuffer, fieldName: String, value: AnyRef): Unit = {
    buffer.append(summaryObjectStartText)
    buffer.append(getShortClassName(value.getClass))
    buffer.append(summaryObjectEndText)
    ()
  }

  /**
    * <p>Append to the {@code toString} a {@code long}
    * value.</p>
    *
    * @param buffer    the {@code StringBuffer} to populate
    * @param fieldName the field name
    * @param value     the value to add to the {@code toString}
    */
  def append(buffer: StringBuffer, fieldName: String, value: Long): Unit = {
    appendFieldStart(buffer, fieldName)
    appendDetail(buffer, fieldName, value)
    appendFieldEnd(buffer, fieldName)
  }

  /**
    * <p>Append to the {@code toString} a {@code long}
    * value.</p>
    *
    * @param buffer    the {@code StringBuffer} to populate
    * @param fieldName the field name, typically not used as already appended
    * @param value     the value to add to the {@code toString}
    */
  protected def appendDetail(buffer: StringBuffer, fieldName: String, value: Long): Unit = {
    buffer.append(value)
    ()
  }

  /**
    * <p>Append to the {@code toString} an {@code int}
    * value.</p>
    *
    * @param buffer    the {@code StringBuffer} to populate
    * @param fieldName the field name
    * @param value     the value to add to the {@code toString}
    */
  def append(buffer: StringBuffer, fieldName: String, value: Int): Unit = {
    appendFieldStart(buffer, fieldName)
    appendDetail(buffer, fieldName, value)
    appendFieldEnd(buffer, fieldName)
  }

  /**
    * <p>Append to the {@code toString} an {@code int}
    * value.</p>
    *
    * @param buffer    the {@code StringBuffer} to populate
    * @param fieldName the field name, typically not used as already appended
    * @param value     the value to add to the {@code toString}
    */
  protected def appendDetail(buffer: StringBuffer, fieldName: String, value: Int): Unit = {
    buffer.append(value)
    ()
  }

  /**
    * <p>Append to the {@code toString} a {@code short}
    * value.</p>
    *
    * @param buffer    the {@code StringBuffer} to populate
    * @param fieldName the field name
    * @param value     the value to add to the {@code toString}
    */
  def append(buffer: StringBuffer, fieldName: String, value: Short): Unit = {
    appendFieldStart(buffer, fieldName)
    appendDetail(buffer, fieldName, value)
    appendFieldEnd(buffer, fieldName)
  }

  /**
    * <p>Append to the {@code toString} a {@code short}
    * value.</p>
    *
    * @param buffer    the {@code StringBuffer} to populate
    * @param fieldName the field name, typically not used as already appended
    * @param value     the value to add to the {@code toString}
    */
  protected def appendDetail(buffer: StringBuffer, fieldName: String, value: Short): Unit = {
    buffer.append(value.toInt)
    ()
  }

  /**
    * <p>Append to the {@code toString} a {@code byte}
    * value.</p>
    *
    * @param buffer    the {@code StringBuffer} to populate
    * @param fieldName the field name
    * @param value     the value to add to the {@code toString}
    */
  def append(buffer: StringBuffer, fieldName: String, value: Byte): Unit = {
    appendFieldStart(buffer, fieldName)
    appendDetail(buffer, fieldName, value)
    appendFieldEnd(buffer, fieldName)
  }

  /**
    * <p>Append to the {@code toString} a {@code byte}
    * value.</p>
    *
    * @param buffer    the {@code StringBuffer} to populate
    * @param fieldName the field name, typically not used as already appended
    * @param value     the value to add to the {@code toString}
    */
  protected def appendDetail(buffer: StringBuffer, fieldName: String, value: Byte): Unit = {
    buffer.append(value.toInt)
    ()
  }

  /**
    * <p>Append to the {@code toString} a {@code char}
    * value.</p>
    *
    * @param buffer    the {@code StringBuffer} to populate
    * @param fieldName the field name
    * @param value     the value to add to the {@code toString}
    */
  def append(buffer: StringBuffer, fieldName: String, value: Char): Unit = {
    appendFieldStart(buffer, fieldName)
    appendDetail(buffer, fieldName, value)
    appendFieldEnd(buffer, fieldName)
  }

  /**
    * <p>Append to the {@code toString} a {@code char}
    * value.</p>
    *
    * @param buffer    the {@code StringBuffer} to populate
    * @param fieldName the field name, typically not used as already appended
    * @param value     the value to add to the {@code toString}
    */
  protected def appendDetail(buffer: StringBuffer, fieldName: String, value: Char): Unit = {
    buffer.append(value)
    ()
  }

  /**
    * <p>Append to the {@code toString} a {@code double}
    * value.</p>
    *
    * @param buffer    the {@code StringBuffer} to populate
    * @param fieldName the field name
    * @param value     the value to add to the {@code toString}
    */
  def append(buffer: StringBuffer, fieldName: String, value: Double): Unit = {
    appendFieldStart(buffer, fieldName)
    appendDetail(buffer, fieldName, value)
    appendFieldEnd(buffer, fieldName)
  }

  /**
    * <p>Append to the {@code toString} a {@code double}
    * value.</p>
    *
    * @param buffer    the {@code StringBuffer} to populate
    * @param fieldName the field name, typically not used as already appended
    * @param value     the value to add to the {@code toString}
    */
  protected def appendDetail(buffer: StringBuffer, fieldName: String, value: Double): Unit = {
    buffer.append(value)
    ()
  }

  /**
    * <p>Append to the {@code toString} a {@code float}
    * value.</p>
    *
    * @param buffer    the {@code StringBuffer} to populate
    * @param fieldName the field name
    * @param value     the value to add to the {@code toString}
    */
  def append(buffer: StringBuffer, fieldName: String, value: Float): Unit = {
    appendFieldStart(buffer, fieldName)
    appendDetail(buffer, fieldName, value)
    appendFieldEnd(buffer, fieldName)
  }

  /**
    * <p>Append to the {@code toString} a {@code float}
    * value.</p>
    *
    * @param buffer    the {@code StringBuffer} to populate
    * @param fieldName the field name, typically not used as already appended
    * @param value     the value to add to the {@code toString}
    */
  protected def appendDetail(buffer: StringBuffer, fieldName: String, value: Float): Unit = {
    buffer.append(value)
    ()
  }

  /**
    * <p>Append to the {@code toString} a {@code boolean}
    * value.</p>
    *
    * @param buffer    the {@code StringBuffer} to populate
    * @param fieldName the field name
    * @param value     the value to add to the {@code toString}
    */
  def append(buffer: StringBuffer, fieldName: String, value: Boolean): Unit = {
    appendFieldStart(buffer, fieldName)
    appendDetail(buffer, fieldName, value)
    appendFieldEnd(buffer, fieldName)
  }

  /**
    * <p>Append to the {@code toString} a {@code boolean}
    * value.</p>
    *
    * @param buffer    the {@code StringBuffer} to populate
    * @param fieldName the field name, typically not used as already appended
    * @param value     the value to add to the {@code toString}
    */
  protected def appendDetail(buffer: StringBuffer, fieldName: String, value: Boolean): Unit = {
    buffer.append(value)
    ()
  }

  /**
    * <p>Append to the {@code toString} an {@code Object}
    * array.</p>
    *
    * @param buffer     the {@code StringBuffer} to populate
    * @param fieldName  the field name
    * @param array      the array to add to the toString
    * @param fullDetail {@code true} for detail, {@code false}
    *                   for summary info, {@code null} for style decides
    */
  def append(buffer: StringBuffer, fieldName: String, array: Array[AnyRef], fullDetail: Boolean): Unit = {
    appendFieldStart(buffer, fieldName)
    if (array == null) appendNullText(buffer, fieldName)
    else if (isFullDetail(fullDetail)) appendDetail(buffer, fieldName, array)
    else appendSummary(buffer, fieldName, array)
    appendFieldEnd(buffer, fieldName)
  }

  /**
    * <p>Append to the {@code toString} the detail of an
    * {@code Object} array.</p>
    *
    * @param buffer    the {@code StringBuffer} to populate
    * @param fieldName the field name, typically not used as already appended
    * @param array     the array to add to the {@code toString},
    *                  not {@code null}
    */
  protected def appendDetail(buffer: StringBuffer, fieldName: String, array: Array[AnyRef]): Unit = {
    buffer.append(arrayStart)

    for (i <- 0 until array.length) {
      val item = array(i)
      appendDetail(buffer, fieldName, i, item)
    }

    buffer.append(arrayEnd)
    ()
  }

  /**
    * <p>Append to the {@code toString} the detail of an
    * {@code Object} array item.</p>
    *
    * @param buffer    the {@code StringBuffer} to populate
    * @param fieldName the field name, typically not used as already appended
    * @param i         the array item index to add
    * @param item      the array item to add
    * @since 3.11
    */
  protected def appendDetail(buffer: StringBuffer, fieldName: String, i: Int, item: AnyRef): Unit = {
    if (i > 0) buffer.append(arraySeparator)
    if (item == null) appendNullText(buffer, fieldName)
    else appendInternal(buffer, fieldName, item, arrayContentDetail)
  }

  /**
    * <p>Append to the {@code toString} the detail of an array type.</p>
    *
    * @param buffer    the {@code StringBuffer} to populate
    * @param fieldName the field name, typically not used as already appended
    * @param array     the array to add to the {@code toString},
    *                  not {@code null}
    * @since 2.0
    */
  protected[lang3] def reflectionAppendArrayDetail(buffer: StringBuffer, fieldName: String, array: AnyRef): Unit = {
    buffer.append(arrayStart)
    val length = reflect.Array.getLength(array)
    for (i <- 0 until length) {
      val item = reflect.Array.get(array, i)
      appendDetail(buffer, fieldName, i, item)
    }
    buffer.append(arrayEnd)
    ()
  }

  /**
    * <p>Append to the {@code toString} a summary of an
    * {@code Object} array.</p>
    *
    * @param buffer    the {@code StringBuffer} to populate
    * @param fieldName the field name, typically not used as already appended
    * @param array     the array to add to the {@code toString},
    *                  not {@code null}
    */
  protected def appendSummary(buffer: StringBuffer, fieldName: String, array: Array[AnyRef]): Unit = {
    appendSummarySize(buffer, fieldName, array.length)
  }

  /**
    * <p>Append to the {@code toString} a {@code long}
    * array.</p>
    *
    * @param buffer     the {@code StringBuffer} to populate
    * @param fieldName  the field name
    * @param array      the array to add to the {@code toString}
    * @param fullDetail {@code true} for detail, {@code false}
    *                   for summary info, {@code null} for style decides
    */
  def append(buffer: StringBuffer, fieldName: String, array: Array[Long], fullDetail: Boolean): Unit = {
    appendFieldStart(buffer, fieldName)
    if (array == null) appendNullText(buffer, fieldName)
    else if (isFullDetail(fullDetail)) appendDetail(buffer, fieldName, array)
    else appendSummary(buffer, fieldName, array)
    appendFieldEnd(buffer, fieldName)
  }

  /**
    * <p>Append to the {@code toString} the detail of a
    * {@code long} array.</p>
    *
    * @param buffer    the {@code StringBuffer} to populate
    * @param fieldName the field name, typically not used as already appended
    * @param array     the array to add to the {@code toString},
    *                  not {@code null}
    */
  protected def appendDetail(buffer: StringBuffer, fieldName: String, array: Array[Long]): Unit = {
    buffer.append(arrayStart)
    for (i <- 0 until array.length) {
      if (i > 0) buffer.append(arraySeparator)
      appendDetail(buffer, fieldName, array(i))
    }
    buffer.append(arrayEnd)
    ()
  }

  /**
    * <p>Append to the {@code toString} a summary of a
    * {@code long} array.</p>
    *
    * @param buffer    the {@code StringBuffer} to populate
    * @param fieldName the field name, typically not used as already appended
    * @param array     the array to add to the {@code toString},
    *                  not {@code null}
    */
  protected def appendSummary(buffer: StringBuffer, fieldName: String, array: Array[Long]): Unit = {
    appendSummarySize(buffer, fieldName, array.length)
  }

  /**
    * <p>Append to the {@code toString} an {@code int}
    * array.</p>
    *
    * @param buffer     the {@code StringBuffer} to populate
    * @param fieldName  the field name
    * @param array      the array to add to the {@code toString}
    * @param fullDetail {@code true} for detail, {@code false}
    *                   for summary info, {@code null} for style decides
    */
  def append(buffer: StringBuffer, fieldName: String, array: Array[Int], fullDetail: Boolean): Unit = {
    appendFieldStart(buffer, fieldName)
    if (array == null) appendNullText(buffer, fieldName)
    else if (isFullDetail(fullDetail)) appendDetail(buffer, fieldName, array)
    else appendSummary(buffer, fieldName, array)
    appendFieldEnd(buffer, fieldName)
  }

  /**
    * <p>Append to the {@code toString} the detail of an
    * {@code int} array.</p>
    *
    * @param buffer    the {@code StringBuffer} to populate
    * @param fieldName the field name, typically not used as already appended
    * @param array     the array to add to the {@code toString},
    *                  not {@code null}
    */
  protected def appendDetail(buffer: StringBuffer, fieldName: String, array: Array[Int]): Unit = {
    buffer.append(arrayStart)
    for (i <- 0 until array.length) {
      if (i > 0) buffer.append(arraySeparator)
      appendDetail(buffer, fieldName, array(i))
    }
    buffer.append(arrayEnd)
    ()
  }

  /**
    * <p>Append to the {@code toString} a summary of an
    * {@code int} array.</p>
    *
    * @param buffer    the {@code StringBuffer} to populate
    * @param fieldName the field name, typically not used as already appended
    * @param array     the array to add to the {@code toString},
    *                  not {@code null}
    */
  protected def appendSummary(buffer: StringBuffer, fieldName: String, array: Array[Int]): Unit = {
    appendSummarySize(buffer, fieldName, array.length)
  }

  /**
    * <p>Append to the {@code toString} a {@code short}
    * array.</p>
    *
    * @param buffer     the {@code StringBuffer} to populate
    * @param fieldName  the field name
    * @param array      the array to add to the {@code toString}
    * @param fullDetail {@code true} for detail, {@code false}
    *                   for summary info, {@code null} for style decides
    */
  def append(buffer: StringBuffer, fieldName: String, array: Array[Short], fullDetail: Boolean): Unit = {
    appendFieldStart(buffer, fieldName)
    if (array == null) appendNullText(buffer, fieldName)
    else if (isFullDetail(fullDetail)) appendDetail(buffer, fieldName, array)
    else appendSummary(buffer, fieldName, array)
    appendFieldEnd(buffer, fieldName)
  }

  /**
    * <p>Append to the {@code toString} the detail of a
    * {@code short} array.</p>
    *
    * @param buffer    the {@code StringBuffer} to populate
    * @param fieldName the field name, typically not used as already appended
    * @param array     the array to add to the {@code toString},
    *                  not {@code null}
    */
  protected def appendDetail(buffer: StringBuffer, fieldName: String, array: Array[Short]): Unit = {
    buffer.append(arrayStart)
    for (i <- 0 until array.length) {
      if (i > 0) buffer.append(arraySeparator)
      appendDetail(buffer, fieldName, array(i))
    }
    buffer.append(arrayEnd)
    ()
  }

  /**
    * <p>Append to the {@code toString} a summary of a
    * {@code short} array.</p>
    *
    * @param buffer    the {@code StringBuffer} to populate
    * @param fieldName the field name, typically not used as already appended
    * @param array     the array to add to the {@code toString},
    *                  not {@code null}
    */
  protected def appendSummary(buffer: StringBuffer, fieldName: String, array: Array[Short]): Unit = {
    appendSummarySize(buffer, fieldName, array.length)
  }

  /**
    * <p>Append to the {@code toString} a {@code byte}
    * array.</p>
    *
    * @param buffer     the {@code StringBuffer} to populate
    * @param fieldName  the field name
    * @param array      the array to add to the {@code toString}
    * @param fullDetail {@code true} for detail, {@code false}
    *                   for summary info, {@code null} for style decides
    */
  def append(buffer: StringBuffer, fieldName: String, array: Array[Byte], fullDetail: Boolean): Unit = {
    appendFieldStart(buffer, fieldName)
    if (array == null) appendNullText(buffer, fieldName)
    else if (isFullDetail(fullDetail)) appendDetail(buffer, fieldName, array)
    else appendSummary(buffer, fieldName, array)
    appendFieldEnd(buffer, fieldName)
  }

  /**
    * <p>Append to the {@code toString} the detail of a
    * {@code byte} array.</p>
    *
    * @param buffer    the {@code StringBuffer} to populate
    * @param fieldName the field name, typically not used as already appended
    * @param array     the array to add to the {@code toString},
    *                  not {@code null}
    */
  protected def appendDetail(buffer: StringBuffer, fieldName: String, array: Array[Byte]): Unit = {
    buffer.append(arrayStart)
    for (i <- 0 until array.length) {
      if (i > 0) buffer.append(arraySeparator)
      appendDetail(buffer, fieldName, array(i))
    }
    buffer.append(arrayEnd)
    ()
  }

  /**
    * <p>Append to the {@code toString} a summary of a
    * {@code byte} array.</p>
    *
    * @param buffer    the {@code StringBuffer} to populate
    * @param fieldName the field name, typically not used as already appended
    * @param array     the array to add to the {@code toString},
    *                  not {@code null}
    */
  protected def appendSummary(buffer: StringBuffer, fieldName: String, array: Array[Byte]): Unit = {
    appendSummarySize(buffer, fieldName, array.length)
  }

  /**
    * <p>Append to the {@code toString} a {@code char}
    * array.</p>
    *
    * @param buffer     the {@code StringBuffer} to populate
    * @param fieldName  the field name
    * @param array      the array to add to the {@code toString}
    * @param fullDetail {@code true} for detail, {@code false}
    *                   for summary info, {@code null} for style decides
    */
  def append(buffer: StringBuffer, fieldName: String, array: Array[Char], fullDetail: Boolean): Unit = {
    appendFieldStart(buffer, fieldName)
    if (array == null) appendNullText(buffer, fieldName)
    else if (isFullDetail(fullDetail)) appendDetail(buffer, fieldName, array)
    else appendSummary(buffer, fieldName, array)
    appendFieldEnd(buffer, fieldName)
  }

  /**
    * <p>Append to the {@code toString} the detail of a
    * {@code char} array.</p>
    *
    * @param buffer    the {@code StringBuffer} to populate
    * @param fieldName the field name, typically not used as already appended
    * @param array     the array to add to the {@code toString},
    *                  not {@code null}
    */
  protected def appendDetail(buffer: StringBuffer, fieldName: String, array: Array[Char]): Unit = {
    buffer.append(arrayStart)
    for (i <- 0 until array.length) {
      if (i > 0) buffer.append(arraySeparator)
      appendDetail(buffer, fieldName, array(i))
    }
    buffer.append(arrayEnd)
    ()
  }

  /**
    * <p>Append to the {@code toString} a summary of a
    * {@code char} array.</p>
    *
    * @param buffer    the {@code StringBuffer} to populate
    * @param fieldName the field name, typically not used as already appended
    * @param array     the array to add to the {@code toString},
    *                  not {@code null}
    */
  protected def appendSummary(buffer: StringBuffer, fieldName: String, array: Array[Char]): Unit = {
    appendSummarySize(buffer, fieldName, array.length)
  }

  /**
    * <p>Append to the {@code toString} a {@code double}
    * array.</p>
    *
    * @param buffer     the {@code StringBuffer} to populate
    * @param fieldName  the field name
    * @param array      the array to add to the toString
    * @param fullDetail {@code true} for detail, {@code false}
    *                   for summary info, {@code null} for style decides
    */
  def append(buffer: StringBuffer, fieldName: String, array: Array[Double], fullDetail: Boolean): Unit = {
    appendFieldStart(buffer, fieldName)
    if (array == null) appendNullText(buffer, fieldName)
    else if (isFullDetail(fullDetail)) appendDetail(buffer, fieldName, array)
    else appendSummary(buffer, fieldName, array)
    appendFieldEnd(buffer, fieldName)
  }

  /**
    * <p>Append to the {@code toString} the detail of a
    * {@code double} array.</p>
    *
    * @param buffer    the {@code StringBuffer} to populate
    * @param fieldName the field name, typically not used as already appended
    * @param array     the array to add to the {@code toString},
    *                  not {@code null}
    */
  protected def appendDetail(buffer: StringBuffer, fieldName: String, array: Array[Double]): Unit = {
    buffer.append(arrayStart)
    for (i <- 0 until array.length) {
      if (i > 0) buffer.append(arraySeparator)
      appendDetail(buffer, fieldName, array(i))
    }
    buffer.append(arrayEnd)
    ()
  }

  /**
    * <p>Append to the {@code toString} a summary of a
    * {@code double} array.</p>
    *
    * @param buffer    the {@code StringBuffer} to populate
    * @param fieldName the field name, typically not used as already appended
    * @param array     the array to add to the {@code toString},
    *                  not {@code null}
    */
  protected def appendSummary(buffer: StringBuffer, fieldName: String, array: Array[Double]): Unit = {
    appendSummarySize(buffer, fieldName, array.length)
  }

  /**
    * <p>Append to the {@code toString} a {@code float}
    * array.</p>
    *
    * @param buffer     the {@code StringBuffer} to populate
    * @param fieldName  the field name
    * @param array      the array to add to the toString
    * @param fullDetail {@code true} for detail, {@code false}
    *                   for summary info, {@code null} for style decides
    */
  def append(buffer: StringBuffer, fieldName: String, array: Array[Float], fullDetail: Boolean): Unit = {
    appendFieldStart(buffer, fieldName)
    if (array == null) appendNullText(buffer, fieldName)
    else if (isFullDetail(fullDetail)) appendDetail(buffer, fieldName, array)
    else appendSummary(buffer, fieldName, array)
    appendFieldEnd(buffer, fieldName)
  }

  /**
    * <p>Append to the {@code toString} the detail of a
    * {@code float} array.</p>
    *
    * @param buffer    the {@code StringBuffer} to populate
    * @param fieldName the field name, typically not used as already appended
    * @param array     the array to add to the {@code toString},
    *                  not {@code null}
    */
  protected def appendDetail(buffer: StringBuffer, fieldName: String, array: Array[Float]): Unit = {
    buffer.append(arrayStart)
    for (i <- 0 until array.length) {
      if (i > 0) buffer.append(arraySeparator)
      appendDetail(buffer, fieldName, array(i))
    }
    buffer.append(arrayEnd)
    ()
  }

  /**
    * <p>Append to the {@code toString} a summary of a
    * {@code float} array.</p>
    *
    * @param buffer    the {@code StringBuffer} to populate
    * @param fieldName the field name, typically not used as already appended
    * @param array     the array to add to the {@code toString},
    *                  not {@code null}
    */
  protected def appendSummary(buffer: StringBuffer, fieldName: String, array: Array[Float]): Unit = {
    appendSummarySize(buffer, fieldName, array.length)
  }

  /**
    * <p>Append to the {@code toString} a {@code boolean}
    * array.</p>
    *
    * @param buffer     the {@code StringBuffer} to populate
    * @param fieldName  the field name
    * @param array      the array to add to the toString
    * @param fullDetail {@code true} for detail, {@code false}
    *                   for summary info, {@code null} for style decides
    */
  def append(buffer: StringBuffer, fieldName: String, array: Array[Boolean], fullDetail: Boolean): Unit = {
    appendFieldStart(buffer, fieldName)
    if (array == null) appendNullText(buffer, fieldName)
    else if (isFullDetail(fullDetail)) appendDetail(buffer, fieldName, array)
    else appendSummary(buffer, fieldName, array)
    appendFieldEnd(buffer, fieldName)
  }

  /**
    * <p>Append to the {@code toString} the detail of a
    * {@code boolean} array.</p>
    *
    * @param buffer    the {@code StringBuffer} to populate
    * @param fieldName the field name, typically not used as already appended
    * @param array     the array to add to the {@code toString},
    *                  not {@code null}
    */
  protected def appendDetail(buffer: StringBuffer, fieldName: String, array: Array[Boolean]): Unit = {
    buffer.append(arrayStart)
    for (i <- 0 until array.length) {
      if (i > 0) buffer.append(arraySeparator)
      appendDetail(buffer, fieldName, array(i))
    }
    buffer.append(arrayEnd)
    ()
  }

  /**
    * <p>Append to the {@code toString} a summary of a
    * {@code boolean} array.</p>
    *
    * @param buffer    the {@code StringBuffer} to populate
    * @param fieldName the field name, typically not used as already appended
    * @param array     the array to add to the {@code toString},
    *                  not {@code null}
    */
  protected def appendSummary(buffer: StringBuffer, fieldName: String, array: Array[Boolean]): Unit = {
    appendSummarySize(buffer, fieldName, array.length)
  }

  /**
    * <p>Append to the {@code toString} the class name.</p>
    *
    * @param buffer the {@code StringBuffer} to populate
    * @param object the {@code Object} whose name to output
    */
  protected def appendClassName(buffer: StringBuffer, `object`: Any): Unit = {
    if (useClassName && `object` != null) {
      ToStringStyle.register(`object`)
      if (useShortClassName) buffer.append(getShortClassName(`object`.getClass))
      else buffer.append(`object`.getClass.getName)
    }
    ()
  }

  /**
    * <p>Append the {@link System# identityHashCode ( java.lang.Object )}.</p>
    *
    * @param buffer the {@code StringBuffer} to populate
    * @param object the {@code Object} whose id to output
    */
  protected def appendIdentityHashCode(buffer: StringBuffer, `object`: Any): Unit = {
    if (this.isUseIdentityHashCode && `object` != null) {
      ToStringStyle.register(`object`)
      buffer.append('@')
      buffer.append(Integer.toHexString(System.identityHashCode(`object`)))
    }
    ()
  }

  /**
    * <p>Append to the {@code toString} the content start.</p>
    *
    * @param buffer the {@code StringBuffer} to populate
    */
  protected def appendContentStart(buffer: StringBuffer): Unit = {
    buffer.append(contentStart)
    ()
  }

  /**
    * <p>Append to the {@code toString} the content end.</p>
    *
    * @param buffer the {@code StringBuffer} to populate
    */
  protected def appendContentEnd(buffer: StringBuffer): Unit = {
    buffer.append(contentEnd)
    ()
  }

  /**
    * <p>Append to the {@code toString} an indicator for {@code null}.</p>
    *
    * <p>The default indicator is {@code '&lt;null&gt;'}.</p>
    *
    * @param buffer    the {@code StringBuffer} to populate
    * @param fieldName the field name, typically not used as already appended
    */
  protected def appendNullText(buffer: StringBuffer, fieldName: String): Unit = {
    buffer.append(nullText)
    ()
  }

  /**
    * <p>Append to the {@code toString} the field separator.</p>
    *
    * @param buffer the {@code StringBuffer} to populate
    */
  protected def appendFieldSeparator(buffer: StringBuffer): Unit = {
    buffer.append(fieldSeparator)
    ()
  }

  /**
    * <p>Append to the {@code toString} the field start.</p>
    *
    * @param buffer    the {@code StringBuffer} to populate
    * @param fieldName the field name
    */
  protected def appendFieldStart(buffer: StringBuffer, fieldName: String): Unit = {
    if (useFieldNames && fieldName != null) {
      buffer.append(fieldName)
      buffer.append(fieldNameValueSeparator)
    }
    ()
  }

  /**
    * <p>Append to the {@code toString} the field end.</p>
    *
    * @param buffer    the {@code StringBuffer} to populate
    * @param fieldName the field name, typically not used as already appended
    */
  protected def appendFieldEnd(buffer: StringBuffer, fieldName: String): Unit = {
    appendFieldSeparator(buffer)
  }

  /**
    * <p>Append to the {@code toString} a size summary.</p>
    *
    * <p>The size summary is used to summarize the contents of
    * {@code Collections}, {@code Maps} and arrays.</p>
    *
    * <p>The output consists of a prefix, the passed in size
    * and a suffix.</p>
    *
    * <p>The default format is {@code '&lt;size=n&gt;'}.</p>
    *
    * @param buffer    the {@code StringBuffer} to populate
    * @param fieldName the field name, typically not used as already appended
    * @param size      the size to append
    */
  protected def appendSummarySize(buffer: StringBuffer, fieldName: String, size: Int): Unit = {
    buffer.append(sizeStartText)
    buffer.append(size)
    buffer.append(sizeEndText)
    ()
  }

  /**
    * <p>Is this field to be output in full detail.</p>
    *
    * <p>This method converts a detail request into a detail level.
    * The calling code may request full detail ({@code true}),
    * but a subclass might ignore that and always return
    * {@code false}. The calling code may pass in
    * {@code null} indicating that it doesn't care about
    * the detail level. In this case the default detail level is
    * used.</p>
    *
    * @param fullDetailRequest the detail level requested
    * @return whether full detail is to be shown
    */
  protected def isFullDetail(fullDetailRequest: Boolean): Boolean = {
    if (fullDetailRequest == null) return defaultFullDetail
    fullDetailRequest.booleanValue
  }

  /**
    * <p>Gets the short class name for a class.</p>
    *
    * <p>The short class name is the classname excluding
    * the package name.</p>
    *
    * @param cls the {@code Class} to get the short name of
    * @return the short name
    */
  protected def getShortClassName(cls: Class[_]): String = ClassUtils.getShortClassName(cls)

  /**
    * <p>Gets whether to use the class name.</p>
    *
    * @return the current useClassName flag
    */
  protected def isUseClassName: Boolean = useClassName

  /**
    * <p>Sets whether to use the class name.</p>
    *
    * @param useClassName the new useClassName flag
    */
  protected def setUseClassName(useClassName: Boolean): Unit = {
    this.useClassName = useClassName
  }

  /**
    * <p>Gets whether to output short or long class names.</p>
    *
    * @return the current useShortClassName flag
    * @since 2.0
    */
  protected def isUseShortClassName: Boolean = useShortClassName

  /**
    * <p>Sets whether to output short or long class names.</p>
    *
    * @param useShortClassName the new useShortClassName flag
    * @since 2.0
    */
  protected def setUseShortClassName(useShortClassName: Boolean): Unit = {
    this.useShortClassName = useShortClassName
  }

  /**
    * <p>Gets whether to use the identity hash code.</p>
    *
    * @return the current useIdentityHashCode flag
    */
  protected def isUseIdentityHashCode: Boolean = useIdentityHashCode

  /**
    * <p>Sets whether to use the identity hash code.</p>
    *
    * @param useIdentityHashCode the new useIdentityHashCode flag
    */
  protected def setUseIdentityHashCode(useIdentityHashCode: Boolean): Unit = {
    this.useIdentityHashCode = useIdentityHashCode
  }

  /**
    * <p>Gets whether to use the field names passed in.</p>
    *
    * @return the current useFieldNames flag
    */
  protected def isUseFieldNames: Boolean = useFieldNames

  /**
    * <p>Sets whether to use the field names passed in.</p>
    *
    * @param useFieldNames the new useFieldNames flag
    */
  protected def setUseFieldNames(useFieldNames: Boolean): Unit = {
    this.useFieldNames = useFieldNames
  }

  /**
    * <p>Gets whether to use full detail when the caller doesn't
    * specify.</p>
    *
    * @return the current defaultFullDetail flag
    */
  protected def isDefaultFullDetail: Boolean = defaultFullDetail

  /**
    * <p>Sets whether to use full detail when the caller doesn't
    * specify.</p>
    *
    * @param defaultFullDetail the new defaultFullDetail flag
    */
  protected def setDefaultFullDetail(defaultFullDetail: Boolean): Unit = {
    this.defaultFullDetail = defaultFullDetail
  }

  /**
    * <p>Gets whether to output array content detail.</p>
    *
    * @return the current array content detail setting
    */
  protected def isArrayContentDetail: Boolean = arrayContentDetail

  /**
    * <p>Sets whether to output array content detail.</p>
    *
    * @param arrayContentDetail the new arrayContentDetail flag
    */
  protected def setArrayContentDetail(arrayContentDetail: Boolean): Unit = {
    this.arrayContentDetail = arrayContentDetail
  }

  /**
    * <p>Gets the array start text.</p>
    *
    * @return the current array start text
    */
  protected def getArrayStart: String = arrayStart

  /**
    * <p>Sets the array start text.</p>
    *
    * <p>{@code null} is accepted, but will be converted to
    * an empty String.</p>
    *
    * @param arrayStart the new array start text
    */
  protected def setArrayStart(arrayStart: String): Unit = {
    if (arrayStart == null) this.arrayStart = StringUtils.EMPTY
    this.arrayStart = arrayStart
  }

  /**
    * <p>Gets the array end text.</p>
    *
    * @return the current array end text
    */
  protected def getArrayEnd: String = arrayEnd

  /**
    * <p>Sets the array end text.</p>
    *
    * <p>{@code null} is accepted, but will be converted to
    * an empty String.</p>
    *
    * @param arrayEnd the new array end text
    */
  protected def setArrayEnd(arrayEnd: String): Unit = {
    if (arrayEnd == null) this.arrayEnd = StringUtils.EMPTY
    this.arrayEnd = arrayEnd
  }

  /**
    * <p>Gets the array separator text.</p>
    *
    * @return the current array separator text
    */
  protected def getArraySeparator: String = arraySeparator

  /**
    * <p>Sets the array separator text.</p>
    *
    * <p>{@code null} is accepted, but will be converted to
    * an empty String.</p>
    *
    * @param arraySeparator the new array separator text
    */
  protected def setArraySeparator(arraySeparator: String): Unit = {
    if (arraySeparator == null) this.arraySeparator = StringUtils.EMPTY
    else this.arraySeparator = arraySeparator
  }

  /**
    * <p>Gets the content start text.</p>
    *
    * @return the current content start text
    */
  protected def getContentStart: String = contentStart

  /**
    * <p>Sets the content start text.</p>
    *
    * <p>{@code null} is accepted, but will be converted to
    * an empty String.</p>
    *
    * @param contentStart the new content start text
    */
  protected def setContentStart(contentStart: String): Unit = {
    if (contentStart == null) this.contentStart = StringUtils.EMPTY
    this.contentStart = contentStart
  }

  /**
    * <p>Gets the content end text.</p>
    *
    * @return the current content end text
    */
  protected def getContentEnd: String = contentEnd

  /**
    * <p>Sets the content end text.</p>
    *
    * <p>{@code null} is accepted, but will be converted to
    * an empty String.</p>
    *
    * @param contentEnd the new content end text
    */
  protected def setContentEnd(contentEnd: String): Unit = {
    if (contentEnd == null) this.contentEnd = StringUtils.EMPTY
    this.contentEnd = contentEnd
  }

  /**
    * <p>Gets the field name value separator text.</p>
    *
    * @return the current field name value separator text
    */
  protected def getFieldNameValueSeparator: String = fieldNameValueSeparator

  /**
    * <p>Sets the field name value separator text.</p>
    *
    * <p>{@code null} is accepted, but will be converted to
    * an empty String.</p>
    *
    * @param fieldNameValueSeparator the new field name value separator text
    */
  protected def setFieldNameValueSeparator(fieldNameValueSeparator: String): Unit = {
    if (fieldNameValueSeparator == null) this.fieldNameValueSeparator = StringUtils.EMPTY
    this.fieldNameValueSeparator = fieldNameValueSeparator
  }

  /**
    * <p>Gets the field separator text.</p>
    *
    * @return the current field separator text
    */
  protected def getFieldSeparator: String = fieldSeparator

  /**
    * <p>Sets the field separator text.</p>
    *
    * <p>{@code null} is accepted, but will be converted to
    * an empty String.</p>
    *
    * @param fieldSeparator the new field separator text
    */
  protected def setFieldSeparator(fieldSeparator: String): Unit = {
    if (fieldSeparator == null) this.fieldSeparator = StringUtils.EMPTY
    this.fieldSeparator = fieldSeparator
  }

  /**
    * <p>Gets whether the field separator should be added at the start
    * of each buffer.</p>
    *
    * @return the fieldSeparatorAtStart flag
    * @since 2.0
    */
  protected def isFieldSeparatorAtStart: Boolean = fieldSeparatorAtStart

  /**
    * <p>Sets whether the field separator should be added at the start
    * of each buffer.</p>
    *
    * @param fieldSeparatorAtStart the fieldSeparatorAtStart flag
    * @since 2.0
    */
  protected def setFieldSeparatorAtStart(fieldSeparatorAtStart: Boolean): Unit = {
    this.fieldSeparatorAtStart = fieldSeparatorAtStart
  }

  /**
    * <p>Gets whether the field separator should be added at the end
    * of each buffer.</p>
    *
    * @return fieldSeparatorAtEnd flag
    * @since 2.0
    */
  protected def isFieldSeparatorAtEnd: Boolean = fieldSeparatorAtEnd

  /**
    * <p>Sets whether the field separator should be added at the end
    * of each buffer.</p>
    *
    * @param fieldSeparatorAtEnd the fieldSeparatorAtEnd flag
    * @since 2.0
    */
  protected def setFieldSeparatorAtEnd(fieldSeparatorAtEnd: Boolean): Unit = {
    this.fieldSeparatorAtEnd = fieldSeparatorAtEnd
  }

  /**
    * <p>Gets the text to output when {@code null} found.</p>
    *
    * @return the current text to output when null found
    */
  protected[lang3] def getNullText: String = nullText

  /**
    * <p>Sets the text to output when {@code null} found.</p>
    *
    * <p>{@code null} is accepted, but will be converted to
    * an empty String.</p>
    *
    * @param nullText the new text to output when null found
    */
  protected def setNullText(nullText: String): Unit = {
    if (nullText == null) this.nullText = StringUtils.EMPTY
    else this.nullText = nullText
  }

  /**
    * <p>Gets the start text to output when a {@code Collection},
    * {@code Map} or array size is output.</p>
    *
    * <p>This is output before the size value.</p>
    *
    * @return the current start of size text
    */
  protected def getSizeStartText: String = sizeStartText

  /**
    * <p>Sets the start text to output when a {@code Collection},
    * {@code Map} or array size is output.</p>
    *
    * <p>This is output before the size value.</p>
    *
    * <p>{@code null} is accepted, but will be converted to
    * an empty String.</p>
    *
    * @param sizeStartText the new start of size text
    */
  protected def setSizeStartText(sizeStartText: String): Unit = {
    if (sizeStartText == null) this.sizeStartText = StringUtils.EMPTY
    else this.sizeStartText = sizeStartText
  }

  /**
    * <p>Gets the end text to output when a {@code Collection},
    * {@code Map} or array size is output.</p>
    *
    * <p>This is output after the size value.</p>
    *
    * @return the current end of size text
    */
  protected def getSizeEndText: String = sizeEndText

  /**
    * <p>Sets the end text to output when a {@code Collection},
    * {@code Map} or array size is output.</p>
    *
    * <p>This is output after the size value.</p>
    *
    * <p>{@code null} is accepted, but will be converted to
    * an empty String.</p>
    *
    * @param sizeEndText the new end of size text
    */
  protected def setSizeEndText(sizeEndText: String): Unit = {
    if (sizeEndText == null) this.sizeEndText = StringUtils.EMPTY
    else this.sizeEndText = sizeEndText
  }

  /**
    * <p>Gets the start text to output when an {@code Object} is
    * output in summary mode.</p>
    *
    * <p>This is output before the size value.</p>
    *
    * @return the current start of summary text
    */
  protected def getSummaryObjectStartText: String = summaryObjectStartText

  /**
    * <p>Sets the start text to output when an {@code Object} is
    * output in summary mode.</p>
    *
    * <p>This is output before the size value.</p>
    *
    * <p>{@code null} is accepted, but will be converted to
    * an empty String.</p>
    *
    * @param summaryObjectStartText the new start of summary text
    */
  protected def setSummaryObjectStartText(summaryObjectStartText: String): Unit = {
    if (summaryObjectStartText == null) this.summaryObjectStartText = StringUtils.EMPTY
    else this.summaryObjectStartText = summaryObjectStartText
  }

  /**
    * <p>Gets the end text to output when an {@code Object} is
    * output in summary mode.</p>
    *
    * <p>This is output after the size value.</p>
    *
    * @return the current end of summary text
    */
  protected def getSummaryObjectEndText: String = summaryObjectEndText

  /**
    * <p>Sets the end text to output when an {@code Object} is
    * output in summary mode.</p>
    *
    * <p>This is output after the size value.</p>
    *
    * <p>{@code null} is accepted, but will be converted to
    * an empty String.</p>
    *
    * @param summaryObjectEndText the new end of summary text
    */
  protected def setSummaryObjectEndText(summaryObjectEndText: String): Unit = {
    if (summaryObjectEndText == null) this.summaryObjectEndText = StringUtils.EMPTY
    else this.summaryObjectEndText = summaryObjectEndText
  }
}