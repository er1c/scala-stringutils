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

package org.apache.commons.lang3.exception

import java.io.Serializable
import java.util
import org.apache.commons.lang3.StringUtils
import org.apache.commons.lang3.tuple.ImmutablePair
import org.apache.commons.lang3.tuple.Pair
import scala.collection.JavaConverters._

/**
  * Default implementation of the context storing the label-value pairs for contexted exceptions.
  * <p>
  * This implementation is serializable, however this is dependent on the values that
  * are added also being serializable.
  * </p>
  *
  * @see ContextedException
  * @see ContextedRuntimeException
  * @since 3.0
  */
@SerialVersionUID(20110706L)
class DefaultExceptionContext extends ExceptionContext with Serializable {
  /** The list storing the label-data pairs. */
  final private val contextValues: util.ArrayList[Pair[String, Any]] = new util.ArrayList[Pair[String, Any]]

  /**
    * {@inheritDoc }
    */
  override def addContextValue(label: String, value: Any): DefaultExceptionContext = {
    contextValues.add(new ImmutablePair[String, Any](label, value))
    this
  }

  override def setContextValue(label: String, value: Any): DefaultExceptionContext = {
    import java.util.function.{Predicate ⇒ JPredicate}
    contextValues.removeIf(
      // Explicit for 2.11 compat
      new JPredicate[Pair[String, Any]] {
        override def test(p: Pair[String, Any]): Boolean = StringUtils.equals(label, p.getKey)
      }
    )

    addContextValue(label, value)
    this
  }

  override def getContextValues(label: String): util.List[Any] = {
    val values = new util.ArrayList[Any]

    for (pair <- contextValues.iterator.asScala) {
      if (StringUtils.equals(label, pair.getKey)) values.add(pair.getValue)
    }

    values
  }

  override def getFirstContextValue(label: String): Any = {
    for (pair <- contextValues.iterator.asScala) {
      if (StringUtils.equals(label, pair.getKey)) return pair.getValue
    }

    null
  }

  override def getContextLabels: util.Set[String] = {
    val labels = new util.HashSet[String]

    for (pair <- contextValues.iterator.asScala) {
      labels.add(pair.getKey)
    }

    labels
  }

  override def getContextEntries: util.List[Pair[String, Any]] = contextValues

  /**
    * Builds the message containing the contextual information.
    *
    * @param baseMessage the base exception message <b>without</b> context information appended
    * @return the exception message <b>with</b> context information appended, never null
    */
  override def getFormattedExceptionMessage(baseMessage: String): String = {
    val buffer = new StringBuilder(256)
    if (baseMessage != null) buffer.append(baseMessage)
    if (!contextValues.isEmpty) {
      if (buffer.length > 0) buffer.append('\n')
      buffer.append("Exception Context:\n")
      var i = 0

      for (pair <- contextValues.iterator.asScala) {
        buffer.append("\t[")
        buffer.append({
          i += 1; i
        })
        buffer.append(':')
        buffer.append(pair.getKey)
        buffer.append("=")
        val value = pair.getValue
        if (value == null) buffer.append("null")
        else {
          var valueStr: String = null
          try {
            valueStr = value.toString
          } catch {
            case e: Exception =>
              valueStr = "Exception thrown on toString(): " + ExceptionUtils.getStackTrace(e)
          }
          buffer.append(valueStr)
        }
        buffer.append("]\n")
      }
      buffer.append("---------------------------------")
    }
    buffer.toString
  }
}
