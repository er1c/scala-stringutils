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

/**
  * Exception thrown when a clone cannot be created. In contrast to
  * {@link CloneNotSupportedException} this is a {@link RuntimeException}.
  *
  * @param message description of the exception
  * @param cause   cause of the exception
  * @since 3.0
  */
@SerialVersionUID(20091223L)
class CloneFailedException(message: String, cause: Throwable) extends RuntimeException(message, cause) {
  /**
    * Constructs a CloneFailedException.
    *
    * @param message description of the exception
    */
  def this(message: String) {
    this(message, null)
  }

  /**
    * Constructs a CloneFailedException.
    *
    * @param cause cause of the exception
    */
  def this(cause: Throwable) {
    this(null, cause)
  }

  /**
    * Constructs a CloneFailedException.
    */
  def this() {
    this(null, null)
  }
}