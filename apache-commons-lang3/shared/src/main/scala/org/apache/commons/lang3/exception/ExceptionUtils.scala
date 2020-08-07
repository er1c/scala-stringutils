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

import java.io.PrintStream
import java.io.PrintWriter
import java.io.StringWriter
import java.lang.reflect.InvocationTargetException
import java.lang.reflect.Method
import java.lang.reflect.UndeclaredThrowableException
import java.util
import java.util.Objects
import java.util.StringTokenizer
import org.apache.commons.lang3.ArrayUtils
import org.apache.commons.lang3.ClassUtils
import org.apache.commons.lang3.StringUtils

/**
  * <p>Provides utilities for manipulating and examining
  * {@code Throwable} objects.</p>
  *
  * @since 1.0
  */
object ExceptionUtils {
  private val NOT_FOUND = -1
  /**
    * <p>The names of methods commonly used to access a wrapped exception.</p>
    */
  // TODO: Remove in Lang 4.0
  private val CAUSE_METHOD_NAMES = Array(
    "getCause",
    "getNextException",
    "getTargetException",
    "getException",
    "getSourceException",
    "getRootCause",
    "getCausedByException",
    "getNested",
    "getLinkedException",
    "getNestedException",
    "getLinkedCause",
    "getThrowable"
  )
  /**
    * <p>Used when printing stack frames to denote the start of a
    * wrapped exception.</p>
    *
    * <p>Package private for accessibility by test suite.</p>
    */
  private[exception] val WRAPPED_MARKER = " [wrapped] "

  /**
    * <p>Introspects the {@code Throwable} to obtain the cause.</p>
    *
    * <p>The method searches for methods with specific names that return a
    * {@code Throwable} object. This will pick up most wrapping exceptions,
    * including those from JDK 1.4.
    *
    * <p>The default list searched for are:</p>
    * <ul>
    * <li>{@code getCause()}</li>
    * <li>{@code getNextException()}</li>
    * <li>{@code getTargetException()}</li>
    * <li>{@code getException()}</li>
    * <li>{@code getSourceException()}</li>
    * <li>{@code getRootCause()}</li>
    * <li>{@code getCausedByException()}</li>
    * <li>{@code getNested()}</li>
    * </ul>
    *
    * <p>If none of the above is found, returns {@code null}.</p>
    *
    * @param throwable the throwable to introspect for a cause, may be null
    * @return the cause of the {@code Throwable},
    *         {@code null} if none found or null throwable input
    * @since 1.0
    * @deprecated This feature will be removed in Lang 4.0, use {@link Throwable# getCause} instead
    */
  @deprecated def getCause(throwable: Throwable): Throwable = getCause(throwable, null)

  /**
    * <p>Introspects the {@code Throwable} to obtain the cause.</p>
    *
    * <p>A {@code null} set of method names means use the default set.
    * A {@code null} in the set of method names will be ignored.</p>
    *
    * @param throwable   the throwable to introspect for a cause, may be null
    * @param methodNames the method names, null treated as default set
    * @return the cause of the {@code Throwable},
    *         {@code null} if none found or null throwable input
    * @since 1.0
    * @deprecated This feature will be removed in Lang 4.0, use {@link Throwable# getCause} instead
    */
  @deprecated def getCause(throwable: Throwable, methodNames: Array[String]): Throwable = {
    if (throwable == null) return null

    val _methodNames = if (methodNames == null) {
      val cause = throwable.getCause
      if (cause != null) return cause
      CAUSE_METHOD_NAMES
    } else methodNames

    for (methodName <- _methodNames
      if methodName != null) {
      val legacyCause = getCauseUsingMethodName(throwable, methodName)
      if (legacyCause != null) return legacyCause
    }

    null
  }

  /**
    * <p>Finds a {@code Throwable} by method name.</p>
    *
    * @param throwable  the exception to examine
    * @param methodName the name of the method to find and invoke
    * @return the wrapped exception, or {@code null} if not found
    */
  private def getCauseUsingMethodName(throwable: Throwable, methodName: String): Throwable = {
    var method: Method = null

    try {
      method = throwable.getClass.getMethod(methodName)
    } catch {
      case _: NoSuchMethodException | _: SecurityException => // NOPMD
      // exception ignored
    }

    if (method != null && classOf[Throwable].isAssignableFrom(method.getReturnType)) {
      try {
        return method.invoke(throwable).asInstanceOf[Throwable]
      } catch {
        case _: IllegalAccessException | _: IllegalArgumentException | _: InvocationTargetException =>
      }
    }

    null
  }

  /**
    * <p>Returns the default names used when searching for the cause of an exception.</p>
    *
    * <p>This may be modified and used in the overloaded getCause(Throwable, String[]) method.</p>
    *
    * @return cloned array of the default method names
    * @since 3.0
    * @deprecated This feature will be removed in Lang 4.0
    */
  @deprecated def getDefaultCauseMethodNames: Array[String] = ArrayUtils.clone(CAUSE_METHOD_NAMES)

  /**
    * Gets a short message summarising the exception.
    * <p>
    * The message returned is of the form
    * {ClassNameWithoutPackage}: {ThrowableMessage}
    *
    * @param th the throwable to get a message for, null returns empty string
    * @return the message, non-null
    * @since 2.2
    */
  def getMessage(th: Throwable): String = {
    if (th == null) return StringUtils.EMPTY

    val clsName = ClassUtils.getShortClassName(th, null)
    val msg = th.getMessage
    clsName + ": " + StringUtils.defaultString(msg)
  }

  /**
    * <p>Introspects the {@code Throwable} to obtain the root cause.</p>
    *
    * <p>This method walks through the exception chain to the last element,
    * "root" of the tree, using {@link Throwable# getCause ( )}, and
    * returns that exception.</p>
    *
    * <p>From version 2.2, this method handles recursive cause structures
    * that might otherwise cause infinite loops. If the throwable parameter
    * has a cause of itself, then null will be returned. If the throwable
    * parameter cause chain loops, the last element in the chain before the
    * loop is returned.</p>
    *
    * @param throwable the throwable to get the root cause for, may be null
    * @return the root cause of the {@code Throwable},
    *         {@code null} if null throwable input
    */
  def getRootCause(throwable: Throwable): Throwable = {
    val list = getThrowableList(throwable)
    if (list.isEmpty) null
    else list.get(list.size - 1)
  }

  /**
    * Gets a short message summarising the root cause exception.
    * <p>
    * The message returned is of the form
    * {ClassNameWithoutPackage}: {ThrowableMessage}
    *
    * @param th the throwable to get a message for, null returns empty string
    * @return the message, non-null
    * @since 2.2
    */
  def getRootCauseMessage(th: Throwable): String = {
    var root = getRootCause(th)
    root =
      if (root == null) th
      else root
    getMessage(root)
  }

  /**
    * <p>Creates a compact stack trace for the root cause of the supplied
    * {@code Throwable}.</p>
    *
    * <p>The output of this method is consistent across JDK versions.
    * It consists of the root exception followed by each of its wrapping
    * exceptions separated by '[wrapped]'. Note that this is the opposite
    * order to the JDK1.4 display.</p>
    *
    * @param throwable the throwable to examine, may be null
    * @return an array of stack trace frames, never null
    * @since 2.0
    */
  def getRootCauseStackTrace(throwable: Throwable): Array[String] = {
    if (throwable == null) return ArrayUtils.EMPTY_STRING_ARRAY

    val throwables = getThrowables(throwable)
    val count = throwables.length
    val frames = new util.ArrayList[String]
    var nextTrace = getStackFrameList(throwables(count - 1))
    var i = count

    while ({ i -= 1; i } >= 0) {
      val trace = nextTrace
      if (i != 0) {
        nextTrace = getStackFrameList(throwables(i - 1))
        removeCommonFrames(trace, nextTrace)
      }
      if (i == count - 1) frames.add(throwables(i).toString)
      else frames.add(WRAPPED_MARKER + throwables(i).toString)
      frames.addAll(trace)
    }

    frames.toArray(ArrayUtils.EMPTY_STRING_ARRAY)
  }

  /**
    * <p>Produces a {@code List} of stack frames - the message
    * is not included. Only the trace of the specified exception is
    * returned, any caused by trace is stripped.</p>
    *
    * <p>This works in most cases - it will only fail if the exception
    * message contains a line that starts with:
    * {@code &quot;&nbsp;&nbsp;&nbsp;at&quot;.}</p>
    *
    * @param t is any throwable
    * @return List of stack frames
    */
  private[exception] def getStackFrameList(t: Throwable): util.ArrayList[String] = {
    val stackTrace = getStackTrace(t)
    val linebreak = System.lineSeparator
    val frames = new StringTokenizer(stackTrace, linebreak)
    val list = new util.ArrayList[String]
    var traceStarted = false

    var done = false
    while (frames.hasMoreTokens && !done) {
      val token = frames.nextToken
      // Determine if the line starts with <whitespace>at
      val at = token.indexOf("at")
      if (at != NOT_FOUND && token.substring(0, at).trim.isEmpty) {
        traceStarted = true
        list.add(token)
      } else if (traceStarted) done = true
    }

    list
  }

  /**
    * <p>Returns an array where each element is a line from the argument.</p>
    *
    * <p>The end of line is determined by the value of {@link System# lineSeparator ( )}.</p>
    *
    * @param stackTrace a stack trace String
    * @return an array where each element is a line from the argument
    */
  private[exception] def getStackFrames(stackTrace: String) = {
    val linebreak = System.lineSeparator
    val frames = new StringTokenizer(stackTrace, linebreak)
    val list = new util.ArrayList[String]
    while ({
      frames.hasMoreTokens
    }) list.add(frames.nextToken)

    list.toArray(ArrayUtils.EMPTY_STRING_ARRAY)
  }

  /**
    * <p>Captures the stack trace associated with the specified
    * {@code Throwable} object, decomposing it into a list of
    * stack frames.</p>
    *
    * <p>The result of this method vary by JDK version as this method
    * uses {@link Throwable# printStackTrace ( java.io.PrintWriter )}.
    * On JDK1.3 and earlier, the cause exception will not be shown
    * unless the specified throwable alters printStackTrace.</p>
    *
    * @param throwable the {@code Throwable} to examine, may be null
    * @return an array of strings describing each stack frame, never null
    */
  def getStackFrames(throwable: Throwable): Array[String] = {
    if (throwable == null) return ArrayUtils.EMPTY_STRING_ARRAY
    getStackFrames(getStackTrace(throwable))
  }

  /**
    * <p>Gets the stack trace from a Throwable as a String.</p>
    *
    * <p>The result of this method vary by JDK version as this method
    * uses {@link Throwable# printStackTrace ( java.io.PrintWriter )}.
    * On JDK1.3 and earlier, the cause exception will not be shown
    * unless the specified throwable alters printStackTrace.</p>
    *
    * @param throwable the {@code Throwable} to be examined
    * @return the stack trace as generated by the exception's
    *         {@code printStackTrace(PrintWriter)} method
    */
  def getStackTrace(throwable: Throwable): String = {
    val sw = new StringWriter
    val pw = new PrintWriter(sw, true)
    throwable.printStackTrace(pw)
    sw.getBuffer.toString
  }

  /**
    * <p>Counts the number of {@code Throwable} objects in the
    * exception chain.</p>
    *
    * <p>A throwable without cause will return {@code 1}.
    * A throwable with one cause will return {@code 2} and so on.
    * A {@code null} throwable will return {@code 0}.</p>
    *
    * <p>From version 2.2, this method handles recursive cause structures
    * that might otherwise cause infinite loops. The cause chain is
    * processed until the end is reached, or until the next item in the
    * chain is already in the result set.</p>
    *
    * @param throwable the throwable to inspect, may be null
    * @return the count of throwables, zero if null input
    */
  def getThrowableCount(throwable: Throwable): Int = getThrowableList(throwable).size

  /**
    * <p>Returns the list of {@code Throwable} objects in the
    * exception chain.</p>
    *
    * <p>A throwable without cause will return a list containing
    * one element - the input throwable.
    * A throwable with one cause will return a list containing
    * two elements. - the input throwable and the cause throwable.
    * A {@code null} throwable will return a list of size zero.</p>
    *
    * <p>This method handles recursive cause structures that might
    * otherwise cause infinite loops. The cause chain is processed until
    * the end is reached, or until the next item in the chain is already
    * in the result set.</p>
    *
    * @param throwable the throwable to inspect, may be null
    * @return the list of throwables, never null
    * @since 2.2
    */
  def getThrowableList(throwable: Throwable): util.List[Throwable] = {
    val list = new util.ArrayList[Throwable]

    var cause = throwable
    while (cause != null && !list.contains(cause)) {
      list.add(cause)
      cause = cause.getCause
    }

    list
  }

  /**
    * <p>Returns the list of {@code Throwable} objects in the
    * exception chain.</p>
    *
    * <p>A throwable without cause will return an array containing
    * one element - the input throwable.
    * A throwable with one cause will return an array containing
    * two elements. - the input throwable and the cause throwable.
    * A {@code null} throwable will return an array of size zero.</p>
    *
    * <p>From version 2.2, this method handles recursive cause structures
    * that might otherwise cause infinite loops. The cause chain is
    * processed until the end is reached, or until the next item in the
    * chain is already in the result set.</p>
    *
    * @see #getThrowableList(Throwable)
    * @param throwable the throwable to inspect, may be null
    * @return the array of throwables, never null
    */
  def getThrowables(throwable: Throwable): Array[Throwable] = {
    val list = getThrowableList(throwable)
    list.toArray(ArrayUtils.EMPTY_THROWABLE_ARRAY)
  }

  /**
    * Does the throwable's causal chain have an immediate or wrapped exception
    * of the given type?
    *
    * @param chain
    * The root of a Throwable causal chain.
    * @param type
    * The exception type to test.
    * @return true, if chain is an instance of type or is an
    *         UndeclaredThrowableException wrapping a cause.
    * @since 3.5
    * @see #wrapAndThrow(Throwable)
    */
  def hasCause(chain: Throwable, `type`: Class[_ <: Throwable]): Boolean = {
    val c =
      if (chain.isInstanceOf[UndeclaredThrowableException]) chain.getCause
      else chain
    `type`.isInstance(c)
  }

  /**
    * <p>Worker method for the {@code indexOfType} methods.</p>
    *
    * @param throwable the throwable to inspect, may be null
    * @param type      the type to search for, subclasses match, null returns -1
    * @param fromIndex the (zero-based) index of the starting position,
    *                  negative treated as zero, larger than chain size returns -1
    * @param subclass  if {@code true}, compares with {@link Class# isAssignableFrom ( Class )}, otherwise compares
    *                  using references
    * @return index of the {@code type} within throwables nested within the specified {@code throwable}
    */
  private def indexOf(throwable: Throwable, `type`: Class[_ <: Throwable], fromIndex: Int, subclass: Boolean): Int = {
    if (throwable == null || `type` == null) return NOT_FOUND
    val startIndex: Int = if (fromIndex < 0) 0 else fromIndex

    val throwables = getThrowables(throwable)
    if (startIndex >= throwables.length) return NOT_FOUND
    if (subclass) for (i <- startIndex until throwables.length) {
      if (`type`.isAssignableFrom(throwables(i).getClass)) return i
    }
    else
      for (i <- startIndex until throwables.length) {
        if (`type` == throwables(i).getClass) return i
      }
    NOT_FOUND
  }

  /**
    * <p>Returns the (zero-based) index of the first {@code Throwable}
    * that matches the specified class (exactly) in the exception chain.
    * Subclasses of the specified class do not match - see
    * {@link #indexOfType ( Throwable, Class)} for the opposite.</p>
    *
    * <p>A {@code null} throwable returns {@code -1}.
    * A {@code null} type returns {@code -1}.
    * No match in the chain returns {@code -1}.</p>
    *
    * @param throwable the throwable to inspect, may be null
    * @param clazz     the class to search for, subclasses do not match, null returns -1
    * @return the index into the throwable chain, -1 if no match or null input
    */
  def indexOfThrowable(throwable: Throwable, clazz: Class[_ <: Throwable]): Int = indexOf(throwable, clazz, 0, false)

  /**
    * <p>Returns the (zero-based) index of the first {@code Throwable}
    * that matches the specified type in the exception chain from
    * a specified index.
    * Subclasses of the specified class do not match - see
    * {@link #indexOfType ( Throwable, Class, int)} for the opposite.</p>
    *
    * <p>A {@code null} throwable returns {@code -1}.
    * A {@code null} type returns {@code -1}.
    * No match in the chain returns {@code -1}.
    * A negative start index is treated as zero.
    * A start index greater than the number of throwables returns {@code -1}.</p>
    *
    * @param throwable the throwable to inspect, may be null
    * @param clazz     the class to search for, subclasses do not match, null returns -1
    * @param fromIndex the (zero-based) index of the starting position,
    *                  negative treated as zero, larger than chain size returns -1
    * @return the index into the throwable chain, -1 if no match or null input
    */
  def indexOfThrowable(throwable: Throwable, clazz: Class[_ <: Throwable], fromIndex: Int): Int =
    indexOf(throwable, clazz, fromIndex, false)

  /**
    * <p>Returns the (zero-based) index of the first {@code Throwable}
    * that matches the specified class or subclass in the exception chain.
    * Subclasses of the specified class do match - see
    * {@link #indexOfThrowable ( Throwable, Class)} for the opposite.</p>
    *
    * <p>A {@code null} throwable returns {@code -1}.
    * A {@code null} type returns {@code -1}.
    * No match in the chain returns {@code -1}.</p>
    *
    * @param throwable the throwable to inspect, may be null
    * @param type      the type to search for, subclasses match, null returns -1
    * @return the index into the throwable chain, -1 if no match or null input
    * @since 2.1
    */
  def indexOfType(throwable: Throwable, `type`: Class[_ <: Throwable]): Int = indexOf(throwable, `type`, 0, true)

  /**
    * <p>Returns the (zero-based) index of the first {@code Throwable}
    * that matches the specified type in the exception chain from
    * a specified index.
    * Subclasses of the specified class do match - see
    * {@link #indexOfThrowable ( Throwable, Class)} for the opposite.</p>
    *
    * <p>A {@code null} throwable returns {@code -1}.
    * A {@code null} type returns {@code -1}.
    * No match in the chain returns {@code -1}.
    * A negative start index is treated as zero.
    * A start index greater than the number of throwables returns {@code -1}.</p>
    *
    * @param throwable the throwable to inspect, may be null
    * @param type      the type to search for, subclasses match, null returns -1
    * @param fromIndex the (zero-based) index of the starting position,
    *                  negative treated as zero, larger than chain size returns -1
    * @return the index into the throwable chain, -1 if no match or null input
    * @since 2.1
    */
  def indexOfType(throwable: Throwable, `type`: Class[_ <: Throwable], fromIndex: Int): Int =
    indexOf(throwable, `type`, fromIndex, true)

  /**
    * <p>Prints a compact stack trace for the root cause of a throwable
    * to {@code System.err}.</p>
    *
    * <p>The compact stack trace starts with the root cause and prints
    * stack frames up to the place where it was caught and wrapped.
    * Then it prints the wrapped exception and continues with stack frames
    * until the wrapper exception is caught and wrapped again, etc.</p>
    *
    * <p>The output of this method is consistent across JDK versions.
    * Note that this is the opposite order to the JDK1.4 display.</p>
    *
    * <p>The method is equivalent to {@code printStackTrace} for throwables
    * that don't have nested causes.</p>
    *
    * @param throwable the throwable to output
    * @since 2.0
    */
  def printRootCauseStackTrace(throwable: Throwable): Unit = {
    printRootCauseStackTrace(throwable, System.err)
  }

  /**
    * <p>Prints a compact stack trace for the root cause of a throwable.</p>
    *
    * <p>The compact stack trace starts with the root cause and prints
    * stack frames up to the place where it was caught and wrapped.
    * Then it prints the wrapped exception and continues with stack frames
    * until the wrapper exception is caught and wrapped again, etc.</p>
    *
    * <p>The output of this method is consistent across JDK versions.
    * Note that this is the opposite order to the JDK1.4 display.</p>
    *
    * <p>The method is equivalent to {@code printStackTrace} for throwables
    * that don't have nested causes.</p>
    *
    * @param throwable   the throwable to output, may be null
    * @param printStream the stream to output to, may not be null
    * @throws NullPointerException if the printStream is {@code null}
    * @since 2.0
    */
  @SuppressWarnings(Array("resource")) def printRootCauseStackTrace(
    throwable: Throwable,
    printStream: PrintStream): Unit = {
    if (throwable == null) return
    Objects.requireNonNull(printStream, "printStream")
    val trace = getRootCauseStackTrace(throwable)
    for (element <- trace) {
      printStream.println(element)
    }
    printStream.flush()
  }

  /**
    * <p>Prints a compact stack trace for the root cause of a throwable.</p>
    *
    * <p>The compact stack trace starts with the root cause and prints
    * stack frames up to the place where it was caught and wrapped.
    * Then it prints the wrapped exception and continues with stack frames
    * until the wrapper exception is caught and wrapped again, etc.</p>
    *
    * <p>The output of this method is consistent across JDK versions.
    * Note that this is the opposite order to the JDK1.4 display.</p>
    *
    * <p>The method is equivalent to {@code printStackTrace} for throwables
    * that don't have nested causes.</p>
    *
    * @param throwable   the throwable to output, may be null
    * @param printWriter the writer to output to, may not be null
    * @throws NullPointerException if the printWriter is {@code null}
    * @since 2.0
    */
  @SuppressWarnings(Array("resource")) def printRootCauseStackTrace(
    throwable: Throwable,
    printWriter: PrintWriter): Unit = {
    if (throwable == null) return
    Objects.requireNonNull(printWriter, "printWriter")
    val trace = getRootCauseStackTrace(throwable)
    for (element <- trace) {
      printWriter.println(element)
    }
    printWriter.flush()
  }

  /**
    * <p>Removes common frames from the cause trace given the two stack traces.</p>
    *
    * @param causeFrames   stack trace of a cause throwable
    * @param wrapperFrames stack trace of a wrapper throwable
    * @throws IllegalArgumentException if either argument is null
    * @since 2.0
    */
  def removeCommonFrames(causeFrames: util.List[String], wrapperFrames: util.List[String]): Unit = {
    if (causeFrames == null || wrapperFrames == null) throw new IllegalArgumentException("The List must not be null")
    var causeFrameIndex = causeFrames.size - 1
    var wrapperFrameIndex = wrapperFrames.size - 1
    while ({
      causeFrameIndex >= 0 && wrapperFrameIndex >= 0
    }) { // Remove the frame from the cause trace if it is the same
      // as in the wrapper trace
      val causeFrame = causeFrames.get(causeFrameIndex)
      val wrapperFrame = wrapperFrames.get(wrapperFrameIndex)
      if (causeFrame == wrapperFrame) causeFrames.remove(causeFrameIndex)
      causeFrameIndex -= 1
      wrapperFrameIndex -= 1
    }
  }

  /**
    * Throw a checked exception without adding the exception to the throws
    * clause of the calling method. This method prevents throws clause
    * pollution and reduces the clutter of "Caused by" exceptions in the
    * stacktrace.
    * <p>
    * The use of this technique may be controversial, but exceedingly useful to
    * library developers.
    * <code>
    * public int propagateExample { // note that there is no throws clause
    * try {
    * return invocation(); // throws IOException
    * } catch (Exception e) {
    * return ExceptionUtils.rethrow(e);  // propagates a checked exception
    * }
    * }
    * </code>
    * <p>
    * This is an alternative to the more conservative approach of wrapping the
    * checked exception in a RuntimeException:
    * <code>
    * public int wrapExample { // note that there is no throws clause
    * try {
    * return invocation(); // throws IOException
    * } catch (Error e) {
    * throw e;
    * } catch (RuntimeException e) {
    * throw e;  // wraps a checked exception
    * } catch (Exception e) {
    * throw new UndeclaredThrowableException(e);  // wraps a checked exception
    * }
    * }
    * </code>
    * <p>
    * One downside to using this approach is that the java compiler will not
    * allow invoking code to specify a checked exception in a catch clause
    * unless there is some code path within the try block that has invoked a
    * method declared with that checked exception. If the invoking site wishes
    * to catch the shaded checked exception, it must either invoke the shaded
    * code through a method re-declaring the desired checked exception, or
    * catch Exception and use the instanceof operator. Either of these
    * techniques are required when interacting with non-java jvm code such as
    * Jython, Scala, or Groovy, since these languages do not consider any
    * exceptions as checked.
    *
    * @param throwable
    *          The throwable to rethrow.
    * @param < R> The type of the returned value.
    * @return Never actually returned, this generic type matches any type
    *         which the calling site requires. "Returning" the results of this
    *         method, as done in the propagateExample above, will satisfy the
    *         java compiler requirement that all code paths return a value.
    * @since 3.5
    * @see #wrapAndThrow(Throwable)
    */
  def rethrow[R](throwable: Throwable): R = { // claim that the typeErasure invocation throws a RuntimeException
    ExceptionUtils.typeErasure[R, RuntimeException](throwable)
  }

  /**
    * <p>Worker method for the {@code throwableOfType} methods.</p>
    *
    * @param <         T> the type of Throwable you are searching.
    * @param throwable the throwable to inspect, may be null
    * @param type      the type to search, subclasses match, null returns null
    * @param fromIndex the (zero-based) index of the starting position,
    *                  negative treated as zero, larger than chain size returns null
    * @param subclass  if {@code true}, compares with {@link Class# isAssignableFrom ( Class )}, otherwise compares
    *                  using references
    * @return throwable of the {@code type} within throwables nested within the specified {@code throwable}
    */
  private def throwableOf[T <: Throwable](
    throwable: Throwable,
    `type`: Class[T],
    fromIndex: Int,
    subclass: Boolean): T = {
    if (throwable == null || `type` == null) return null.asInstanceOf[T]

    val startIndex: Int = if (fromIndex < 0) 0 else fromIndex
    val throwables = getThrowables(throwable)
    if (startIndex >= throwables.length) return null.asInstanceOf[T]

    if (subclass) for (i <- startIndex until throwables.length) {
      if (`type`.isAssignableFrom(throwables(i).getClass)) return `type`.cast(throwables(i))
    }
    else
      for (i <- startIndex until throwables.length) {
        if (`type` == throwables(i).getClass) return `type`.cast(throwables(i))
      }

    null.asInstanceOf[T]
  }

  /**
    * <p>Returns the first {@code Throwable}
    * that matches the specified class (exactly) in the exception chain.
    * Subclasses of the specified class do not match - see
    * {@link #throwableOfType ( Throwable, Class)} for the opposite.</p>
    *
    * <p>A {@code null} throwable returns {@code null}.
    * A {@code null} type returns {@code null}.
    * No match in the chain returns {@code null}.</p>
    *
    * @param <         T> the type of Throwable you are searching.
    * @param throwable the throwable to inspect, may be null
    * @param clazz     the class to search for, subclasses do not match, null returns null
    * @return the first matching throwable from the throwable chain, null if no match or null input
    * @since 3.10
    */
  def throwableOfThrowable[T <: Throwable](throwable: Throwable, clazz: Class[T]): T =
    throwableOf(throwable, clazz, 0, false)

  /**
    * <p>Returns the first {@code Throwable}
    * that matches the specified type in the exception chain from
    * a specified index.
    * Subclasses of the specified class do not match - see
    * {@link #throwableOfType ( Throwable, Class, int)} for the opposite.</p>
    *
    * <p>A {@code null} throwable returns {@code null}.
    * A {@code null} type returns {@code null}.
    * No match in the chain returns {@code null}.
    * A negative start index is treated as zero.
    * A start index greater than the number of throwables returns {@code null}.</p>
    *
    * @param <         T> the type of Throwable you are searching.
    * @param throwable the throwable to inspect, may be null
    * @param clazz     the class to search for, subclasses do not match, null returns null
    * @param fromIndex the (zero-based) index of the starting position,
    *                  negative treated as zero, larger than chain size returns null
    * @return the first matching throwable from the throwable chain, null if no match or null input
    * @since 3.10
    */
  def throwableOfThrowable[T <: Throwable](throwable: Throwable, clazz: Class[T], fromIndex: Int): T =
    throwableOf(throwable, clazz, fromIndex, false)

  /**
    * <p>Returns the throwable of the first {@code Throwable}
    * that matches the specified class or subclass in the exception chain.
    * Subclasses of the specified class do match - see
    * {@link #throwableOfThrowable ( Throwable, Class)} for the opposite..</p>
    *
    * <p>A {@code null} throwable returns {@code null}.
    * A {@code null} type returns {@code null}.
    * No match in the chain returns {@code null}.</p>
    *
    * @param <         T> the type of Throwable you are searching.
    * @param throwable the throwable to inspect, may be null
    * @param type      the type to search for, subclasses match, null returns null
    * @return the first matching throwable from the throwable chain, null if no match or null input
    * @since 3.10
    */
  def throwableOfType[T <: Throwable](throwable: Throwable, `type`: Class[T]): T =
    throwableOf(throwable, `type`, 0, true)

  /**
    * <p>Returns the first {@code Throwable}
    * that matches the specified type in the exception chain from
    * a specified index.
    * Subclasses of the specified class do match - see
    * {@link #throwableOfThrowable ( Throwable, Class)} for the opposite.</p>
    *
    * <p>A {@code null} throwable returns {@code null}.
    * A {@code null} type returns {@code null}.
    * No match in the chain returns {@code null}.
    * A negative start index is treated as zero.
    * A start index greater than the number of throwables returns {@code null}.</p>
    *
    * @param <         T> the type of Throwable you are searching.
    * @param throwable the throwable to inspect, may be null
    * @param type      the type to search for, subclasses match, null returns null
    * @param fromIndex the (zero-based) index of the starting position,
    *                  negative treated as zero, larger than chain size returns null
    * @return the first matching throwable from the throwable chain, null if no match or null input
    * @since 3.10
    */
  def throwableOfType[T <: Throwable](throwable: Throwable, `type`: Class[T], fromIndex: Int): T =
    throwableOf(throwable, `type`, fromIndex, true)

  /**
    * Claim a Throwable is another Exception type using type erasure. This
    * hides a checked exception from the java compiler, allowing a checked
    * exception to be thrown without having the exception in the method's throw
    * clause.
    */
  @SuppressWarnings(Array("unchecked"))
  //@throws[T]
  private def typeErasure[R, T <: Throwable](throwable: Throwable) =
    throw throwable.asInstanceOf[T]

  /**
    * Throw a checked exception without adding the exception to the throws
    * clause of the calling method. For checked exceptions, this method throws
    * an UndeclaredThrowableException wrapping the checked exception. For
    * Errors and RuntimeExceptions, the original exception is rethrown.
    * <p>
    * The downside to using this approach is that invoking code which needs to
    * handle specific checked exceptions must sniff up the exception chain to
    * determine if the caught exception was caused by the checked exception.
    *
    * @param throwable
    *          The throwable to rethrow.
    * @param < R> The type of the returned value.
    * @return Never actually returned, this generic type matches any type
    *         which the calling site requires. "Returning" the results of this
    *         method will satisfy the java compiler requirement that all code
    *         paths return a value.
    * @since 3.5
    * @see #rethrow(Throwable)
    * @see #hasCause(Throwable, Class)
    */
  def wrapAndThrow[R](throwable: Throwable): R = {
    if (throwable.isInstanceOf[RuntimeException]) throw throwable.asInstanceOf[RuntimeException]
    if (throwable.isInstanceOf[Error]) throw throwable.asInstanceOf[Error]
    throw new UndeclaredThrowableException(throwable)
  }
}
