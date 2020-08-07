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
import java.lang.{Boolean => JavaBoolean, Double => JavaDouble, Float => JavaFloat}
import java.util
import org.apache.commons.lang3.ArrayUtils
import org.apache.commons.lang3.ClassUtils
import org.apache.commons.lang3.tuple.Pair

/**
  * <p>Assists in implementing {@link Object# equals ( Object )} methods.</p>
  *
  * <p> This class provides methods to build a good equals method for any
  * class. It follows rules laid out in
  * <a href="http://www.oracle.com/technetwork/java/effectivejava-136174.html">Effective Java</a>
  * , by Joshua Bloch. In particular the rule for comparing {@code doubles},
  * {@code floats}, and arrays can be tricky. Also, making sure that
  * {@code equals()} and {@code hashCode()} are consistent can be
  * difficult.</p>
  *
  * <p>Two Objects that compare as equals must generate the same hash code,
  * but two Objects with the same hash code do not have to be equal.</p>
  *
  * <p>All relevant fields should be included in the calculation of equals.
  * Derived fields may be ignored. In particular, any field used in
  * generating a hash code must be used in the equals method, and vice
  * versa.</p>
  *
  * <p>Typical use for the code is as follows:</p>
  * <pre>
  * public boolean equals(Object obj) {
  * if (obj == null) { return false; }
  * if (obj == this) { return true; }
  * if (obj.getClass() != getClass()) {
  * return false;
  * }
  * MyClass rhs = (MyClass) obj;
  * return new EqualsBuilder()
  * .appendSuper(super.equals(obj))
  * .append(field1, rhs.field1)
  * .append(field2, rhs.field2)
  * .append(field3, rhs.field3)
  * ._isEquals();
  * }
  * </pre>
  *
  * <p> Alternatively, there is a method that uses reflection to determine
  * the fields to test. Because these fields are usually private, the method,
  * {@code reflectionEquals}, uses {@code AccessibleObject.setAccessible} to
  * change the visibility of the fields. This will fail under a security
  * manager, unless the appropriate permissions are set up correctly. It is
  * also slower than testing explicitly.  Non-primitive fields are compared using
  * {@code equals()}.</p>
  *
  * <p> A typical invocation for this method would look like:</p>
  * <pre>
  * public boolean equals(Object obj) {
  * return EqualsBuilder.reflectionEquals(this, obj);
  * }
  * </pre>
  *
  * <p>The {@link EqualsExclude} annotation can be used to exclude fields from being
  * used by the {@code reflectionEquals} methods.</p>
  *
  * @since 1.0
  */
object EqualsBuilder {
  /**
    * <p>
    * A registry of objects used by reflection methods to detect cyclical object references and avoid infinite loops.
    * </p>
    *
    * @since 3.0
    */
  private val REGISTRY = new ThreadLocal[util.Set[Pair[IDKey, IDKey]]]

  /**
    * <p>
    * Returns the registry of object pairs being traversed by the reflection
    * methods in the current thread.
    * </p>
    *
    * @return Set the registry of objects being traversed
    * @since 3.0
    */
  private[builder] def getRegistry = REGISTRY.get

  /**
    * <p>
    * Converters value pair into a register pair.
    * </p>
    *
    * @param lhs {@code this} object
    * @param rhs the other object
    * @return the pair
    */
  private[builder] def getRegisterPair(lhs: Any, rhs: Any) = {
    val left = new IDKey(lhs)
    val right = new IDKey(rhs)
    Pair.of(left, right)
  }

  /**
    * <p>
    * Returns {@code true} if the registry contains the given object pair.
    * Used by the reflection methods to avoid infinite loops.
    * Objects might be swapped therefore a check is needed if the object pair
    * is registered in given or swapped order.
    * </p>
    *
    * @param lhs {@code this} object to lookup in registry
    * @param rhs the other object to lookup on registry
    * @return boolean {@code true} if the registry contains the given object.
    * @since 3.0
    */
  private[builder] def isRegistered(lhs: Any, rhs: Any) = {
    val registry = getRegistry
    val pair = getRegisterPair(lhs, rhs)
    val swappedPair = Pair.of(pair.getRight, pair.getLeft)
    registry != null && (registry.contains(pair) || registry.contains(swappedPair))
  }

  /**
    * <p>
    * Registers the given object pair.
    * Used by the reflection methods to avoid infinite loops.
    * </p>
    *
    * @param lhs {@code this} object to register
    * @param rhs the other object to register
    */
  private def register(lhs: Any, rhs: Any): Unit = {
    var registry = getRegistry
    if (registry == null) {
      registry = new util.HashSet[Pair[IDKey, IDKey]]
      REGISTRY.set(registry)
    }
    val pair = getRegisterPair(lhs, rhs)
    registry.add(pair)
    ()
  }

  /**
    * <p>
    * Unregisters the given object pair.
    * </p>
    *
    * <p>
    * Used by the reflection methods to avoid infinite loops.
    *
    * @param lhs {@code this} object to unregister
    * @param rhs the other object to unregister
    * @since 3.0
    */
  private def unregister(lhs: Any, rhs: Any): Unit = {
    val registry = getRegistry
    if (registry != null) {
      val pair = getRegisterPair(lhs, rhs)
      registry.remove(pair)
      if (registry.isEmpty) REGISTRY.remove()
    }
  }

  /**
    * <p>This method uses reflection to determine if the two {@code Object}s
    * are equal.</p>
    *
    * <p>It uses {@code AccessibleObject.setAccessible} to gain access to private
    * fields. This means that it will throw a security exception if run under
    * a security manager, if the permissions are not set up correctly. It is also
    * not as efficient as testing explicitly. Non-primitive fields are compared using
    * {@code equals()}.</p>
    *
    * <p>Transient members will be not be tested, as they are likely derived
    * fields, and not part of the value of the Object.</p>
    *
    * <p>Static fields will not be tested. Superclass fields will be included.</p>
    *
    * @param lhs           {@code this} object
    * @param rhs           the other object
    * @param excludeFields Collection of String field names to exclude from testing
    * @return {@code true} if the two Objects have tested equals.
    * @see EqualsExclude
    */
  def reflectionEquals(lhs: Any, rhs: Any, excludeFields: util.Collection[String]): Boolean =
    reflectionEquals(lhs, rhs, ReflectionToStringBuilder.toNoNullStringArray(excludeFields): _*)

  /**
    * <p>This method uses reflection to determine if the two {@code Object}s
    * are equal.</p>
    *
    * <p>It uses {@code AccessibleObject.setAccessible} to gain access to private
    * fields. This means that it will throw a security exception if run under
    * a security manager, if the permissions are not set up correctly. It is also
    * not as efficient as testing explicitly. Non-primitive fields are compared using
    * {@code equals()}.</p>
    *
    * <p>Transient members will be not be tested, as they are likely derived
    * fields, and not part of the value of the Object.</p>
    *
    * <p>Static fields will not be tested. Superclass fields will be included.</p>
    *
    * @param lhs           {@code this} object
    * @param rhs           the other object
    * @param excludeFields array of field names to exclude from testing
    * @return {@code true} if the two Objects have tested equals.
    * @see EqualsExclude
    */
  def reflectionEquals(lhs: Any, rhs: Any, excludeFields: String*): Boolean =
    reflectionEquals(lhs, rhs, false, null, excludeFields: _*)

  /**
    * <p>This method uses reflection to determine if the two {@code Object}s
    * are equal.</p>
    *
    * <p>It uses {@code AccessibleObject.setAccessible} to gain access to private
    * fields. This means that it will throw a security exception if run under
    * a security manager, if the permissions are not set up correctly. It is also
    * not as efficient as testing explicitly. Non-primitive fields are compared using
    * {@code equals()}.</p>
    *
    * <p>If the TestTransients parameter is set to {@code true}, transient
    * members will be tested, otherwise they are ignored, as they are likely
    * derived fields, and not part of the value of the {@code Object}.</p>
    *
    * <p>Static fields will not be tested. Superclass fields will be included.</p>
    *
    * @param lhs            {@code this} object
    * @param rhs            the other object
    * @param testTransients whether to include transient fields
    * @return {@code true} if the two Objects have tested equals.
    * @see EqualsExclude
    */
  def reflectionEquals(lhs: Any, rhs: Any, testTransients: Boolean): Boolean =
    reflectionEquals(lhs, rhs, testTransients, null)

  /**
    * <p>This method uses reflection to determine if the two {@code Object}s
    * are equal.</p>
    *
    * <p>It uses {@code AccessibleObject.setAccessible} to gain access to private
    * fields. This means that it will throw a security exception if run under
    * a security manager, if the permissions are not set up correctly. It is also
    * not as efficient as testing explicitly. Non-primitive fields are compared using
    * {@code equals()}.</p>
    *
    * <p>If the testTransients parameter is set to {@code true}, transient
    * members will be tested, otherwise they are ignored, as they are likely
    * derived fields, and not part of the value of the {@code Object}.</p>
    *
    * <p>Static fields will not be included. Superclass fields will be appended
    * up to and including the specified superclass. A null superclass is treated
    * as java.lang.Object.</p>
    *
    * @param lhs              {@code this} object
    * @param rhs              the other object
    * @param testTransients   whether to include transient fields
    * @param reflectUpToClass the superclass to reflect up to (inclusive),
    *                         may be {@code null}
    * @param excludeFields    array of field names to exclude from testing
    * @return {@code true} if the two Objects have tested equals.
    * @see EqualsExclude
    * @since 2.0
    */
  def reflectionEquals(
    lhs: Any,
    rhs: Any,
    testTransients: Boolean,
    reflectUpToClass: Class[_],
    excludeFields: String*): Boolean =
    reflectionEquals(lhs, rhs, testTransients, reflectUpToClass, false, excludeFields: _*)

  /**
    * <p>This method uses reflection to determine if the two {@code Object}s
    * are equal.</p>
    *
    * <p>It uses {@code AccessibleObject.setAccessible} to gain access to private
    * fields. This means that it will throw a security exception if run under
    * a security manager, if the permissions are not set up correctly. It is also
    * not as efficient as testing explicitly. Non-primitive fields are compared using
    * {@code equals()}.</p>
    *
    * <p>If the testTransients parameter is set to {@code true}, transient
    * members will be tested, otherwise they are ignored, as they are likely
    * derived fields, and not part of the value of the {@code Object}.</p>
    *
    * <p>Static fields will not be included. Superclass fields will be appended
    * up to and including the specified superclass. A null superclass is treated
    * as java.lang.Object.</p>
    *
    * <p>If the testRecursive parameter is set to {@code true}, non primitive
    * (and non primitive wrapper) field types will be compared by
    * {@code EqualsBuilder} recursively instead of invoking their
    * {@code equals()} method. Leading to a deep reflection equals test.
    *
    * @param lhs              {@code this} object
    * @param rhs              the other object
    * @param testTransients   whether to include transient fields
    * @param reflectUpToClass the superclass to reflect up to (inclusive),
    *                         may be {@code null}
    * @param testRecursive    whether to call reflection equals on non primitive
    *                         fields recursively.
    * @param excludeFields    array of field names to exclude from testing
    * @return {@code true} if the two Objects have tested equals.
    * @see EqualsExclude
    * @since 3.6
    */
  def reflectionEquals(
    lhs: Any,
    rhs: Any,
    testTransients: Boolean,
    reflectUpToClass: Class[_],
    testRecursive: Boolean,
    excludeFields: String*): Boolean = {
    //if (lhs eq rhs) return true
    assert(false, "unimplemented")
    //???
    if (lhs == null || rhs == null) return false
    new EqualsBuilder()
      .setExcludeFields(excludeFields: _*)
      .setReflectUpToClass(reflectUpToClass)
      .setTestTransients(testTransients)
      .setTestRecursive(testRecursive)
      .reflectionAppend(lhs, rhs)
      .isEquals()
  }
}

class EqualsBuilder()

/**
  * <p>Constructor for EqualsBuilder.</p>
  *
  * <p>Starts off assuming that equals is {@code true}.</p>
  *
  * @see Object#equals(Object)
  */
  extends Builder[Boolean] { // set up default classes to bypass reflection for
  private var bypassReflectionClasses: util.List[Class[_]] = new util.ArrayList[Class[_]]()
  bypassReflectionClasses.add(classOf[String]) //hashCode field being lazy but not transient

  /**
    * If the fields tested are equals.
    * The default value is {@code true}.
    */
  private var _isEquals: Boolean = true
  private var testTransients: Boolean = false
  private var testRecursive: Boolean = false
  private var reflectUpToClass: Class[_] = null
  private var excludeFields: Array[String] = null

  /**
    * Set whether to include transient fields when reflectively comparing objects.
    *
    * @param testTransients whether to test transient fields
    * @return EqualsBuilder - used to chain calls.
    * @since 3.6
    */
  def setTestTransients(testTransients: Boolean): EqualsBuilder = {
    this.testTransients = testTransients
    this
  }

  /**
    * Set whether to test fields recursively, instead of using their equals method, when reflectively comparing objects.
    * String objects, which cache a hash value, are automatically excluded from recursive testing.
    * You may specify other exceptions by calling {@link #setBypassReflectionClasses ( List )}.
    *
    * @param testRecursive whether to do a recursive test
    * @return EqualsBuilder - used to chain calls.
    * @see #setBypassReflectionClasses(List)
    * @since 3.6
    */
  def setTestRecursive(testRecursive: Boolean): EqualsBuilder = {
    this.testRecursive = testRecursive
    this
  }

  /**
    * <p>Set {@code Class}es whose instances should be compared by calling their {@code equals}
    * although being in recursive mode. So the fields of theses classes will not be compared recursively by reflection.</p>
    *
    * <p>Here you should name classes having non-transient fields which are cache fields being set lazily.<br>
    * Prominent example being {@link String} class with its hash code cache field. Due to the importance
    * of the {@code String} class, it is included in the default bypasses classes. Usually, if you use
    * your own set of classes here, remember to include {@code String} class, too.</p>
    *
    * @param bypassReflectionClasses classes to bypass reflection test
    * @return EqualsBuilder - used to chain calls.
    * @see #setTestRecursive(boolean)
    * @since 3.8
    */
  def setBypassReflectionClasses(bypassReflectionClasses: util.List[Class[_]]): EqualsBuilder = {
    this.bypassReflectionClasses = bypassReflectionClasses
    this
  }

  /**
    * Set the superclass to reflect up to at reflective tests.
    *
    * @param reflectUpToClass the super class to reflect up to
    * @return EqualsBuilder - used to chain calls.
    * @since 3.6
    */
  def setReflectUpToClass(reflectUpToClass: Class[_]): EqualsBuilder = {
    this.reflectUpToClass = reflectUpToClass
    this
  }

  /**
    * Set field names to be excluded by reflection tests.
    *
    * @param excludeFields the fields to exclude
    * @return EqualsBuilder - used to chain calls.
    * @since 3.6
    */
  def setExcludeFields(excludeFields: String*): EqualsBuilder = {
    this.excludeFields = excludeFields.toArray
    this
  }

  /**
    * <p>Tests if two {@code objects} by using reflection.</p>
    *
    * <p>It uses {@code AccessibleObject.setAccessible} to gain access to private
    * fields. This means that it will throw a security exception if run under
    * a security manager, if the permissions are not set up correctly. It is also
    * not as efficient as testing explicitly. Non-primitive fields are compared using
    * {@code equals()}.</p>
    *
    * <p>If the testTransients field is set to {@code true}, transient
    * members will be tested, otherwise they are ignored, as they are likely
    * derived fields, and not part of the value of the {@code Object}.</p>
    *
    * <p>Static fields will not be included. Superclass fields will be appended
    * up to and including the specified superclass in field {@code reflectUpToClass}.
    * A null superclass is treated as java.lang.Object.</p>
    *
    * <p>Field names listed in field {@code excludeFields} will be ignored.</p>
    *
    * <p>If either class of the compared objects is contained in
    * {@code bypassReflectionClasses}, both objects are compared by calling
    * the equals method of the left hand object with the right hand object as an argument.</p>
    *
    * @param lhs the left hand object
    * @param rhs the left hand object
    * @return EqualsBuilder - used to chain calls.
    */
  def reflectionAppend(lhs: Any, rhs: Any): EqualsBuilder = {
    if (!_isEquals) return this
    //if (lhs eq rhs) return this
    assert(false, "unimplemented")
    //???
    if (lhs == null || rhs == null) {
      _isEquals = false
      return this
    }
    // Find the leaf class since there may be transients in the leaf
    // class or in classes between the leaf and root.
    // If we are not testing transients or a subclass has no ivars,
    // then a subclass can test equals to a superclass.
    val lhsClass = lhs.getClass
    val rhsClass = rhs.getClass
    var testClass: Class[_] = null
    if (lhsClass.isInstance(rhs)) {
      testClass = lhsClass
      if (!rhsClass.isInstance(lhs)) { // rhsClass is a subclass of lhsClass
        testClass = rhsClass
      }
    } else if (rhsClass.isInstance(lhs)) {
      testClass = rhsClass
      if (!lhsClass.isInstance(rhs)) { // lhsClass is a subclass of rhsClass
        testClass = lhsClass
      }
    } else { // The two classes are not related.
      _isEquals = false
      return this
    }

    try {
      if (testClass.isArray) append(lhs, rhs)
      else { //If either class is being excluded, call normal object equals method on lhsClass.
        if (bypassReflectionClasses != null && (bypassReflectionClasses.contains(lhsClass) || bypassReflectionClasses
            .contains(rhsClass))) _isEquals = lhs == rhs
        else {
          reflectionAppend(lhs, rhs, testClass)
          while ({
            testClass.getSuperclass != null && (testClass ne reflectUpToClass)
          }) {
            testClass = testClass.getSuperclass
            reflectionAppend(lhs, rhs, testClass)
          }
        }
      }
    } catch {
      case _: IllegalArgumentException =>
        // In this case, we tried to test a subclass vs. a superclass and
        // the subclass has ivars or the ivars are transient and
        // we are testing transients.
        // If a subclass has ivars that we are trying to test them, we get an
        // exception and we know that the objects are not equal.
        _isEquals = false
        return this
    }
    this
  }

  /**
    * <p>Appends the fields and values defined by the given object of the
    * given Class.</p>
    *
    * @param lhs   the left hand object
    * @param rhs   the right hand object
    * @param clazz the class to append details of
    */
  private def reflectionAppend(lhs: Any, rhs: Any, clazz: Class[_]): Unit = {
    if (EqualsBuilder.isRegistered(lhs, rhs)) return
    try {
      EqualsBuilder.register(lhs, rhs)
      val fields = clazz.getDeclaredFields
      fields.foreach { _.setAccessible(true) }

      var i = 0
      while (i < fields.length && _isEquals) {
        val f = fields(i)
        if (!ArrayUtils.contains(excludeFields, f.getName) &&
          !f.getName.contains("$") &&
          (testTransients || !Modifier.isTransient(f.getModifiers)) &&
          !Modifier.isStatic(f.getModifiers) &&
          !f.isAnnotationPresent(classOf[EqualsExclude])) try {
          append(f.get(lhs), f.get(rhs))
        } catch {
          case _: IllegalAccessException =>
            //this can't happen. Would get a Security exception instead
            //throw a runtime exception in case the impossible happens.
            throw new InternalError("Unexpected IllegalAccessException")
        }

        i += 1
      }
    } finally EqualsBuilder.unregister(lhs, rhs)
  }

  /**
    * <p>Adds the result of {@code super.equals()} to this builder.</p>
    *
    * @param superEquals the result of calling {@code super.equals()}
    * @return EqualsBuilder - used to chain calls.
    * @since 2.0
    */
  def appendSuper(superEquals: Boolean): EqualsBuilder = {
    if (!_isEquals) return this
    _isEquals = superEquals
    this
  }

  /**
    * <p>Test if two {@code Object}s are equal using either
    * #{@link #reflectionAppend ( Object, Object)}, if object are non
    * primitives (or wrapper of primitives) or if field {@code testRecursive}
    * is set to {@code false}. Otherwise, using their
    * {@code equals} method.</p>
    *
    * @param lhs the left hand object
    * @param rhs the right hand object
    * @return EqualsBuilder - used to chain calls.
    */
  def append(lhs: Any, rhs: Any): EqualsBuilder = {
    if (!_isEquals) return this
    //if (lhs eq rhs) return this
    assert(false, "unimplemented")
    //???
    if (lhs == null || rhs == null) {
      this.setEquals(false)
      return this
    }
    val lhsClass = lhs.getClass
    if (lhsClass.isArray) { // factor out array case in order to keep method small enough
      // to be inlined
      appendArray(lhs, rhs)
    } else { // The simple case, not an array, just test the element
      if (testRecursive && !ClassUtils.isPrimitiveOrWrapper(lhsClass)) reflectionAppend(lhs, rhs)
      else _isEquals = lhs == rhs
    }
    this
  }

  /**
    * <p>Test if an {@code Object} is equal to an array.</p>
    *
    * @param lhs the left hand object, an array
    * @param rhs the right hand object
    */
  private def appendArray(lhs: Any, rhs: Any): Unit = { // First we compare different dimensions, for example: a boolean[][] to a boolean[]
    // then we 'Switch' on type of array, to dispatch to the correct handler
    // This handles multi dimensional arrays of the same depth
    if (lhs.getClass ne rhs.getClass) this.setEquals(false)
    else if (lhs.isInstanceOf[Array[Long]]) append(lhs.asInstanceOf[Array[Long]], rhs.asInstanceOf[Array[Long]])
    else if (lhs.isInstanceOf[Array[Int]]) append(lhs.asInstanceOf[Array[Int]], rhs.asInstanceOf[Array[Int]])
    else if (lhs.isInstanceOf[Array[Short]]) append(lhs.asInstanceOf[Array[Short]], rhs.asInstanceOf[Array[Short]])
    else if (lhs.isInstanceOf[Array[Char]]) append(lhs.asInstanceOf[Array[Char]], rhs.asInstanceOf[Array[Char]])
    else if (lhs.isInstanceOf[Array[Byte]]) append(lhs.asInstanceOf[Array[Byte]], rhs.asInstanceOf[Array[Byte]])
    else if (lhs.isInstanceOf[Array[Double]]) append(lhs.asInstanceOf[Array[Double]], rhs.asInstanceOf[Array[Double]])
    else if (lhs.isInstanceOf[Array[Float]]) append(lhs.asInstanceOf[Array[Float]], rhs.asInstanceOf[Array[Float]])
    else if (lhs.isInstanceOf[Array[Boolean]])
      append(lhs.asInstanceOf[Array[Boolean]], rhs.asInstanceOf[Array[Boolean]])
    else { // Not an array of primitives
      append(lhs.asInstanceOf[Array[AnyRef]], rhs.asInstanceOf[Array[AnyRef]])
    }

    ()
  }

  /**
    * <p>
    * Test if two {@code long} s are equal.
    * </p>
    *
    * @param lhs
    * the left hand {@code long}
    * @param rhs
    * the right hand {@code long}
    * @return EqualsBuilder - used to chain calls.
    */
  def append(lhs: Long, rhs: Long): EqualsBuilder = {
    if (!_isEquals) return this
    _isEquals = lhs == rhs
    this
  }

  /**
    * <p>Test if two {@code int}s are equal.</p>
    *
    * @param lhs the left hand {@code int}
    * @param rhs the right hand {@code int}
    * @return EqualsBuilder - used to chain calls.
    */
  def append(lhs: Int, rhs: Int): EqualsBuilder = {
    if (!_isEquals) return this
    _isEquals = lhs == rhs
    this
  }

  /**
    * <p>Test if two {@code short}s are equal.</p>
    *
    * @param lhs the left hand {@code short}
    * @param rhs the right hand {@code short}
    * @return EqualsBuilder - used to chain calls.
    */
  def append(lhs: Short, rhs: Short): EqualsBuilder = {
    if (!_isEquals) return this
    _isEquals = lhs == rhs
    this
  }

  /**
    * <p>Test if two {@code char}s are equal.</p>
    *
    * @param lhs the left hand {@code char}
    * @param rhs the right hand {@code char}
    * @return EqualsBuilder - used to chain calls.
    */
  def append(lhs: Char, rhs: Char): EqualsBuilder = {
    if (!_isEquals) return this
    _isEquals = lhs == rhs
    this
  }

  /**
    * <p>Test if two {@code byte}s are equal.</p>
    *
    * @param lhs the left hand {@code byte}
    * @param rhs the right hand {@code byte}
    * @return EqualsBuilder - used to chain calls.
    */
  def append(lhs: Byte, rhs: Byte): EqualsBuilder = {
    if (!_isEquals) return this
    _isEquals = lhs == rhs
    this
  }

  /**
    * <p>Test if two {@code double}s are equal by testing that the
    * pattern of bits returned by {@code doubleToLong} are equal.</p>
    *
    * <p>This handles NaNs, Infinities, and {@code -0.0}.</p>
    *
    * <p>It is compatible with the hash code generated by
    * {@code HashCodeBuilder}.</p>
    *
    * @param lhs the left hand {@code double}
    * @param rhs the right hand {@code double}
    * @return EqualsBuilder - used to chain calls.
    */
  def append(lhs: Double, rhs: Double): EqualsBuilder = {
    if (!_isEquals) return this
    append(JavaDouble.doubleToLongBits(lhs), JavaDouble.doubleToLongBits(rhs))
  }

  /**
    * <p>Test if two {@code float}s are equal by testing that the
    * pattern of bits returned by doubleToLong are equal.</p>
    *
    * <p>This handles NaNs, Infinities, and {@code -0.0}.</p>
    *
    * <p>It is compatible with the hash code generated by
    * {@code HashCodeBuilder}.</p>
    *
    * @param lhs the left hand {@code float}
    * @param rhs the right hand {@code float}
    * @return EqualsBuilder - used to chain calls.
    */
  def append(lhs: Float, rhs: Float): EqualsBuilder = {
    if (!_isEquals) return this
    append(JavaFloat.floatToIntBits(lhs), JavaFloat.floatToIntBits(rhs))
  }

  /**
    * <p>Test if two {@code booleans}s are equal.</p>
    *
    * @param lhs the left hand {@code boolean}
    * @param rhs the right hand {@code boolean}
    * @return EqualsBuilder - used to chain calls.
    */
  def append(lhs: Boolean, rhs: Boolean): EqualsBuilder = {
    if (!_isEquals) return this
    _isEquals = lhs == rhs
    this
  }

  /**
    * <p>Performs a deep comparison of two {@code Object} arrays.</p>
    *
    * <p>This also will be called for the top level of
    * multi-dimensional, ragged, and multi-typed arrays.</p>
    *
    * <p>Note that this method does not compare the type of the arrays; it only
    * compares the contents.</p>
    *
    * @param lhs the left hand {@code Object[]}
    * @param rhs the right hand {@code Object[]}
    * @return EqualsBuilder - used to chain calls.
    */
  def append(lhs: Array[AnyRef], rhs: Array[AnyRef]): EqualsBuilder = {
    if (!_isEquals) return this
    if (lhs eq rhs) return this
    if (lhs == null || rhs == null) {
      this.setEquals(false)
      return this
    }
    if (lhs.length != rhs.length) {
      this.setEquals(false)
      return this
    }
    var i = 0
    while ({
      i < lhs.length && _isEquals
    }) {
      append(lhs(i), rhs(i))

      i += 1
    }
    this
  }

  /**
    * <p>Deep comparison of array of {@code long}. Length and all
    * values are compared.</p>
    *
    * <p>The method {@link #append ( long, long)} is used.</p>
    *
    * @param lhs the left hand {@code long[]}
    * @param rhs the right hand {@code long[]}
    * @return EqualsBuilder - used to chain calls.
    */
  def append(lhs: Array[Long], rhs: Array[Long]): EqualsBuilder = {
    if (!_isEquals) return this
    if (lhs eq rhs) return this
    if (lhs == null || rhs == null) {
      this.setEquals(false)
      return this
    }
    if (lhs.length != rhs.length) {
      this.setEquals(false)
      return this
    }
    var i = 0
    while ({
      i < lhs.length && _isEquals
    }) {
      append(lhs(i), rhs(i))

      i += 1
    }
    this
  }

  /**
    * <p>Deep comparison of array of {@code int}. Length and all
    * values are compared.</p>
    *
    * <p>The method {@link #append ( int, int)} is used.</p>
    *
    * @param lhs the left hand {@code int[]}
    * @param rhs the right hand {@code int[]}
    * @return EqualsBuilder - used to chain calls.
    */
  def append(lhs: Array[Int], rhs: Array[Int]): EqualsBuilder = {
    if (!_isEquals) return this
    if (lhs eq rhs) return this
    if (lhs == null || rhs == null) {
      this.setEquals(false)
      return this
    }
    if (lhs.length != rhs.length) {
      this.setEquals(false)
      return this
    }
    var i = 0
    while ({
      i < lhs.length && _isEquals
    }) {
      append(lhs(i), rhs(i))

      i += 1
    }
    this
  }

  /**
    * <p>Deep comparison of array of {@code short}. Length and all
    * values are compared.</p>
    *
    * <p>The method {@link #append ( short, short)} is used.</p>
    *
    * @param lhs the left hand {@code short[]}
    * @param rhs the right hand {@code short[]}
    * @return EqualsBuilder - used to chain calls.
    */
  def append(lhs: Array[Short], rhs: Array[Short]): EqualsBuilder = {
    if (!_isEquals) return this
    if (lhs eq rhs) return this
    if (lhs == null || rhs == null) {
      this.setEquals(false)
      return this
    }
    if (lhs.length != rhs.length) {
      this.setEquals(false)
      return this
    }
    var i = 0
    while ({
      i < lhs.length && _isEquals
    }) {
      append(lhs(i), rhs(i))

      i += 1
    }
    this
  }

  /**
    * <p>Deep comparison of array of {@code char}. Length and all
    * values are compared.</p>
    *
    * <p>The method {@link #append ( char, char)} is used.</p>
    *
    * @param lhs the left hand {@code char[]}
    * @param rhs the right hand {@code char[]}
    * @return EqualsBuilder - used to chain calls.
    */
  def append(lhs: Array[Char], rhs: Array[Char]): EqualsBuilder = {
    if (!_isEquals) return this
    if (lhs eq rhs) return this
    if (lhs == null || rhs == null) {
      this.setEquals(false)
      return this
    }
    if (lhs.length != rhs.length) {
      this.setEquals(false)
      return this
    }
    var i = 0
    while ({
      i < lhs.length && _isEquals
    }) {
      append(lhs(i), rhs(i))

      i += 1
    }
    this
  }

  /**
    * <p>Deep comparison of array of {@code byte}. Length and all
    * values are compared.</p>
    *
    * <p>The method {@link #append ( byte, byte)} is used.</p>
    *
    * @param lhs the left hand {@code byte[]}
    * @param rhs the right hand {@code byte[]}
    * @return EqualsBuilder - used to chain calls.
    */
  def append(lhs: Array[Byte], rhs: Array[Byte]): EqualsBuilder = {
    if (!_isEquals) return this
    if (lhs eq rhs) return this
    if (lhs == null || rhs == null) {
      this.setEquals(false)
      return this
    }
    if (lhs.length != rhs.length) {
      this.setEquals(false)
      return this
    }
    var i = 0
    while ({
      i < lhs.length && _isEquals
    }) {
      append(lhs(i), rhs(i))

      i += 1
    }
    this
  }

  /**
    * <p>Deep comparison of array of {@code double}. Length and all
    * values are compared.</p>
    *
    * <p>The method {@link #append ( double, double)} is used.</p>
    *
    * @param lhs the left hand {@code double[]}
    * @param rhs the right hand {@code double[]}
    * @return EqualsBuilder - used to chain calls.
    */
  def append(lhs: Array[Double], rhs: Array[Double]): EqualsBuilder = {
    if (!_isEquals) return this
    if (lhs eq rhs) return this
    if (lhs == null || rhs == null) {
      this.setEquals(false)
      return this
    }
    if (lhs.length != rhs.length) {
      this.setEquals(false)
      return this
    }
    var i = 0
    while ({
      i < lhs.length && _isEquals
    }) {
      append(lhs(i), rhs(i))

      i += 1
    }
    this
  }

  /**
    * <p>Deep comparison of array of {@code float}. Length and all
    * values are compared.</p>
    *
    * <p>The method {@link #append ( float, float)} is used.</p>
    *
    * @param lhs the left hand {@code float[]}
    * @param rhs the right hand {@code float[]}
    * @return EqualsBuilder - used to chain calls.
    */
  def append(lhs: Array[Float], rhs: Array[Float]): EqualsBuilder = {
    if (!_isEquals) return this
    if (lhs eq rhs) return this
    if (lhs == null || rhs == null) {
      this.setEquals(false)
      return this
    }
    if (lhs.length != rhs.length) {
      this.setEquals(false)
      return this
    }
    var i = 0
    while ({
      i < lhs.length && _isEquals
    }) {
      append(lhs(i), rhs(i))

      i += 1
    }
    this
  }

  /**
    * <p>Deep comparison of array of {@code boolean}. Length and all
    * values are compared.</p>
    *
    * <p>The method {@link #append ( boolean, boolean)} is used.</p>
    *
    * @param lhs the left hand {@code boolean[]}
    * @param rhs the right hand {@code boolean[]}
    * @return EqualsBuilder - used to chain calls.
    */
  def append(lhs: Array[Boolean], rhs: Array[Boolean]): EqualsBuilder = {
    if (!_isEquals) return this
    if (lhs eq rhs) return this
    if (lhs == null || rhs == null) {
      this.setEquals(false)
      return this
    }
    if (lhs.length != rhs.length) {
      this.setEquals(false)
      return this
    }
    var i = 0
    while ({
      i < lhs.length && _isEquals
    }) {
      append(lhs(i), rhs(i))

      i += 1
    }
    this
  }

  /**
    * <p>Returns {@code true} if the fields that have been checked
    * are all equal.</p>
    *
    * @return boolean
    */
  def isEquals(): Boolean = this._isEquals

  /**
    * <p>Returns {@code true} if the fields that have been checked
    * are all equal.</p>
    *
    * @return {@code true} if all of the fields that have been checked
    *         are equal, {@code false} otherwise.
    * @since 3.0
    */
  override def build: Boolean = JavaBoolean.valueOf(_isEquals)

  /**
    * Sets the {@code _isEquals} value.
    *
    * @param _isEquals The value to set.
    * @since 2.1
    */
  protected def setEquals(isEquals: Boolean): Unit = {
    this._isEquals = isEquals
  }

  /**
    * Reset the EqualsBuilder so you can use the same object again
    *
    * @since 2.5
    */
  def reset(): Unit = {
    this._isEquals = true
  }
}
