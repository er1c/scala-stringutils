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

import java.lang.reflect.Method
import java.lang.reflect.Modifier
import java.util
import java.util.Collections
import org.apache.commons.lang3.mutable.MutableObject
import java.lang.{Boolean => JavaBoolean, Byte => JavaByte, Double => JavaDouble, Float => JavaFloat, Long => JavaLong, Short => JavaShort}
import scala.collection.JavaConverters._

/**
  * <p>Operates on classes without using reflection.</p>
  *
  * <p>This class handles invalid {@code null} inputs as best it can.
  * Each method documents its behavior in more detail.</p>
  *
  * <p>The notion of a {@code canonical name} includes the human
  * readable name for the type, for example {@code int[]}. The
  * non-canonical method variants work with the JVM names, such as
  * {@code [I}. </p>
  *
  * @since 2.0
  */
object ClassUtils {

  /**
    * Inclusivity literals for {@link #hierarchy ( Class, Interfaces)}.
    *
    * @since 3.2
    */
  object Interfaces extends Enumeration {
    type Interfaces = Value
    val
      /** Includes interfaces. */
      INCLUDE,

      /** Excludes interfaces. */
      EXCLUDE = Value
  }

  /**
    * The package separator character: {@code '&#x2e;' == {@value}}.
    */
  val PACKAGE_SEPARATOR_CHAR: Char = '.'
  /**
    * The package separator String: {@code "&#x2e;"}.
    */
  val PACKAGE_SEPARATOR: String = String.valueOf(PACKAGE_SEPARATOR_CHAR)
  /**
    * The inner class separator character: {@code '$' == {@value}}.
    */
  val INNER_CLASS_SEPARATOR_CHAR: Char = '$'
  /**
    * The inner class separator String: {@code "$"}.
    */
  val INNER_CLASS_SEPARATOR: String = String.valueOf(INNER_CLASS_SEPARATOR_CHAR)
  /**
    * Maps names of primitives to their corresponding primitive {@code Class}es.
    */
  private val namePrimitiveMap: Map[String, Class[_]] = Map(
    "boolean" -> JavaBoolean.TYPE,
    "byte"    -> JavaByte.TYPE,
    "char"    -> Character.TYPE,
    "short"   -> JavaShort.TYPE,
    "int"     -> Integer.TYPE,
    "long"    -> JavaLong.TYPE,
    "double"  -> JavaDouble.TYPE,
    "float"   -> JavaFloat.TYPE,
    "void"    -> Void.TYPE
  )

  /**
    * Maps primitive {@code Class}es to their corresponding wrapper {@code Class}.
    */
  private val primitiveWrapperMap: Map[Class[_], Class[_]] = Map(
    JavaBoolean.TYPE -> classOf[Boolean],
    JavaByte.TYPE    -> classOf[Byte],
    Character.TYPE   -> classOf[Character],
    JavaShort.TYPE   -> classOf[Short],
    Integer.TYPE     -> classOf[Integer],
    JavaLong.TYPE    -> classOf[Long],
    JavaDouble.TYPE  -> classOf[Double],
    JavaFloat.TYPE   -> classOf[Float],
    Void.TYPE        -> Void.TYPE
  )

  /**
    * Maps wrapper {@code Class}es to their corresponding primitive types.
    */
  private val wrapperPrimitiveMap: Map[Class[_], Class[_]] = {
    //???
    Map.empty
  }
/*
    private static final Map<Class<?>, Class<?>> wrapperPrimitiveMap = new HashMap<>();
    static {
        for (final Map.Entry<Class<?>, Class<?>> entry : primitiveWrapperMap.entrySet()) {
            final Class<?> primitiveClass = entry.getKey();
            final Class<?> wrapperClass = entry.getValue();
            if (!primitiveClass.equals(wrapperClass)) {
                wrapperPrimitiveMap.put(wrapperClass, primitiveClass);
            }
        }
    }
 */

  /**
    * Maps a primitive class name to its corresponding abbreviation used in array class names.
    */
  private val abbreviationMap: Map[String, String] = Map(
    "int"     -> "I",
    "boolean" -> "Z",
    "float"   -> "F",
    "long"    -> "J",
    "short"   -> "S",
    "byte"    -> "B",
    "double"  -> "D",
    "char"    -> "C"
  )

  /**
    * Maps an abbreviation used in array class names to corresponding primitive class name.
    */
  private val reverseAbbreviationMap: Map[String, String] = abbreviationMap.map{ v => v._2 -> v._1 }

  /**
    * <p>Gets the class name of the {@code object} without the package name or names.</p>
    *
    * <p>The method looks up the class of the object and then converts the name of the class invoking
    * {@link #getShortClassName ( Class )} (see relevant notes there).</p>
    *
    * @param object      the class to get the short name for, may be {@code null}
    * @param valueIfNull the value to return if the object is {@code null}
    * @return the class name of the object without the package name, or {@code valueIfNull}
    *         if the argument {@code object} is {@code null}
    */
  def getShortClassName(`object`: Any, valueIfNull: String): String = {
    if (`object` == null) return valueIfNull
    getShortClassName(`object`.getClass)
  }

  /**
    * <p>Gets the class name minus the package name from a {@code Class}.</p>
    *
    * <p>This method simply gets the name using {@code Class.getName()} and then calls
    * {@link #getShortClassName ( Class )}. See relevant notes there.</p>
    *
    * @param cls the class to get the short name for.
    * @return the class name without the package name or an empty string. If the class
    *         is an inner class then the returned value will contain the outer class
    *         or classes separated with {@code .} (dot) character.
    */
  def getShortClassName(cls: Class[_]): String = {
    if (cls == null) return StringUtils.EMPTY
    getShortClassName(cls.getName)
  }

  /**
    * <p>Gets the class name minus the package name from a String.</p>
    *
    * <p>The string passed in is assumed to be a class name - it is not checked. The string has to be formatted the way
    * as the JDK method {@code Class.getName()} returns it, and not the usual way as we write it, for example in import
    * statements, or as it is formatted by {@code Class.getCanonicalName()}.</p>
    *
    * <p>The difference is is significant only in case of classes that are inner classes of some other
    * classes. In this case the separator between the outer and inner class (possibly on multiple hierarchy level) has
    * to be {@code $} (dollar sign) and not {@code .} (dot), as it is returned by {@code Class.getName()}</p>
    *
    * <p>Note that this method is called from the {@link #getShortClassName ( Class )} method using the string
    * returned by {@code Class.getName()}.</p>
    *
    * <p>Note that this method differs from {@link #getSimpleName ( Class )} in that this will
    * return, for example {@code "Map.Entry"} whilst the {@code java.lang.Class} variant will simply
    * return {@code "Entry"}. In this example the argument {@code className} is the string
    * {@code java.util.Map$Entry} (note the {@code $} sign.</p>
    *
    * @param className the className to get the short name for. It has to be formatted as returned by
    *                  {@code Class.getName()} and not {@code Class.getCanonicalName()}
    * @return the class name of the class without the package name or an empty string. If the class is
    *         an inner class then value contains the outer class or classes and the separator is replaced
    *         to be {@code .} (dot) character.
    */
  def getShortClassName(className: String): String = {
    if (StringUtils.isEmpty(className)) return StringUtils.EMPTY

    var _className: String = className
    val arrayPrefix = new StringBuilder

    // Handle array encoding
    if (_className.startsWith("[")) {
      while (_className.charAt(0) == '[') {
        _className = _className.substring(1)
        arrayPrefix.append("[]")
      }

      // Strip Object type encoding
      if (_className.charAt(0) == 'L' && _className.charAt(_className.length - 1) == ';')
        _className = _className.substring(1, _className.length - 1)

      if (reverseAbbreviationMap.contains(_className))
        _className = reverseAbbreviationMap(_className)
    }
    val lastDotIdx = _className.lastIndexOf(PACKAGE_SEPARATOR_CHAR.toInt)
    val innerIdx = _className.indexOf(INNER_CLASS_SEPARATOR_CHAR.toInt,
      if (lastDotIdx == -1) 0
      else lastDotIdx + 1
    )
    var out = _className.substring(lastDotIdx + 1)
    if (innerIdx != -1) out = out.replace(INNER_CLASS_SEPARATOR_CHAR, PACKAGE_SEPARATOR_CHAR)
    out + arrayPrefix
  }

  /**
    * <p>Null-safe version of {@code cls.getSimpleName()}</p>
    *
    * @param cls the class for which to get the simple name; may be null
    * @return the simple class name or the empty string in case the argument is {@code null}
    * @since 3.0
    * @see Class#getSimpleName()
    */
  def getSimpleName(cls: Class[_]): String = getSimpleName(cls, StringUtils.EMPTY)

  /**
    * <p>Null-safe version of {@code cls.getSimpleName()}</p>
    *
    * @param cls         the class for which to get the simple name; may be null
    * @param valueIfNull the value to return if null
    * @return the simple class name or {@code valueIfNull} if the
    *         argument {@code cls} is {@code null}
    * @since 3.0
    * @see Class#getSimpleName()
    */
  def getSimpleName(cls: Class[_], valueIfNull: String): String = if (cls == null) valueIfNull
  else cls.getSimpleName

  /**
    * <p>Null-safe version of {@code object.getClass().getSimpleName()}</p>
    *
    * <p>It is to note that this method is overloaded and in case the argument {@code object} is a
    * {@code Class} object then the {@link #getSimpleName ( Class )} will be invoked. If this is
    * a significant possibility then the caller should check this case and call {@code
    * getSimpleName(Class.class)} or just simply use the string literal {@code "Class"}, which
    * is the result of the method in that case.</p>
    *
    * @param object the object for which to get the simple class name; may be null
    * @return the simple class name or the empty string in case the argument is {@code null}
    * @since 3.7
    * @see Class#getSimpleName()
    */
  def getSimpleName(`object`: Any): String = getSimpleName(`object`, StringUtils.EMPTY)

  /**
    * <p>Null-safe version of {@code object.getClass().getSimpleName()}</p>
    *
    * @param object      the object for which to get the simple class name; may be null
    * @param valueIfNull the value to return if {@code object} is {@code null}
    * @return the simple class name or {@code valueIfNull} if the
    *         argument {@code object} is {@code null}
    * @since 3.0
    * @see Class#getSimpleName()
    */
  def getSimpleName(`object`: Any, valueIfNull: String): String = if (`object` == null) valueIfNull
  else `object`.getClass.getSimpleName

  /**
    * <p>Null-safe version of {@code cls.getName()}</p>
    *
    * @param cls the class for which to get the class name; may be null
    * @return the class name or the empty string in case the argument is {@code null}
    * @since 3.7
    * @see Class#getSimpleName()
    */
  def getName(cls: Class[_]): String = getName(cls, StringUtils.EMPTY)

  /**
    * <p>Null-safe version of {@code cls.getName()}</p>
    *
    * @param cls         the class for which to get the class name; may be null
    * @param valueIfNull the return value if the argument {@code cls} is {@code null}
    * @return the class name or {@code valueIfNull}
    * @since 3.7
    * @see Class#getName()
    */
  def getName(cls: Class[_], valueIfNull: String): String = if (cls == null) valueIfNull
  else cls.getName

  /**
    * <p>Null-safe version of {@code object.getClass().getName()}</p>
    *
    * @param object the object for which to get the class name; may be null
    * @return the class name or the empty String
    * @since 3.7
    * @see Class#getSimpleName()
    */
  def getName(`object`: Any): String = getName(`object`, StringUtils.EMPTY)

  /**
    * <p>Null-safe version of {@code object.getClass().getSimpleName()}</p>
    *
    * @param object      the object for which to get the class name; may be null
    * @param valueIfNull the value to return if {@code object} is {@code null}
    * @return the class name or {@code valueIfNull}
    * @since 3.0
    * @see Class#getName()
    */
  def getName(`object`: Any, valueIfNull: String): String = if (`object` == null) valueIfNull
  else `object`.getClass.getName

  /**
    * <p>Gets the package name of an {@code Object}.</p>
    *
    * @param object      the class to get the package name for, may be null
    * @param valueIfNull the value to return if null
    * @return the package name of the object, or the null value
    */
  def getPackageName(`object`: Any, valueIfNull: String): String = {
    if (`object` == null) return valueIfNull
    getPackageName(`object`.getClass)
  }

  /**
    * <p>Gets the package name of a {@code Class}.</p>
    *
    * @param cls the class to get the package name for, may be {@code null}.
    * @return the package name or an empty string
    */
  def getPackageName(cls: Class[_]): String = {
    if (cls == null) return StringUtils.EMPTY
    getPackageName(cls.getName)
  }

  /**
    * <p>Gets the package name from a {@code String}.</p>
    *
    * <p>The string passed in is assumed to be a class name - it is not checked.</p>
    * <p>If the class is unpackaged, return an empty string.</p>
    *
    * @param className the className to get the package name for, may be {@code null}
    * @return the package name or an empty string
    */
  def getPackageName(className: String): String = {
    if (StringUtils.isEmpty(className)) return StringUtils.EMPTY

    var _className: String = className
    // Strip array encoding
    while (_className.charAt(0) == '[') _className = _className.substring(1)
    if (_className.charAt(0) == 'L' && _className.charAt(_className.length - 1) == ';')
      _className = _className.substring(1)
    val i = _className.lastIndexOf(PACKAGE_SEPARATOR_CHAR.toInt)
    if (i == -1) return StringUtils.EMPTY
    _className.substring(0, i)
  }

  /**
    * <p>Gets the abbreviated name of a {@code Class}.</p>
    *
    * @param cls        the class to get the abbreviated name for, may be {@code null}
    * @param lengthHint the desired length of the abbreviated name
    * @return the abbreviated name or an empty string
    * @throws IllegalArgumentException if len &lt;= 0
    * @see #getAbbreviatedName(String, int)
    * @since 3.4
    */
  def getAbbreviatedName(cls: Class[_], lengthHint: Int): String = {
    if (cls == null) return StringUtils.EMPTY
    getAbbreviatedName(cls.getName, lengthHint)
  }

  /**
    * <p>Gets the abbreviated class name from a {@code String}.</p>
    *
    * <p>The string passed in is assumed to be a class name - it is not checked.</p>
    *
    * <p>The abbreviation algorithm will shorten the class name, usually without
    * significant loss of meaning.</p>
    *
    * <p>The abbreviated class name will always include the complete package hierarchy.
    * If enough space is available, rightmost sub-packages will be displayed in full
    * length. The abbreviated package names will be shortened to a single character.</p>
    * <p>Only package names are shortened, the class simple name remains untouched. (See examples.)</p>
    * <p>The result will be longer than the desired length only if all the package names
    * shortened to a single character plus the class simple name with the separating dots
    * together are longer than the desired length. In other words, when the class name
    * cannot be shortened to the desired length.</p>
    * <p>If the class name can be shortened then
    * the final length will be at most {@code lengthHint} characters.</p>
    * <p>If the {@code lengthHint} is zero or negative then the method
    * throws exception. If you want to achieve the shortest possible version then
    * use {@code 1} as a {@code lengthHint}.</p>
    *
    * <table>
    * <caption>Examples</caption>
    * <tr><td>className</td><td>len</td><td>return</td></tr>
    * <tr><td>              null</td><td> 1</td><td>""</td></tr>
    * <tr><td>"java.lang.String"</td><td> 5</td><td>"j.l.String"</td></tr>
    * <tr><td>"java.lang.String"</td><td>15</td><td>"j.lang.String"</td></tr>
    * <tr><td>"java.lang.String"</td><td>30</td><td>"java.lang.String"</td></tr>
    * <tr><td>"org.apache.commons.lang3.ClassUtils"</td><td>18</td><td>"o.a.c.l.ClassUtils"</td></tr>
    * </table>
    *
    * @param className  the className to get the abbreviated name for, may be {@code null}
    * @param lengthHint the desired length of the abbreviated name
    * @return the abbreviated name or an empty string if the specified
    *         class name is {@code null} or empty string. The abbreviated name may be
    *         longer than the desired length if it cannot be abbreviated to the desired length.
    * @throws IllegalArgumentException if {@code len <= 0}
    * @since 3.4
    */
  def getAbbreviatedName(className: String, lengthHint: Int): String = {
    if (lengthHint <= 0) throw new IllegalArgumentException("len must be > 0")
    if (className == null) return StringUtils.EMPTY
    if (className.length <= lengthHint) return className
    val abbreviated = className.toCharArray
    var target = 0
    var source = 0

    while (source < abbreviated.length) { // copy the next part
      var runAheadTarget = target
      while (source < abbreviated.length && abbreviated(source) != '.') {
        abbreviated({runAheadTarget += 1; runAheadTarget - 1}) =
          abbreviated({source += 1; source - 1})
      }

      target += 1
      if (useFull(runAheadTarget, source, abbreviated.length, lengthHint) || target > runAheadTarget) target = runAheadTarget
      // copy the '.' unless it was the last part
      if (source < abbreviated.length)
        abbreviated({target += 1; target - 1}) =
          abbreviated({source += 1; source - 1})
    }

    new String(abbreviated, 0, target)
  }

  /**
    * <p>Decides if the part that was just copied to its destination
    * location in the work array can be kept as it was copied or must be
    * abbreviated. It must be kept when the part is the last one, which
    * is the simple name of the class. In this case the {@code source}
    * index, from where the characters are copied points one position
    * after the last character, a.k.a. {@code source ==
     * originalLength}</p>
    *
    * <p>If the part is not the last one then it can be kept
    * unabridged if the number of the characters copied so far plus
    * the character that are to be copied is less than or equal to the
    * desired length.</p>
    *
    * @param runAheadTarget the target index (where the characters were
    *                       copied to) pointing after the last character
    *                       copied when the current part was copied
    * @param source         the source index (where the characters were
    *                       copied from) pointing after the last
    *                       character copied when the current part was
    *                       copied
    * @param originalLength the original length of the class full name,
    *                       which is abbreviated
    * @param desiredLength  the desired length of the abbreviated class
    *                       name
    * @return {@code true} if it can be kept in its original length
    *         {@code false} if the current part has to be abbreviated and
    */
  private def useFull(runAheadTarget: Int, source: Int, originalLength: Int, desiredLength: Int): Boolean =
    source >= originalLength || runAheadTarget + originalLength - source <= desiredLength

  /**
    * <p>Gets a {@code List} of superclasses for the given class.</p>
    *
    * @param cls the class to look up, may be {@code null}
    * @return the {@code List} of superclasses in order going up from this one
    *         {@code null} if null input
    */
  def getAllSuperclasses(cls: Class[_]): util.List[Class[_]] = {
    if (cls == null) return null
    val classes = new util.ArrayList[Class[_]]()

    var superclass = cls.getSuperclass
    while (superclass != null) {
      classes.add(superclass)
      superclass = superclass.getSuperclass
    }
    classes
  }

  /**
    * <p>Gets a {@code List} of all interfaces implemented by the given
    * class and its superclasses.</p>
    *
    * <p>The order is determined by looking through each interface in turn as
    * declared in the source file and following its hierarchy up. Then each
    * superclass is considered in the same way. Later duplicates are ignored,
    * so the order is maintained.</p>
    *
    * @param cls the class to look up, may be {@code null}
    * @return the {@code List} of interfaces in order,
    *         {@code null} if null input
    */
  def getAllInterfaces(cls: Class[_]): util.List[Class[_]] = {
    if (cls == null) return null
    val interfacesFound = new util.LinkedHashSet[Class[_]]()

    getAllInterfaces(cls, interfacesFound)

    new util.ArrayList(interfacesFound)
  }

  /**
    * Gets the interfaces for the specified class.
    *
    * @param cls             the class to look up, may be {@code null}
    * @param interfacesFound the {@code Set} of interfaces for the class
    */
  private def getAllInterfaces(cls: Class[_], interfacesFound: util.HashSet[Class[_]]): Unit = {
    var c = cls
    while (c != null) {
      val interfaces = c.getInterfaces
      for (i <- interfaces) {
        if (interfacesFound.add(i)) getAllInterfaces(i, interfacesFound)
      }
      c = c.getSuperclass
    }
  }

  /**
    * <p>Given a {@code List} of class names, this method converts them into classes.</p>
    *
    * <p>A new {@code List} is returned. If the class name cannot be found, {@code null}
    * is stored in the {@code List}. If the class name in the {@code List} is
    * {@code null}, {@code null} is stored in the output {@code List}.</p>
    *
    * @param classNames the classNames to change
    * @return a {@code List} of Class objects corresponding to the class names,
    *         {@code null} if null input
    * @throws ClassCastException if classNames contains a non String entry
    */
  def convertClassNamesToClasses(classNames: List[String]): List[Class[_]] = {
    if (classNames == null || classNames.isEmpty) return Nil
    val classes = List.newBuilder[Class[_]]

    for (className <- classNames) {
      try {
        classes += (Class.forName(className))
      } catch {
        case _: Exception => classes += null
      }
    }

    classes.result()
  }

  /**
    * <p>Given a {@code List} of {@code Class} objects, this method converts
    * them into class names.</p>
    *
    * <p>A new {@code List} is returned. {@code null} objects will be copied into
    * the returned list as {@code null}.</p>
    *
    * @param classes the classes to change
    * @return a {@code List} of class names corresponding to the Class objects,
    *         {@code null} if null input
    * @throws ClassCastException if {@code classes} contains a non-{@code Class} entry
    */
  def convertClassesToClassNames(classes: util.List[Class[_]]): util.List[String] = {
    if (classes == null) return null
    val classNames = new util.ArrayList[String](classes.size)

    for (cls <- classes.iterator.asScala) {
      if (cls == null) classNames.add(null)
      else classNames.add(cls.getName)
    }

    classNames
  }

  /**
    * <p>Checks if an array of Classes can be assigned to another array of Classes.</p>
    *
    * <p>This method calls {@link #isAssignable ( Class, Class) isAssignable} for each
    * Class pair in the input arrays. It can be used to check if a set of arguments
    * (the first parameter) are suitably compatible with a set of method parameter types
    * (the second parameter).</p>
    *
    * <p>Unlike the {@link Class# isAssignableFrom ( java.lang.Class )} method, this
    * method takes into account widenings of primitive classes and
    * {@code null}s.</p>
    *
    * <p>Primitive widenings allow an int to be assigned to a {@code long},
    * {@code float} or {@code double}. This method returns the correct
    * result for these cases.</p>
    *
    * <p>{@code Null} may be assigned to any reference type. This method will
    * return {@code true} if {@code null} is passed in and the toClass is
    * non-primitive.</p>
    *
    * <p>Specifically, this method tests whether the type represented by the
    * specified {@code Class} parameter can be converted to the type
    * represented by this {@code Class} object via an identity conversion
    * widening primitive or widening reference conversion. See
    * <em><a href="http://docs.oracle.com/javase/specs/">The Java Language Specification</a></em>,
    * sections 5.1.1, 5.1.2 and 5.1.4 for details.</p>
    *
    * <p><strong>Since Lang 3.0,</strong> this method will default behavior for
    * calculating assignability between primitive and wrapper types <em>corresponding
    * to the running Java version</em>; i.e. autoboxing will be the default
    * behavior in VMs running Java versions &gt; 1.5.</p>
    *
    * @param classArray   the array of Classes to check, may be {@code null}
    * @param toClassArray the array of Classes to try to assign into, may be {@code null}
    * @return {@code true} if assignment possible
    */
  def isAssignable(classArray: Array[Class[_]], toClassArray: Class[_]*): Boolean =
    isAssignable(classArray, toClassArray.toArray, true)

  /**
    * <p>Checks if an array of Classes can be assigned to another array of Classes.</p>
    *
    * <p>This method calls {@link #isAssignable ( Class, Class) isAssignable} for each
    * Class pair in the input arrays. It can be used to check if a set of arguments
    * (the first parameter) are suitably compatible with a set of method parameter types
    * (the second parameter).</p>
    *
    * <p>Unlike the {@link Class# isAssignableFrom ( java.lang.Class )} method, this
    * method takes into account widenings of primitive classes and
    * {@code null}s.</p>
    *
    * <p>Primitive widenings allow an int to be assigned to a {@code long},
    * {@code float} or {@code double}. This method returns the correct
    * result for these cases.</p>
    *
    * <p>{@code Null} may be assigned to any reference type. This method will
    * return {@code true} if {@code null} is passed in and the toClass is
    * non-primitive.</p>
    *
    * <p>Specifically, this method tests whether the type represented by the
    * specified {@code Class} parameter can be converted to the type
    * represented by this {@code Class} object via an identity conversion
    * widening primitive or widening reference conversion. See
    * <em><a href="http://docs.oracle.com/javase/specs/">The Java Language Specification</a></em>,
    * sections 5.1.1, 5.1.2 and 5.1.4 for details.</p>
    *
    * @param classArray   the array of Classes to check, may be {@code null}
    * @param toClassArray the array of Classes to try to assign into, may be {@code null}
    * @param autoboxing   whether to use implicit autoboxing/unboxing between primitives and wrappers
    * @return {@code true} if assignment possible
    */
  def isAssignable(classArray: Array[Class[_]], toClassArray: Array[Class[_]], autoboxing: Boolean): Boolean = {
    if (!ArrayUtils.isSameLength(classArray, toClassArray)) return false

    val _classArray =
      if (classArray == null) ArrayUtils.EMPTY_CLASS_ARRAY
      else classArray

    val _toClassArray =
      if (toClassArray == null) ArrayUtils.EMPTY_CLASS_ARRAY
      else toClassArray

    for (i: Int <- 0 until _classArray.length) {
      if (!isAssignable(_classArray(i), _toClassArray(i), autoboxing)) return false
    }

    true
  }

  /**
    * Returns whether the given {@code type} is a primitive or primitive wrapper ({@link Boolean}, {@link Byte}, {@link Character},
    * {@link Short}, {@link Integer}, {@link Long}, {@link Double}, {@link Float}).
    *
    * @param type
    * The class to query or null.
    * @return true if the given {@code type} is a primitive or primitive wrapper ({@link Boolean}, {@link Byte}, {@link Character},
    *         {@link Short}, {@link Integer}, {@link Long}, {@link Double}, {@link Float}).
    * @since 3.1
    */
  def isPrimitiveOrWrapper(`type`: Class[_]): Boolean = {
    if (`type` == null) return false
    `type`.isPrimitive || isPrimitiveWrapper(`type`)
  }

  /**
    * Returns whether the given {@code type} is a primitive wrapper ({@link Boolean}, {@link Byte}, {@link Character}, {@link Short},
    * {@link Integer}, {@link Long}, {@link Double}, {@link Float}).
    *
    * @param type
    * The class to query or null.
    * @return true if the given {@code type} is a primitive wrapper ({@link Boolean}, {@link Byte}, {@link Character}, {@link Short},
    *         {@link Integer}, {@link Long}, {@link Double}, {@link Float}).
    * @since 3.1
    */
  def isPrimitiveWrapper(`type`: Class[_]): Boolean = wrapperPrimitiveMap.contains(`type`)

  /**
    * <p>Checks if one {@code Class} can be assigned to a variable of
    * another {@code Class}.</p>
    *
    * <p>Unlike the {@link Class# isAssignableFrom ( java.lang.Class )} method,
    * this method takes into account widenings of primitive classes and
    * {@code null}s.</p>
    *
    * <p>Primitive widenings allow an int to be assigned to a long, float or
    * double. This method returns the correct result for these cases.</p>
    *
    * <p>{@code Null} may be assigned to any reference type. This method
    * will return {@code true} if {@code null} is passed in and the
    * toClass is non-primitive.</p>
    *
    * <p>Specifically, this method tests whether the type represented by the
    * specified {@code Class} parameter can be converted to the type
    * represented by this {@code Class} object via an identity conversion
    * widening primitive or widening reference conversion. See
    * <em><a href="http://docs.oracle.com/javase/specs/">The Java Language Specification</a></em>,
    * sections 5.1.1, 5.1.2 and 5.1.4 for details.</p>
    *
    * <p><strong>Since Lang 3.0,</strong> this method will default behavior for
    * calculating assignability between primitive and wrapper types <em>corresponding
    * to the running Java version</em>; i.e. autoboxing will be the default
    * behavior in VMs running Java versions &gt; 1.5.</p>
    *
    * @param cls     the Class to check, may be null
    * @param toClass the Class to try to assign into, returns false if null
    * @return {@code true} if assignment possible
    */
  def isAssignable(cls: Class[_], toClass: Class[_]): Boolean = isAssignable(cls, toClass, true)

  /**
    * <p>Checks if one {@code Class} can be assigned to a variable of
    * another {@code Class}.</p>
    *
    * <p>Unlike the {@link Class# isAssignableFrom ( java.lang.Class )} method,
    * this method takes into account widenings of primitive classes and
    * {@code null}s.</p>
    *
    * <p>Primitive widenings allow an int to be assigned to a long, float or
    * double. This method returns the correct result for these cases.</p>
    *
    * <p>{@code Null} may be assigned to any reference type. This method
    * will return {@code true} if {@code null} is passed in and the
    * toClass is non-primitive.</p>
    *
    * <p>Specifically, this method tests whether the type represented by the
    * specified {@code Class} parameter can be converted to the type
    * represented by this {@code Class} object via an identity conversion
    * widening primitive or widening reference conversion. See
    * <em><a href="http://docs.oracle.com/javase/specs/">The Java Language Specification</a></em>,
    * sections 5.1.1, 5.1.2 and 5.1.4 for details.</p>
    *
    * @param cls        the Class to check, may be null
    * @param toClass    the Class to try to assign into, returns false if null
    * @param autoboxing whether to use implicit autoboxing/unboxing between primitives and wrappers
    * @return {@code true} if assignment possible
    */
  def isAssignable(cls: Class[_], toClass: Class[_], autoboxing: Boolean): Boolean = {
    if (toClass == null) return false

    // have to check for null, as isAssignableFrom doesn't
    if (cls == null) return !toClass.isPrimitive

    var _cls = cls
    //autoboxing:
    if (autoboxing) {
      if (_cls.isPrimitive && !toClass.isPrimitive) {
        _cls = primitiveToWrapper(_cls)
        if (_cls == null) return false
      }
      if (toClass.isPrimitive && !_cls.isPrimitive) {
        _cls = wrapperToPrimitive(_cls)
        if (_cls == null) return false
      }
    }

    if (_cls == toClass) return true

    if (_cls.isPrimitive) {
      if (!toClass.isPrimitive) return false
      if (Integer.TYPE == _cls) return JavaLong.TYPE == toClass || JavaFloat.TYPE == toClass || JavaDouble.TYPE == toClass
      if (JavaLong.TYPE == _cls) return JavaFloat.TYPE == toClass || JavaDouble.TYPE == toClass
      if (JavaBoolean.TYPE == _cls) return false
      if (JavaDouble.TYPE == _cls) return false
      if (JavaFloat.TYPE == _cls) return JavaDouble.TYPE == toClass
      if (Character.TYPE == _cls) return Integer.TYPE == toClass || JavaLong.TYPE == toClass || JavaFloat.TYPE == toClass || JavaDouble.TYPE == toClass
      if (JavaShort.TYPE == _cls) return Integer.TYPE == toClass || JavaLong.TYPE == toClass || JavaFloat.TYPE == toClass || JavaDouble.TYPE == toClass
      if (JavaByte.TYPE == _cls) return JavaShort.TYPE == toClass || Integer.TYPE == toClass || JavaLong.TYPE == toClass || JavaFloat.TYPE == toClass || JavaDouble.TYPE == toClass
      // should never get here
      return false
    }

    toClass.isAssignableFrom(_cls)
  }

  /**
    * <p>Converts the specified primitive Class object to its corresponding
    * wrapper Class object.</p>
    *
    * <p>NOTE: From v2.2, this method handles {@code Void.TYPE},
    * returning {@code Void.TYPE}.</p>
    *
    * @param cls the class to convert, may be null
    * @return the wrapper class for {@code cls} or {@code cls} if
    *         {@code cls} is not a primitive. {@code null} if null input.
    * @since 2.1
    */
  def primitiveToWrapper(cls: Class[_]): Class[_] = {
    var convertedClass = cls
    if (cls != null && cls.isPrimitive) convertedClass = primitiveWrapperMap(cls)
    convertedClass
  }

  /**
    * <p>Converts the specified array of primitive Class objects to an array of
    * its corresponding wrapper Class objects.</p>
    *
    * @param classes the class array to convert, may be null or empty
    * @return an array which contains for each given class, the wrapper class or
    *         the original class if class is not a primitive. {@code null} if null input.
    *         Empty array if an empty array passed in.
    * @since 2.1
    */
  def primitivesToWrappers(classes: Class[_]*): Array[Class[_]] = {
    if (classes == null) return null
    if (classes.length == 0) return classes.toArray

    val convertedClasses = new Array[Class[_]](classes.length)
    for (i <- 0 until classes.length) {
      convertedClasses(i) = primitiveToWrapper(classes(i))
    }
    convertedClasses
  }

  /**
    * <p>Converts the specified wrapper class to its corresponding primitive
    * class.</p>
    *
    * <p>This method is the counter part of {@code primitiveToWrapper()}.
    * If the passed in class is a wrapper class for a primitive type, this
    * primitive type will be returned (e.g. {@code Integer.TYPE} for
    * {@code Integer.class}). For other classes, or if the parameter is
    * <b>null</b>, the return value is <b>null</b>.</p>
    *
    * @param cls the class to convert, may be <b>null</b>
    * @return the corresponding primitive type if {@code cls} is a
    *         wrapper class, <b>null</b> otherwise
    * @see #primitiveToWrapper(Class)
    * @since 2.4
    */
  def wrapperToPrimitive(cls: Class[_]): Class[_] = wrapperPrimitiveMap(cls)

  /**
    * <p>Converts the specified array of wrapper Class objects to an array of
    * its corresponding primitive Class objects.</p>
    *
    * <p>This method invokes {@code wrapperToPrimitive()} for each element
    * of the passed in array.</p>
    *
    * @param classes the class array to convert, may be null or empty
    * @return an array which contains for each given class, the primitive class or
    *         <b>null</b> if the original class is not a wrapper class. {@code null} if null input.
    *         Empty array if an empty array passed in.
    * @see #wrapperToPrimitive(Class)
    * @since 2.4
    */
  def wrappersToPrimitives(classes: Class[_]*): Array[Class[_]] = {
    if (classes == null) return null
    if (classes.length == 0) return classes.toArray

    val convertedClasses = new Array[Class[_]](classes.length)
    for (i <- 0 until classes.length) {
      convertedClasses(i) = wrapperToPrimitive(classes(i))
    }
    convertedClasses
  }

  /**
    * <p>Is the specified class an inner class or static nested class.</p>
    *
    * @param cls the class to check, may be null
    * @return {@code true} if the class is an inner or static nested class,
    *         false if not or {@code null}
    */
  def isInnerClass(cls: Class[_]): Boolean = cls != null && cls.getEnclosingClass != null

  /**
    * Returns the class represented by {@code className} using the
    * {@code classLoader}.  This implementation supports the syntaxes
    * "{@code java.util.Map.Entry[]}", "{@code java.util.Map$Entry[]}",
    * "{@code [Ljava.util.Map.Entry;}", and "{@code [Ljava.util.Map$Entry;}".
    *
    * @param classLoader the class loader to use to load the class
    * @param className   the class name
    * @param initialize  whether the class must be initialized
    * @return the class represented by {@code className} using the {@code classLoader}
    * @throws ClassNotFoundException if the class is not found
    */
  @throws[ClassNotFoundException]
  def getClass(classLoader: ClassLoader, className: String, initialize: Boolean): Class[_] = try {
    var clazz: Class[_] = null
    if (namePrimitiveMap.contains(className)) clazz = namePrimitiveMap(className)
    else clazz = Class.forName(toCanonicalName(className), initialize, classLoader)
    clazz
  } catch {
    case ex: ClassNotFoundException =>
      // allow path separators (.) as inner class name separators
      val lastDotIndex = className.lastIndexOf(PACKAGE_SEPARATOR_CHAR.toInt)
      if (lastDotIndex != -1) try {
        return getClass(classLoader, className.substring(0, lastDotIndex) + INNER_CLASS_SEPARATOR_CHAR + className.substring(lastDotIndex + 1), initialize)
      } catch {
        case _: ClassNotFoundException => // NOPMD
        // ignore exception
      }
      throw ex
  }

  /**
    * Returns the (initialized) class represented by {@code className}
    * using the {@code classLoader}.  This implementation supports
    * the syntaxes "{@code java.util.Map.Entry[]}",
    * "{@code java.util.Map$Entry[]}", "{@code [Ljava.util.Map.Entry;}",
    * and "{@code [Ljava.util.Map$Entry;}".
    *
    * @param classLoader the class loader to use to load the class
    * @param className   the class name
    * @return the class represented by {@code className} using the {@code classLoader}
    * @throws ClassNotFoundException if the class is not found
    */
  @throws[ClassNotFoundException]
  def getClass(classLoader: ClassLoader, className: String): Class[_] =
    getClass(classLoader, className, true)

  /**
    * Returns the (initialized) class represented by {@code className}
    * using the current thread's context class loader. This implementation
    * supports the syntaxes "{@code java.util.Map.Entry[]}",
    * "{@code java.util.Map$Entry[]}", "{@code [Ljava.util.Map.Entry;}",
    * and "{@code [Ljava.util.Map$Entry;}".
    *
    * @param className the class name
    * @return the class represented by {@code className} using the current thread's context class loader
    * @throws ClassNotFoundException if the class is not found
    */
  @throws[ClassNotFoundException]
  def getClass(className: String): Class[_] = getClass(className, true)

  /**
    * Returns the class represented by {@code className} using the
    * current thread's context class loader. This implementation supports the
    * syntaxes "{@code java.util.Map.Entry[]}", "{@code java.util.Map$Entry[]}",
    * "{@code [Ljava.util.Map.Entry;}", and "{@code [Ljava.util.Map$Entry;}".
    *
    * @param className  the class name
    * @param initialize whether the class must be initialized
    * @return the class represented by {@code className} using the current thread's context class loader
    * @throws ClassNotFoundException if the class is not found
    */
  @throws[ClassNotFoundException]
  def getClass(className: String, initialize: Boolean): Class[_] = {
    val contextCL = Thread.currentThread.getContextClassLoader
    val loader =
      if (contextCL == null) this.getClass().getClassLoader
      else contextCL

    getClass(loader, className, initialize)
  }

  /**
    * <p>Returns the desired Method much like {@code Class.getMethod}, however
    * it ensures that the returned Method is from a public class or interface and not
    * from an anonymous inner class. This means that the Method is invokable and
    * doesn't fall foul of Java bug
    * <a href="http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=4071957">4071957</a>).</p>
    *
    * <pre>
    * <code>Set set = Collections.unmodifiableSet(...);
    * Method method = ClassUtils.getPublicMethod(set.getClass(), "isEmpty",  new Class[0]);
    * Object result = method.invoke(set, new Object[]);</code>
    * </pre>
    *
    * @param cls            the class to check, not null
    * @param methodName     the name of the method
    * @param parameterTypes the list of parameters
    * @return the method
    * @throws NullPointerException  if the class is null
    * @throws SecurityException     if a security violation occurred
    * @throws NoSuchMethodException if the method is not found in the given class
    *                               or if the method doesn't conform with the requirements
    */
  @throws[NoSuchMethodException]
  def getPublicMethod(cls: Class[_], methodName: String, parameterTypes: Class[_]*): Method = {
    val declaredMethod = cls.getMethod(methodName, parameterTypes:_*)
    if (Modifier.isPublic(declaredMethod.getDeclaringClass.getModifiers)) return declaredMethod
    val candidateClasses = new util.ArrayList[Class[_]]()
    candidateClasses.addAll(getAllInterfaces(cls))
    candidateClasses.addAll(getAllSuperclasses(cls))

    candidateClasses.iterator.asScala.filter{ cls: Class[_] =>
      Modifier.isPublic(cls.getModifiers)
    }.foreach { cls: Class[_] =>
      var candidateMethod: Method = null
      try {
        candidateMethod = cls.getMethod(methodName, parameterTypes:_*)
      } catch {
        case _: NoSuchMethodException =>
      }
      if (candidateMethod != null && Modifier.isPublic(candidateMethod.getDeclaringClass.getModifiers))
        return candidateMethod
    }

    throw new NoSuchMethodException("Can't find a public method for " + methodName + " " + ArrayUtils.toString(parameterTypes.toArray))
  }

  /**
    * Converts a class name to a JLS style class name.
    *
    * @param className the class name
    * @return the converted name
    */
  private def toCanonicalName(className: String) = {
    Validate.notNull(className, "className must not be null.")
    var _className = className
    _className = StringUtils.deleteWhitespace(_className)

    if (_className.endsWith("[]")) {
      val classNameBuffer = new StringBuilder
      while (_className.endsWith("[]")) {
        _className = _className.substring(0, _className.length - 2)
        classNameBuffer.append("[")
      }
      val abbreviation = abbreviationMap.getOrElse(_className, null)
      if (abbreviation != null) classNameBuffer.append(abbreviation)
      else classNameBuffer.append("L").append(_className).append(";")
      _className = classNameBuffer.toString
    }

    _className
  }

  /**
    * <p>Converts an array of {@code Object} in to an array of {@code Class} objects.
    * If any of these objects is null, a null element will be inserted into the array.</p>
    *
    * <p>This method returns {@code null} for a {@code null} input array.</p>
    *
    * @param array an {@code Object} array
    * @return a {@code Class} array, {@code null} if null array input
    * @since 2.4
    */
  def toClass(array: Any*): Array[Class[_]] = {
    if (array == null) return null
    if (array.length == 0) return ArrayUtils.EMPTY_CLASS_ARRAY

    val classes = new Array[Class[_]](array.length)
    for (i <- 0 until array.length) {
      classes(i) =
        if (array(i) == null) null
        else array(i).getClass
    }

    classes
  }

  /**
    * <p>Gets the canonical name minus the package name for an {@code Object}.</p>
    *
    * @param object      the class to get the short name for, may be null
    * @param valueIfNull the value to return if null
    * @return the canonical name of the object without the package name, or the null value
    * @since 2.4
    */
  def getShortCanonicalName(`object`: Any, valueIfNull: String): String =
    if (`object` == null) valueIfNull
    else getShortCanonicalName(`object`.getClass.getName)

  /**
    * <p>Gets the canonical class name for a {@code Class}.</p>
    *
    * @param cls the class for which to get the canonical class name; may be null
    * @return the canonical name of the class, or the empty String
    * @since 3.7
    * @see Class#getCanonicalName()
    */
  def getCanonicalName(cls: Class[_]): String =
    getCanonicalName(cls, StringUtils.EMPTY)

  /**
    * <p>Gets the canonical name for a {@code Class}.</p>
    *
    * @param cls         the class for which to get the canonical class name; may be null
    * @param valueIfNull the return value if null
    * @return the canonical name of the class, or {@code valueIfNull}
    * @since 3.7
    * @see Class#getCanonicalName()
    */
  def getCanonicalName(cls: Class[_], valueIfNull: String): String = {
    if (cls == null) return valueIfNull

    val canonicalName = cls.getCanonicalName
    if (canonicalName == null) valueIfNull
    else canonicalName
  }

  /**
    * <p>Gets the canonical name for an {@code Object}.</p>
    *
    * @param object the object for which to get the canonical class name; may be null
    * @return the canonical name of the object, or the empty String
    * @since 3.7
    * @see Class#getCanonicalName()
    */
  def getCanonicalName(`object`: Any): String = getCanonicalName(`object`, StringUtils.EMPTY)

  /**
    * <p>Gets the canonical name for an {@code Object}.</p>
    *
    * @param object      the object for which to get the canonical class name; may be null
    * @param valueIfNull the return value if null
    * @return the canonical name of the object or {@code valueIfNull}
    * @since 3.7
    * @see Class#getCanonicalName()
    */
  def getCanonicalName(`object`: Any, valueIfNull: String): String = {
    if (`object` == null) return valueIfNull

    val canonicalName = `object`.getClass.getCanonicalName
    if (canonicalName == null) valueIfNull
    else canonicalName
  }

  /**
    * <p>Gets the canonical name minus the package name from a {@code Class}.</p>
    *
    * @param cls the class for which to get the short canonical class name; may be null
    * @return the canonical name without the package name or an empty string
    * @since 2.4
    */
  def getShortCanonicalName(cls: Class[_]): String =
    if (cls == null) StringUtils.EMPTY
    else getShortCanonicalName(cls.getName)

  /**
    * <p>Gets the canonical name minus the package name from a String.</p>
    *
    * <p>The string passed in is assumed to be a class name - it is not checked.</p>
    *
    * <p>Note that this method is mainly designed to handle the arrays and primitives properly.
    * If the class is an inner class then the result value will not contain the outer classes.
    * This way the behavior of this method is different from {@link #getShortClassName ( String )}.
    * The argument in that case is class name and not canonical name and the return value
    * retains the outer classes.</p>
    *
    * <p>Note that there is no way to reliably identify the part of the string representing the
    * package hierarchy and the part that is the outer class or classes in case of an inner class.
    * Trying to find the class would require reflective call and the class itself may not even be
    * on the class path. Relying on the fact that class names start with capital letter and packages
    * with lower case is heuristic.</p>
    *
    * <p>It is recommended to use {@link #getShortClassName ( String )} for cases when the class
    * is an inner class and use this method for cases it is designed for.</p>
    *
    * <table>
    * <caption>Examples</caption>
    * <tr><td>return value</td><td>input</td></tr>
    * <tr><td>{@code ""}</td><td>{@code (String)null}</td></tr>
    * <tr><td>{@code "Map.Entry"}</td><td>{@code java.util.Map.Entry.class.getName()}</td></tr>
    * <tr><td>{@code "Entry"}</td><td>{@code java.util.Map.Entry.class.getCanonicalName()}</td></tr>
    * <tr><td>{@code "ClassUtils"}</td><td>{@code "org.apache.commons.lang3.ClassUtils"}</td></tr>
    * <tr><td>{@code "ClassUtils[]"}</td><td>{@code "[Lorg.apache.commons.lang3.ClassUtils;"}</td></tr>
    * <tr><td>{@code "ClassUtils[][]"}</td><td>{@code "[[Lorg.apache.commons.lang3.ClassUtils;"}</td></tr>
    * <tr><td>{@code "ClassUtils[]"}</td><td>{@code "org.apache.commons.lang3.ClassUtils[]"}</td></tr>
    * <tr><td>{@code "ClassUtils[][]"}</td><td>{@code "org.apache.commons.lang3.ClassUtils[][]"}</td></tr>
    * <tr><td>{@code "int[]"}</td><td>{@code "[I"}</td></tr>
    * <tr><td>{@code "int[]"}</td><td>{@code int[].class.getCanonicalName()}</td></tr>
    * <tr><td>{@code "int[]"}</td><td>{@code int[].class.getName()}</td></tr>
    * <tr><td>{@code "int[][]"}</td><td>{@code "[[I"}</td></tr>
    * <tr><td>{@code "int[]"}</td><td>{@code "int[]"}</td></tr>
    * <tr><td>{@code "int[][]"}</td><td>{@code "int[][]"}</td></tr>
    * </table>
    *
    * @param canonicalName the class name to get the short name for
    * @return the canonical name of the class without the package name or an empty string
    * @since 2.4
    */
  def getShortCanonicalName(canonicalName: String): String =
    getShortClassName(getCanonicalName(canonicalName))

  /**
    * <p>Gets the package name from the class name of an {@code Object}.</p>
    *
    * @param object      the class to get the package name for, may be null
    * @param valueIfNull the value to return if null
    * @return the package name of the object, or the null value
    * @since 2.4
    */
  def getPackageCanonicalName(`object`: Any, valueIfNull: String): String =
    if (`object` == null) valueIfNull
    else getPackageCanonicalName(`object`.getClass.getName)

  /**
    * <p>Gets the package name from the canonical name of a {@code Class}.</p>
    *
    * @param cls the class to get the package name for, may be {@code null}.
    * @return the package name or an empty string
    * @since 2.4
    */
  def getPackageCanonicalName(cls: Class[_]): String =
    if (cls == null) StringUtils.EMPTY
    else getPackageCanonicalName(cls.getName)

  /**
    * <p>Gets the package name from the class name. </p>
    *
    * <p>The string passed in is assumed to be a class name - it is not checked.</p>
    * <p>If the class is in the default package, return an empty string.</p>
    *
    * @param name the name to get the package name for, may be {@code null}
    * @return the package name or an empty string
    * @since 2.4
    */
  def getPackageCanonicalName(name: String): String =
    getPackageName(getCanonicalName(name))

  /**
    * <p>Converts a given name of class into canonical format.
    * If name of class is not a name of array class it returns
    * unchanged name.</p>
    *
    * <p>The method does not change the {@code $} separators in case
    * the class is inner class.</p>
    *
    * <p>Example:
    * <ul>
    * <li>{@code getCanonicalName("[I") = "int[]"}</li>
    * <li>{@code getCanonicalName("[Ljava.lang.String;") = "java.lang.String[]"}</li>
    * <li>{@code getCanonicalName("java.lang.String") = "java.lang.String"}</li>
    * </ul>
    * </p>
    *
    * @param className the name of class
    * @return canonical form of class name
    * @since 2.4
    */
  private def getCanonicalName(className: String): String = {
    if (className == null) return null

    var _className = StringUtils.deleteWhitespace(className)

    var dim = 0
    while (_className.startsWith("[")) {
      dim += 1
      _className = _className.substring(1)
    }
    if (dim < 1) return _className

    if (_className.startsWith("L"))
      _className = _className.substring(
        1,
        if (_className.endsWith(";")) _className.length - 1
        else _className.length
      )
    else if (!_className.isEmpty) _className = reverseAbbreviationMap(_className.substring(0, 1))

    val canonicalClassNameBuffer = new StringBuilder(_className)
    for (_ <- 0 until dim) {
      canonicalClassNameBuffer.append("[]")
    }

    canonicalClassNameBuffer.toString
  }

  /**
    * Gets an {@link Iterable} that can iterate over a class hierarchy in ascending (subclass to superclass) order,
    * excluding interfaces.
    *
    * @param type the type to get the class hierarchy from
    * @return Iterable an Iterable over the class hierarchy of the given class
    * @since 3.2
    */
  def hierarchy(`type`: Class[_]): Iterable[Class[_]] = hierarchy(`type`, Interfaces.EXCLUDE)

  /**
    * Gets an {@link Iterable} that can iterate over a class hierarchy in ascending (subclass to superclass) order.
    *
    * @param type               the type to get the class hierarchy from
    * @param interfacesBehavior switch indicating whether to include or exclude interfaces
    * @return Iterable an Iterable over the class hierarchy of the given class
    * @since 3.2
    */
  def hierarchy(`type`: Class[_], interfacesBehavior: ClassUtils.Interfaces.Value): Iterable[Class[_]] = {

    val classes: Iterable[Class[_]] = new Iterable[Class[_]] {
      private val nextObject: MutableObject[Class[_]] = new MutableObject[Class[_]](`type`)

      override val iterator: Iterator[Class[_]] = new Iterator[Class[_]] {
        override def hasNext = nextObject.getValue != null

        override def next(): Class[_] = {
          val result = nextObject.getValue
          nextObject.setValue(result.getSuperclass)
          result
        }

//        def remove(): Unit = {
//          throw new UnsupportedOperationException
//        }
      }
    }

    if (interfacesBehavior ne Interfaces.INCLUDE) return classes

    val seenInterfaces = new util.HashSet[Class[_]]()
    val wrapped = classes.iterator

    new Iterable[Class[_]] {
      private[lang3] var interfaces = Collections.emptySet[Class[_]].iterator

      override val iterator: Iterator[Class[_]] = new Iterator[Class[_]] {
        override def hasNext: Boolean = return interfaces.hasNext || wrapped.hasNext

        override def next(): Class[_] = {
          if (interfaces.hasNext) {
            val nextInterface = interfaces.next
            seenInterfaces.add(nextInterface)
            return nextInterface
          }

          val nextSuperclass = wrapped.next()
          val currentInterfaces = new util.LinkedHashSet[Class[_]]
          walkInterfaces(currentInterfaces, nextSuperclass)
          interfaces = currentInterfaces.iterator
          nextSuperclass
        }

        private def walkInterfaces(addTo: util.Set[Class[_]], c: Class[_]): Unit = {
          for (iface <- c.getInterfaces) {
            if (!seenInterfaces.contains(iface)) addTo.add(iface)
            walkInterfaces(addTo, iface)
          }
        }

//        def remove(): Unit = throw new UnsupportedOperationException
      }
    }
  }
}
