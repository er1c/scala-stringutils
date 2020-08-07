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

import java.io.IOException
import java.io.Writer
import org.apache.commons.lang3.text.translate.AggregateTranslator
import org.apache.commons.lang3.text.translate.CharSequenceTranslator
import org.apache.commons.lang3.text.translate.EntityArrays
import org.apache.commons.lang3.text.translate.JavaUnicodeEscaper
import org.apache.commons.lang3.text.translate.LookupTranslator
import org.apache.commons.lang3.text.translate.NumericEntityEscaper
import org.apache.commons.lang3.text.translate.NumericEntityUnescaper
import org.apache.commons.lang3.text.translate.OctalUnescaper
import org.apache.commons.lang3.text.translate.UnicodeUnescaper
import org.apache.commons.lang3.text.translate.UnicodeUnpairedSurrogateRemover

/**
  * <p>Escapes and unescapes {@code String}s for
  * Java, Java Script, HTML and XML.</p>
  *
  * <p>#ThreadSafe#</p>
  *
  * @since 2.0
  * @deprecated as of 3.6, use commons-text
  *             <a href="https://commons.apache.org/proper/commons-text/javadocs/api-release/org/apache/commons/text/StringEscapeUtils.html">
  *             StringEscapeUtils</a> instead
  */
@deprecated object StringEscapeUtils {
  /**
    * Translator object for escaping Java.
    *
    * While {@link #escapeJava ( String )} is the expected method of use, this
    * object allows the Java escaping functionality to be used
    * as the foundation for a custom translator.
    *
    * @since 3.0
    */
  val ESCAPE_JAVA: CharSequenceTranslator = new LookupTranslator(Array("\"", "\\\""), Array("\\", "\\\\"))
    .`with`(new LookupTranslator(EntityArrays.JAVA_CTRL_CHARS_ESCAPE:_*))
    .`with`(JavaUnicodeEscaper.outsideOf(32, 0x7f))

  /**
    * Translator object for escaping EcmaScript/JavaScript.
    *
    * While {@link #escapeEcmaScript ( String )} is the expected method of use, this
    * object allows the EcmaScript escaping functionality to be used
    * as the foundation for a custom translator.
    *
    * @since 3.0
    */
  val ESCAPE_ECMASCRIPT = new AggregateTranslator(
    new LookupTranslator(Array("'", "\\'"), Array("\"", "\\\""), Array("\\", "\\\\"), Array("/", "\\/")),
    new LookupTranslator(EntityArrays.JAVA_CTRL_CHARS_ESCAPE:_*),
    JavaUnicodeEscaper.outsideOf(32, 0x7f)
  )

  /**
    * Translator object for escaping Json.
    *
    * While {@link #escapeJson ( String )} is the expected method of use, this
    * object allows the Json escaping functionality to be used
    * as the foundation for a custom translator.
    *
    * @since 3.2
    */
  val ESCAPE_JSON = new AggregateTranslator(
    new LookupTranslator(Array("\"", "\\\""), Array("\\", "\\\\"), Array("/", "\\/")),
    new LookupTranslator(EntityArrays.JAVA_CTRL_CHARS_ESCAPE:_*),
    JavaUnicodeEscaper.outsideOf(32, 0x7f)
  )

/*
   public static final CharSequenceTranslator ESCAPE_JSON =
        new AggregateTranslator(
            new LookupTranslator(
                      new String[][] {
                            {"\"", "\\\""},
                            {"\\", "\\\\"},
                            {"/", "\\/"}
                      }),
            new LookupTranslator(EntityArrays.JAVA_CTRL_CHARS_ESCAPE()),
            JavaUnicodeEscaper.outsideOf(32, 0x7f)
        );
 */

  /**
    * Translator object for escaping XML.
    *
    * While {@link #escapeXml ( String )} is the expected method of use, this
    * object allows the XML escaping functionality to be used
    * as the foundation for a custom translator.
    *
    * @since 3.0
    * @deprecated use {@link #ESCAPE_XML10} or {@link #ESCAPE_XML11} instead.
    */
  @deprecated val ESCAPE_XML = new AggregateTranslator(
    new LookupTranslator(EntityArrays.BASIC_ESCAPE:_*),
    new LookupTranslator(EntityArrays.APOS_ESCAPE:_*)
  )

  /**
    * Translator object for escaping XML 1.0.
    *
    * While {@link #escapeXml10 ( String )} is the expected method of use, this
    * object allows the XML escaping functionality to be used
    * as the foundation for a custom translator.
    *
    * @since 3.3
    */
  val ESCAPE_XML10 = new AggregateTranslator(
    new LookupTranslator(EntityArrays.BASIC_ESCAPE:_*),
    new LookupTranslator(EntityArrays.APOS_ESCAPE:_*),
    new LookupTranslator(Array("\u0000", StringUtils.EMPTY), Array("\u0001", StringUtils.EMPTY), Array("\u0002", StringUtils.EMPTY), Array("\u0003", StringUtils.EMPTY), Array("\u0004", StringUtils.EMPTY), Array("\u0005", StringUtils.EMPTY), Array("\u0006", StringUtils.EMPTY), Array("\u0007", StringUtils.EMPTY), Array("\u0008", StringUtils.EMPTY), Array("\u000b", StringUtils.EMPTY), Array("\u000c", StringUtils.EMPTY), Array("\u000e", StringUtils.EMPTY), Array("\u000f", StringUtils.EMPTY), Array("\u0010", StringUtils.EMPTY), Array("\u0011", StringUtils.EMPTY), Array("\u0012", StringUtils.EMPTY), Array("\u0013", StringUtils.EMPTY), Array("\u0014", StringUtils.EMPTY), Array("\u0015", StringUtils.EMPTY), Array("\u0016", StringUtils.EMPTY), Array("\u0017", StringUtils.EMPTY), Array("\u0018", StringUtils.EMPTY), Array("\u0019", StringUtils.EMPTY), Array("\u001a", StringUtils.EMPTY), Array("\u001b", StringUtils.EMPTY), Array("\u001c", StringUtils.EMPTY), Array("\u001d", StringUtils.EMPTY), Array("\u001e", StringUtils.EMPTY), Array("\u001f", StringUtils.EMPTY), Array("\ufffe", StringUtils.EMPTY), Array("\uffff", StringUtils.EMPTY)),
    NumericEntityEscaper.between(0x7f, 0x84),
    NumericEntityEscaper.between(0x86, 0x9f),
    new UnicodeUnpairedSurrogateRemover
  )

  /**
    * Translator object for escaping XML 1.1.
    *
    * While {@link #escapeXml11 ( String )} is the expected method of use, this
    * object allows the XML escaping functionality to be used
    * as the foundation for a custom translator.
    *
    * @since 3.3
    */
  val ESCAPE_XML11 = new AggregateTranslator(
    new LookupTranslator(EntityArrays.BASIC_ESCAPE:_*),
    new LookupTranslator(EntityArrays.APOS_ESCAPE:_*),
    new LookupTranslator(Array("\u0000", StringUtils.EMPTY), Array("\u000b", "&#11;"), Array("\u000c", "&#12;"), Array("\ufffe", StringUtils.EMPTY), Array("\uffff", StringUtils.EMPTY)),
    NumericEntityEscaper.between(0x1, 0x8),
    NumericEntityEscaper.between(0xe, 0x1f),
    NumericEntityEscaper.between(0x7f, 0x84),
    NumericEntityEscaper.between(0x86, 0x9f),
    new UnicodeUnpairedSurrogateRemover
  )

  /**
    * Translator object for escaping HTML version 3.0.
    *
    * While {@link #escapeHtml3 ( String )} is the expected method of use, this
    * object allows the HTML escaping functionality to be used
    * as the foundation for a custom translator.
    *
    * @since 3.0
    */
  val ESCAPE_HTML3 = new AggregateTranslator(
    new LookupTranslator(EntityArrays.BASIC_ESCAPE:_*),
    new LookupTranslator(EntityArrays.ISO8859_1_ESCAPE:_*)
  )

  /**
    * Translator object for escaping HTML version 4.0.
    *
    * While {@link #escapeHtml4 ( String )} is the expected method of use, this
    * object allows the HTML escaping functionality to be used
    * as the foundation for a custom translator.
    *
    * @since 3.0
    */
  val ESCAPE_HTML4 = new AggregateTranslator(
    new LookupTranslator(EntityArrays.BASIC_ESCAPE:_*),
    new LookupTranslator(EntityArrays.ISO8859_1_ESCAPE:_*),
    new LookupTranslator(EntityArrays.HTML40_EXTENDED_ESCAPE:_*)
  )

  /**
    * Translator object for escaping individual Comma Separated Values.
    *
    * While {@link #escapeCsv ( String )} is the expected method of use, this
    * object allows the CSV escaping functionality to be used
    * as the foundation for a custom translator.
    *
    * @since 3.0
    */
  val ESCAPE_CSV = new StringEscapeUtils.CsvEscaper

  // TODO: Create a parent class - 'SinglePassTranslator' ?
  //       It would handle the index checking + length returning,
  //       and could also have an optimization check method.
  private[lang3] object CsvEscaper {
    private val CSV_DELIMITER = ','
    private val CSV_QUOTE = '"'
    private val CSV_QUOTE_STR = String.valueOf(CSV_QUOTE)
    private val CSV_SEARCH_CHARS = Array(CSV_DELIMITER, CSV_QUOTE, CharUtils.CR, CharUtils.LF)
  }

  private[lang3] class CsvEscaper extends CharSequenceTranslator {
    @throws[IOException]
    override def translate(input: CharSequence, index: Int, out: Writer): Int = {
      if (index != 0) throw new IllegalStateException("CsvEscaper should never reach the [1] index")
      if (StringUtils.containsNone(input.toString, CsvEscaper.CSV_SEARCH_CHARS:_*)) out.write(input.toString)
      else {
        out.write(CsvEscaper.CSV_QUOTE.toInt)
        out.write(StringUtils.replace(input.toString, CsvEscaper.CSV_QUOTE_STR, CsvEscaper.CSV_QUOTE_STR + CsvEscaper.CSV_QUOTE_STR))
        out.write(CsvEscaper.CSV_QUOTE.toInt)
      }
      Character.codePointCount(input, 0, input.length)
    }
  }

  /**
    * Translator object for unescaping escaped Java.
    *
    * While {@link #unescapeJava ( String )} is the expected method of use, this
    * object allows the Java unescaping functionality to be used
    * as the foundation for a custom translator.
    *
    * @since 3.0
    */
  // TODO: throw "illegal character: \92" as an Exception if a \ on the end of the Java (as per the compiler)?
  val UNESCAPE_JAVA = new AggregateTranslator(
    new OctalUnescaper, // .between('\1', '\377'),
    new UnicodeUnescaper,
    new LookupTranslator(EntityArrays.JAVA_CTRL_CHARS_UNESCAPE:_*),
    new LookupTranslator(Array("\\\\", "\\"), Array("\\\"", "\""), Array("\\'", "'"), Array("\\", ""))
  )

  /**
    * Translator object for unescaping escaped EcmaScript.
    *
    * While {@link #unescapeEcmaScript ( String )} is the expected method of use, this
    * object allows the EcmaScript unescaping functionality to be used
    * as the foundation for a custom translator.
    *
    * @since 3.0
    */
  val UNESCAPE_ECMASCRIPT: CharSequenceTranslator = UNESCAPE_JAVA
  /**
    * Translator object for unescaping escaped Json.
    *
    * While {@link #unescapeJson ( String )} is the expected method of use, this
    * object allows the Json unescaping functionality to be used
    * as the foundation for a custom translator.
    *
    * @since 3.2
    */
  val UNESCAPE_JSON: CharSequenceTranslator = UNESCAPE_JAVA
  /**
    * Translator object for unescaping escaped HTML 3.0.
    *
    * While {@link #unescapeHtml3 ( String )} is the expected method of use, this
    * object allows the HTML unescaping functionality to be used
    * as the foundation for a custom translator.
    *
    * @since 3.0
    */
  val UNESCAPE_HTML3 = new AggregateTranslator(
    new LookupTranslator(EntityArrays.BASIC_UNESCAPE:_*),
    new LookupTranslator(EntityArrays.ISO8859_1_UNESCAPE:_*),
    new NumericEntityUnescaper
  )

  /**
    * Translator object for unescaping escaped HTML 4.0.
    *
    * While {@link #unescapeHtml4 ( String )} is the expected method of use, this
    * object allows the HTML unescaping functionality to be used
    * as the foundation for a custom translator.
    *
    * @since 3.0
    */
  val UNESCAPE_HTML4 = new AggregateTranslator(
    new LookupTranslator(EntityArrays.BASIC_UNESCAPE:_*),
    new LookupTranslator(EntityArrays.ISO8859_1_UNESCAPE:_*),
    new LookupTranslator(EntityArrays.HTML40_EXTENDED_UNESCAPE:_*),
    new NumericEntityUnescaper
  )

  /**
    * Translator object for unescaping escaped XML.
    *
    * While {@link #unescapeXml ( String )} is the expected method of use, this
    * object allows the XML unescaping functionality to be used
    * as the foundation for a custom translator.
    *
    * @since 3.0
    */
  val UNESCAPE_XML = new AggregateTranslator(
    new LookupTranslator(EntityArrays.BASIC_UNESCAPE:_*),
    new LookupTranslator(EntityArrays.APOS_UNESCAPE:_*),
    new NumericEntityUnescaper
  )

  /**
    * Translator object for unescaping escaped Comma Separated Value entries.
    *
    * While {@link #unescapeCsv ( String )} is the expected method of use, this
    * object allows the CSV unescaping functionality to be used
    * as the foundation for a custom translator.
    *
    * @since 3.0
    */
  val UNESCAPE_CSV = new StringEscapeUtils.CsvUnescaper

  private[lang3] object CsvUnescaper {
    private val CSV_DELIMITER = ','
    private val CSV_QUOTE = '"'
    private val CSV_QUOTE_STR = String.valueOf(CSV_QUOTE)
    private val CSV_SEARCH_CHARS = Array(CSV_DELIMITER, CSV_QUOTE, CharUtils.CR, CharUtils.LF)
  }

  private[lang3] class CsvUnescaper extends CharSequenceTranslator {
    @throws[IOException]
    override def translate(input: CharSequence, index: Int, out: Writer): Int = {
      if (index != 0) throw new IllegalStateException("CsvUnescaper should never reach the [1] index")
      if (input.charAt(0) != CsvUnescaper.CSV_QUOTE || input.charAt(input.length - 1) != CsvUnescaper.CSV_QUOTE) {
        out.write(input.toString)
        return Character.codePointCount(input, 0, input.length)
      }
      // strip quotes
      val quoteless = input.subSequence(1, input.length - 1).toString
      if (StringUtils.containsAny(quoteless, CsvUnescaper.CSV_SEARCH_CHARS)) { // deal with escaped quotes; ie) ""
        out.write(StringUtils.replace(quoteless, CsvUnescaper.CSV_QUOTE_STR + CsvUnescaper.CSV_QUOTE_STR, CsvUnescaper.CSV_QUOTE_STR))
      }
      else out.write(input.toString)
      Character.codePointCount(input, 0, input.length)
    }
  }

  /**
    * <p>Escapes the characters in a {@code String} using Java String rules.</p>
    *
    * <p>Deals correctly with quotes and control-chars (tab, backslash, cr, ff, etc.) </p>
    *
    * <p>So a tab becomes the characters {@code '\\'} and
    * {@code 't'}.</p>
    *
    * <p>The only difference between Java strings and JavaScript strings
    * is that in JavaScript, a single quote and forward-slash (/) are escaped.</p>
    *
    * <p>Example:</p>
    * <pre>
    * input string: He didn't say, "Stop!"
    * output string: He didn't say, \"Stop!\"
    * </pre>
    *
    * @param input String to escape values in, may be null
    * @return String with escaped values, {@code null} if null string input
    */
  def escapeJava(input: String): String = ESCAPE_JAVA.translate(input)

  /**
    * <p>Escapes the characters in a {@code String} using EcmaScript String rules.</p>
    * <p>Escapes any values it finds into their EcmaScript String form.
    * Deals correctly with quotes and control-chars (tab, backslash, cr, ff, etc.) </p>
    *
    * <p>So a tab becomes the characters {@code '\\'} and
    * {@code 't'}.</p>
    *
    * <p>The only difference between Java strings and EcmaScript strings
    * is that in EcmaScript, a single quote and forward-slash (/) are escaped.</p>
    *
    * <p>Note that EcmaScript is best known by the JavaScript and ActionScript dialects. </p>
    *
    * <p>Example:</p>
    * <pre>
    * input string: He didn't say, "Stop!"
    * output string: He didn\'t say, \"Stop!\"
    * </pre>
    *
    * @param input String to escape values in, may be null
    * @return String with escaped values, {@code null} if null string input
    * @since 3.0
    */
  def escapeEcmaScript(input: String): String = ESCAPE_ECMASCRIPT.translate(input)

  /**
    * <p>Escapes the characters in a {@code String} using Json String rules.</p>
    * <p>Escapes any values it finds into their Json String form.
    * Deals correctly with quotes and control-chars (tab, backslash, cr, ff, etc.) </p>
    *
    * <p>So a tab becomes the characters {@code '\\'} and
    * {@code 't'}.</p>
    *
    * <p>The only difference between Java strings and Json strings
    * is that in Json, forward-slash (/) is escaped.</p>
    *
    * <p>See http://www.ietf.org/rfc/rfc4627.txt for further details. </p>
    *
    * <p>Example:</p>
    * <pre>
    * input string: He didn't say, "Stop!"
    * output string: He didn't say, \"Stop!\"
    * </pre>
    *
    * @param input String to escape values in, may be null
    * @return String with escaped values, {@code null} if null string input
    * @since 3.2
    */
  def escapeJson(input: String): String = ESCAPE_JSON.translate(input)
//
//  /**
//    * <p>Unescapes any Java literals found in the {@code String}.
//    * For example, it will turn a sequence of {@code '\'} and
//    * {@code 'n'} into a newline character, unless the {@code '\'}
//    * is preceded by another {@code '\'}.</p>
//    *
//    * @param input the {@code String} to unescape, may be null
//    * @return a new unescaped {@code String}, {@code null} if null string input
//    */
//  def unescapeJava(input: String): String = UNESCAPE_JAVA.translate(input)
//
//  /**
//    * <p>Unescapes any EcmaScript literals found in the {@code String}.</p>
//    *
//    * <p>For example, it will turn a sequence of {@code '\'} and {@code 'n'}
//    * into a newline character, unless the {@code '\'} is preceded by another
//    * {@code '\'}.</p>
//    *
//    * @see #unescapeJava(String)
//    * @param input the {@code String} to unescape, may be null
//    * @return A new unescaped {@code String}, {@code null} if null string input
//    * @since 3.0
//    */
//  def unescapeEcmaScript(input: String): String = UNESCAPE_ECMASCRIPT.translate(input)
//
//  /**
//    * <p>Unescapes any Json literals found in the {@code String}.</p>
//    *
//    * <p>For example, it will turn a sequence of {@code '\'} and {@code 'n'}
//    * into a newline character, unless the {@code '\'} is preceded by another
//    * {@code '\'}.</p>
//    *
//    * @see #unescapeJava(String)
//    * @param input the {@code String} to unescape, may be null
//    * @return A new unescaped {@code String}, {@code null} if null string input
//    * @since 3.2
//    */
//  def unescapeJson(input: String): String = UNESCAPE_JSON.translate(input)
//
//  /**
//    * <p>Escapes the characters in a {@code String} using HTML entities.</p>
//    *
//    * <p>
//    * For example:
//    * </p>
//    * <p>{@code "bread" &amp; "butter"}</p>
//    * becomes:
//    * <p>
//    * {@code &amp;quot;bread&amp;quot; &amp;amp; &amp;quot;butter&amp;quot;}.
//    * </p>
//    *
//    * <p>Supports all known HTML 4.0 entities, including funky accents.
//    * Note that the commonly used apostrophe escape character (&amp;apos;)
//    * is not a legal entity and so is not supported). </p>
//    *
//    * @param input the {@code String} to escape, may be null
//    * @return a new escaped {@code String}, {@code null} if null string input
//    * @see <a href="http://hotwired.lycos.com/webmonkey/reference/special_characters/">ISO Entities</a>
//    * @see <a href="http://www.w3.org/TR/REC-html32#latin1">HTML 3.2 Character Entities for ISO Latin-1</a>
//    * @see <a href="http://www.w3.org/TR/REC-html40/sgml/entities.html">HTML 4.0 Character entity references</a>
//    * @see <a href="http://www.w3.org/TR/html401/charset.html#h-5.3">HTML 4.01 Character References</a>
//    * @see <a href="http://www.w3.org/TR/html401/charset.html#code-position">HTML 4.01 Code positions</a>
//    * @since 3.0
//    */
//  def escapeHtml4(input: String): String = ESCAPE_HTML4.translate(input)
//
//  /**
//    * <p>Escapes the characters in a {@code String} using HTML entities.</p>
//    * <p>Supports only the HTML 3.0 entities. </p>
//    *
//    * @param input the {@code String} to escape, may be null
//    * @return a new escaped {@code String}, {@code null} if null string input
//    * @since 3.0
//    */
//  def escapeHtml3(input: String): String = ESCAPE_HTML3.translate(input)
//
//  /**
//    * <p>Unescapes a string containing entity escapes to a string
//    * containing the actual Unicode characters corresponding to the
//    * escapes. Supports HTML 4.0 entities.</p>
//    *
//    * <p>For example, the string {@code "&lt;Fran&ccedil;ais&gt;"}
//    * will become {@code "<Français>"}</p>
//    *
//    * <p>If an entity is unrecognized, it is left alone, and inserted
//    * verbatim into the result string. e.g. {@code "&gt;&zzzz;x"} will
//    * become {@code ">&zzzz;x"}.</p>
//    *
//    * @param input the {@code String} to unescape, may be null
//    * @return a new unescaped {@code String}, {@code null} if null string input
//    * @since 3.0
//    */
//  def unescapeHtml4(input: String): String = UNESCAPE_HTML4.translate(input)
//
//  /**
//    * <p>Unescapes a string containing entity escapes to a string
//    * containing the actual Unicode characters corresponding to the
//    * escapes. Supports only HTML 3.0 entities.</p>
//    *
//    * @param input the {@code String} to unescape, may be null
//    * @return a new unescaped {@code String}, {@code null} if null string input
//    * @since 3.0
//    */
//  def unescapeHtml3(input: String): String = UNESCAPE_HTML3.translate(input)
//
//  /**
//    * <p>Escapes the characters in a {@code String} using XML entities.</p>
//    *
//    * <p>For example: {@code "bread" & "butter"} =&gt;
//    * {@code &quot;bread&quot; &amp; &quot;butter&quot;}.
//    * </p>
//    *
//    * <p>Supports only the five basic XML entities (gt, lt, quot, amp, apos).
//    * Does not support DTDs or external entities.</p>
//    *
//    * <p>Note that Unicode characters greater than 0x7f are as of 3.0, no longer
//    * escaped. If you still wish this functionality, you can achieve it
//    * via the following:
//    * {@code StringEscapeUtils.ESCAPE_XML.with( NumericEntityEscaper.between(0x7f, Integer.MAX_VALUE) );}</p>
//    *
//    * @param input the {@code String} to escape, may be null
//    * @return a new escaped {@code String}, {@code null} if null string input
//    * @see #unescapeXml(java.lang.String)
//    * @deprecated use {@link #escapeXml10 ( java.lang.String )} or {@link #escapeXml11 ( java.lang.String )} instead.
//    */
//  @deprecated def escapeXml(input: String): String = ESCAPE_XML.translate(input)
//
//  /**
//    * <p>Escapes the characters in a {@code String} using XML entities.</p>
//    *
//    * <p>For example: {@code "bread" & "butter"} =&gt;
//    * {@code &quot;bread&quot; &amp; &quot;butter&quot;}.
//    * </p>
//    *
//    * <p>Note that XML 1.0 is a text-only format: it cannot represent control
//    * characters or unpaired Unicode surrogate codepoints, even after escaping.
//    * {@code escapeXml10} will remove characters that do not fit in the
//    * following ranges:</p>
//    *
//    * <p>{@code #x9 | #xA | #xD | [#x20-#xD7FF] | [#xE000-#xFFFD] | [#x10000-#x10FFFF]}</p>
//    *
//    * <p>Though not strictly necessary, {@code escapeXml10} will escape
//    * characters in the following ranges:</p>
//    *
//    * <p>{@code [#x7F-#x84] | [#x86-#x9F]}</p>
//    *
//    * <p>The returned string can be inserted into a valid XML 1.0 or XML 1.1
//    * document. If you want to allow more non-text characters in an XML 1.1
//    * document, use {@link #escapeXml11 ( String )}.</p>
//    *
//    * @param input the {@code String} to escape, may be null
//    * @return a new escaped {@code String}, {@code null} if null string input
//    * @see #unescapeXml(java.lang.String)
//    * @since 3.3
//    */
//  def escapeXml10(input: String): String = ESCAPE_XML10.translate(input)
//
//  /**
//    * <p>Escapes the characters in a {@code String} using XML entities.</p>
//    *
//    * <p>For example: {@code "bread" & "butter"} =&gt;
//    * {@code &quot;bread&quot; &amp; &quot;butter&quot;}.
//    * </p>
//    *
//    * <p>XML 1.1 can represent certain control characters, but it cannot represent
//    * the null byte or unpaired Unicode surrogate codepoints, even after escaping.
//    * {@code escapeXml11} will remove characters that do not fit in the following
//    * ranges:</p>
//    *
//    * <p>{@code [#x1-#xD7FF] | [#xE000-#xFFFD] | [#x10000-#x10FFFF]}</p>
//    *
//    * <p>{@code escapeXml11} will escape characters in the following ranges:</p>
//    *
//    * <p>{@code [#x1-#x8] | [#xB-#xC] | [#xE-#x1F] | [#x7F-#x84] | [#x86-#x9F]}</p>
//    *
//    * <p>The returned string can be inserted into a valid XML 1.1 document. Do not
//    * use it for XML 1.0 documents.</p>
//    *
//    * @param input the {@code String} to escape, may be null
//    * @return a new escaped {@code String}, {@code null} if null string input
//    * @see #unescapeXml(java.lang.String)
//    * @since 3.3
//    */
//  def escapeXml11(input: String): String = ESCAPE_XML11.translate(input)
//
//  /**
//    * <p>Unescapes a string containing XML entity escapes to a string
//    * containing the actual Unicode characters corresponding to the
//    * escapes.</p>
//    *
//    * <p>Supports only the five basic XML entities (gt, lt, quot, amp, apos).
//    * Does not support DTDs or external entities.</p>
//    *
//    * <p>Note that numerical \\u Unicode codes are unescaped to their respective
//    * Unicode characters. This may change in future releases. </p>
//    *
//    * @param input the {@code String} to unescape, may be null
//    * @return a new unescaped {@code String}, {@code null} if null string input
//    * @see #escapeXml(String)
//    * @see #escapeXml10(String)
//    * @see #escapeXml11(String)
//    */
//  def unescapeXml(input: String): String = UNESCAPE_XML.translate(input)
//
//  /**
//    * <p>Returns a {@code String} value for a CSV column enclosed in double quotes,
//    * if required.</p>
//    *
//    * <p>If the value contains a comma, newline or double quote, then the
//    * String value is returned enclosed in double quotes.</p>
//    *
//    * <p>Any double quote characters in the value are escaped with another double quote.</p>
//    *
//    * <p>If the value does not contain a comma, newline or double quote, then the
//    * String value is returned unchanged.</p>
//    *
//    * see <a href="http://en.wikipedia.org/wiki/Comma-separated_values">Wikipedia</a> and
//    * <a href="http://tools.ietf.org/html/rfc4180">RFC 4180</a>.
//    *
//    * @param input the input CSV column String, may be null
//    * @return the input String, enclosed in double quotes if the value contains a comma,
//    *         newline or double quote, {@code null} if null string input
//    * @since 2.4
//    */
//  def escapeCsv(input: String): String = ESCAPE_CSV.translate(input)
//
//  /**
//    * <p>Returns a {@code String} value for an unescaped CSV column. </p>
//    *
//    * <p>If the value is enclosed in double quotes, and contains a comma, newline
//    * or double quote, then quotes are removed.
//    * </p>
//    *
//    * <p>Any double quote escaped characters (a pair of double quotes) are unescaped
//    * to just one double quote. </p>
//    *
//    * <p>If the value is not enclosed in double quotes, or is and does not contain a
//    * comma, newline or double quote, then the String value is returned unchanged.</p>
//    *
//    * see <a href="http://en.wikipedia.org/wiki/Comma-separated_values">Wikipedia</a> and
//    * <a href="http://tools.ietf.org/html/rfc4180">RFC 4180</a>.
//    *
//    * @param input the input CSV column String, may be null
//    * @return the input String, with enclosing double quotes removed and embedded double
//    *         quotes unescaped, {@code null} if null string input
//    * @since 2.4
//    */
//  def unescapeCsv(input: String): String = UNESCAPE_CSV.translate(input)
}
