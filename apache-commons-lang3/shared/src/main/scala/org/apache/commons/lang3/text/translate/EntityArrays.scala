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

package org.apache.commons.lang3.text.translate

/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/**
  * Class holding various entity data for HTML and XML - generally for use with
  * the LookupTranslator.
  * All arrays are of length [*][2].
  *
  * @since 3.0
  * @deprecated as of 3.6, use commons-text
  *             <a href="https://commons.apache.org/proper/commons-text/javadocs/api-release/org/apache/commons/text/translate/CodePointTranslator.html">
  *             EntityArrays</a> instead
  */
@deprecated object EntityArrays {
  /**
    * Mapping to escape <a href="https://secure.wikimedia.org/wikipedia/en/wiki/ISO/IEC_8859-1">ISO-8859-1</a>
    * characters to their named HTML 3.x equivalents.
    *
    * @return the mapping table
    */
  def ISO8859_1_ESCAPE: Array[Array[CharSequence]] = _ISO8859_1_ESCAPE.clone

  private val _ISO8859_1_ESCAPE: Array[Array[CharSequence]] = Array(
    Array("\u00A0", "&nbsp;"), // non-breaking space
    Array("\u00A1", "&iexcl;"), // inverted exclamation mark
    Array("\u00A2", "&cent;"), // cent sign
    Array("\u00A3", "&pound;"), // pound sign
    Array("\u00A4", "&curren;"), // currency sign
    Array("\u00A5", "&yen;"), // yen sign = yuan sign
    Array("\u00A6", "&brvbar;"), // broken bar = broken vertical bar
    Array("\u00A7", "&sect;"), // section sign
    Array("\u00A8", "&uml;"), // diaeresis = spacing diaeresis
    Array("\u00A9", "&copy;"), // © - copyright sign
    Array("\u00AA", "&ordf;"), // feminine ordinal indicator
    Array("\u00AB", "&laquo;"), // left-pointing double angle quotation mark = left pointing guillemet
    Array("\u00AC", "&not;"), // not sign
    Array("\u00AD", "&shy;"), // soft hyphen = discretionary hyphen
    Array("\u00AE", "&reg;"), // ® - registered trademark sign
    Array("\u00AF", "&macr;"), // macron = spacing macron = overline = APL overbar
    Array("\u00B0", "&deg;"), // degree sign
    Array("\u00B1", "&plusmn;"), // plus-minus sign = plus-or-minus sign
    Array("\u00B2", "&sup2;"), // superscript two = superscript digit two = squared
    Array("\u00B3", "&sup3;"), // superscript three = superscript digit three = cubed
    Array("\u00B4", "&acute;"), // acute accent = spacing acute
    Array("\u00B5", "&micro;"), // micro sign
    Array("\u00B6", "&para;"), // pilcrow sign = paragraph sign
    Array("\u00B7", "&middot;"), // middle dot = Georgian comma = Greek middle dot
    Array("\u00B8", "&cedil;"), // cedilla = spacing cedilla
    Array("\u00B9", "&sup1;"), // superscript one = superscript digit one
    Array("\u00BA", "&ordm;"), // masculine ordinal indicator
    Array("\u00BB", "&raquo;"), // right-pointing double angle quotation mark = right pointing guillemet
    Array("\u00BC", "&frac14;"), // vulgar fraction one quarter = fraction one quarter
    Array("\u00BD", "&frac12;"), // vulgar fraction one half = fraction one half
    Array("\u00BE", "&frac34;"), // vulgar fraction three quarters = fraction three quarters
    Array("\u00BF", "&iquest;"), // inverted question mark = turned question mark
    Array("\u00C0", "&Agrave;"), // À - uppercase A, grave accent
    Array("\u00C1", "&Aacute;"), // Á - uppercase A, acute accent
    Array("\u00C2", "&Acirc;"), // Â - uppercase A, circumflex accent
    Array("\u00C3", "&Atilde;"), // Ã - uppercase A, tilde
    Array("\u00C4", "&Auml;"), // Ä - uppercase A, umlaut
    Array("\u00C5", "&Aring;"), // Å - uppercase A, ring
    Array("\u00C6", "&AElig;"), // Æ - uppercase AE
    Array("\u00C7", "&Ccedil;"), // Ç - uppercase C, cedilla
    Array("\u00C8", "&Egrave;"), // È - uppercase E, grave accent
    Array("\u00C9", "&Eacute;"), // É - uppercase E, acute accent
    Array("\u00CA", "&Ecirc;"), // Ê - uppercase E, circumflex accent
    Array("\u00CB", "&Euml;"), // Ë - uppercase E, umlaut
    Array("\u00CC", "&Igrave;"), // Ì - uppercase I, grave accent
    Array("\u00CD", "&Iacute;"), // Í - uppercase I, acute accent
    Array("\u00CE", "&Icirc;"), // Î - uppercase I, circumflex accent
    Array("\u00CF", "&Iuml;"), // Ï - uppercase I, umlaut
    Array("\u00D0", "&ETH;"), // Ð - uppercase Eth, Icelandic
    Array("\u00D1", "&Ntilde;"), // Ñ - uppercase N, tilde
    Array("\u00D2", "&Ograve;"), // Ò - uppercase O, grave accent
    Array("\u00D3", "&Oacute;"), // Ó - uppercase O, acute accent
    Array("\u00D4", "&Ocirc;"), // Ô - uppercase O, circumflex accent
    Array("\u00D5", "&Otilde;"), // Õ - uppercase O, tilde
    Array("\u00D6", "&Ouml;"), // Ö - uppercase O, umlaut
    Array("\u00D7", "&times;"), // multiplication sign
    Array("\u00D8", "&Oslash;"), // Ø - uppercase O, slash
    Array("\u00D9", "&Ugrave;"), // Ù - uppercase U, grave accent
    Array("\u00DA", "&Uacute;"), // Ú - uppercase U, acute accent
    Array("\u00DB", "&Ucirc;"), // Û - uppercase U, circumflex accent
    Array("\u00DC", "&Uuml;"), // Ü - uppercase U, umlaut
    Array("\u00DD", "&Yacute;"), // Ý - uppercase Y, acute accent
    Array("\u00DE", "&THORN;"), // Þ - uppercase THORN, Icelandic
    Array("\u00DF", "&szlig;"), // ß - lowercase sharps, German
    Array("\u00E0", "&agrave;"), // à - lowercase a, grave accent
    Array("\u00E1", "&aacute;"), // á - lowercase a, acute accent
    Array("\u00E2", "&acirc;"), // â - lowercase a, circumflex accent
    Array("\u00E3", "&atilde;"), // ã - lowercase a, tilde
    Array("\u00E4", "&auml;"), // ä - lowercase a, umlaut
    Array("\u00E5", "&aring;"), // å - lowercase a, ring
    Array("\u00E6", "&aelig;"), // æ - lowercase ae
    Array("\u00E7", "&ccedil;"), // ç - lowercase c, cedilla
    Array("\u00E8", "&egrave;"), // è - lowercase e, grave accent
    Array("\u00E9", "&eacute;"), // é - lowercase e, acute accent
    Array("\u00EA", "&ecirc;"), // ê - lowercase e, circumflex accent
    Array("\u00EB", "&euml;"), // ë - lowercase e, umlaut
    Array("\u00EC", "&igrave;"), // ì - lowercase i, grave accent
    Array("\u00ED", "&iacute;"), // í - lowercase i, acute accent
    Array("\u00EE", "&icirc;"), // î - lowercase i, circumflex accent
    Array("\u00EF", "&iuml;"), // ï - lowercase i, umlaut
    Array("\u00F0", "&eth;"), // ð - lowercase eth, Icelandic
    Array("\u00F1", "&ntilde;"), // ñ - lowercase n, tilde
    Array("\u00F2", "&ograve;"), // ò - lowercase o, grave accent
    Array("\u00F3", "&oacute;"), // ó - lowercase o, acute accent
    Array("\u00F4", "&ocirc;"), // ô - lowercase o, circumflex accent
    Array("\u00F5", "&otilde;"), // õ - lowercase o, tilde
    Array("\u00F6", "&ouml;"), // ö - lowercase o, umlaut
    Array("\u00F7", "&divide;"), // division sign
    Array("\u00F8", "&oslash;"), // ø - lowercase o, slash
    Array("\u00F9", "&ugrave;"), // ù - lowercase u, grave accent
    Array("\u00FA", "&uacute;"), // ú - lowercase u, acute accent
    Array("\u00FB", "&ucirc;"), // û - lowercase u, circumflex accent
    Array("\u00FC", "&uuml;"), // ü - lowercase u, umlaut
    Array("\u00FD", "&yacute;"), // ý - lowercase y, acute accent
    Array("\u00FE", "&thorn;"), // þ - lowercase thorn, Icelandic
    Array("\u00FF", "&yuml;")
  ) // ÿ - lowercase y, umlaut

  /**
    * Reverse of {@link #ISO8859_1_ESCAPE ( )} for unescaping purposes.
    *
    * @return the mapping table
    */
  def ISO8859_1_UNESCAPE: Array[Array[CharSequence]] = _ISO8859_1_UNESCAPE.clone

  private val _ISO8859_1_UNESCAPE = invert(ISO8859_1_ESCAPE)

  /**
    * Mapping to escape additional <a href="http://www.w3.org/TR/REC-html40/sgml/entities.html">character entity
    * references</a>. Note that this must be used with {@link #ISO8859_1_ESCAPE ( )} to get the full list of
    * HTML 4.0 character entities.
    *
    * @return the mapping table
    */
  def HTML40_EXTENDED_ESCAPE: Array[Array[CharSequence]] = _HTML40_EXTENDED_ESCAPE.clone

  private val _HTML40_EXTENDED_ESCAPE: Array[Array[CharSequence]] = Array( // <!-- Latin Extended-B -->
    Array("\u0192", "&fnof;"), // latin small f with hook = function= florin, U+0192 ISOtech -->
    // <!-- Greek -->
    Array("\u0391", "&Alpha;"), // greek capital letter alpha, U+0391 -->
    Array("\u0392", "&Beta;"), // greek capital letter beta, U+0392 -->
    Array("\u0393", "&Gamma;"), // greek capital letter gamma, U+0393 ISOgrk3 -->
    Array("\u0394", "&Delta;"), // greek capital letter delta, U+0394 ISOgrk3 -->
    Array("\u0395", "&Epsilon;"), // greek capital letter epsilon, U+0395 -->
    Array("\u0396", "&Zeta;"), // greek capital letter zeta, U+0396 -->
    Array("\u0397", "&Eta;"), // greek capital letter eta, U+0397 -->
    Array("\u0398", "&Theta;"), // greek capital letter theta, U+0398 ISOgrk3 -->
    Array("\u0399", "&Iota;"), // greek capital letter iota, U+0399 -->
    Array("\u039A", "&Kappa;"), // greek capital letter kappa, U+039A -->
    Array("\u039B", "&Lambda;"), // greek capital letter lambda, U+039B ISOgrk3 -->
    Array("\u039C", "&Mu;"), // greek capital letter mu, U+039C -->
    Array("\u039D", "&Nu;"), // greek capital letter nu, U+039D -->
    Array("\u039E", "&Xi;"), // greek capital letter xi, U+039E ISOgrk3 -->
    Array("\u039F", "&Omicron;"), // greek capital letter omicron, U+039F -->
    Array("\u03A0", "&Pi;"), // greek capital letter pi, U+03A0 ISOgrk3 -->
    Array("\u03A1", "&Rho;"), // greek capital letter rho, U+03A1 -->
    // <!-- there is no Sigmaf, and no U+03A2 character either -->
    Array("\u03A3", "&Sigma;"), // greek capital letter sigma, U+03A3 ISOgrk3 -->
    Array("\u03A4", "&Tau;"), // greek capital letter tau, U+03A4 -->
    Array("\u03A5", "&Upsilon;"), // greek capital letter upsilon, U+03A5 ISOgrk3 -->
    Array("\u03A6", "&Phi;"), // greek capital letter phi, U+03A6 ISOgrk3 -->
    Array("\u03A7", "&Chi;"), // greek capital letter chi, U+03A7 -->
    Array("\u03A8", "&Psi;"), // greek capital letter psi, U+03A8 ISOgrk3 -->
    Array("\u03A9", "&Omega;"), // greek capital letter omega, U+03A9 ISOgrk3 -->
    Array("\u03B1", "&alpha;"), // greek small letter alpha, U+03B1 ISOgrk3 -->
    Array("\u03B2", "&beta;"), // greek small letter beta, U+03B2 ISOgrk3 -->
    Array("\u03B3", "&gamma;"), // greek small letter gamma, U+03B3 ISOgrk3 -->
    Array("\u03B4", "&delta;"), // greek small letter delta, U+03B4 ISOgrk3 -->
    Array("\u03B5", "&epsilon;"), // greek small letter epsilon, U+03B5 ISOgrk3 -->
    Array("\u03B6", "&zeta;"), // greek small letter zeta, U+03B6 ISOgrk3 -->
    Array("\u03B7", "&eta;"), // greek small letter eta, U+03B7 ISOgrk3 -->
    Array("\u03B8", "&theta;"), // greek small letter theta, U+03B8 ISOgrk3 -->
    Array("\u03B9", "&iota;"), // greek small letter iota, U+03B9 ISOgrk3 -->
    Array("\u03BA", "&kappa;"), // greek small letter kappa, U+03BA ISOgrk3 -->
    Array("\u03BB", "&lambda;"), // greek small letter lambda, U+03BB ISOgrk3 -->
    Array("\u03BC", "&mu;"), // greek small letter mu, U+03BC ISOgrk3 -->
    Array("\u03BD", "&nu;"), // greek small letter nu, U+03BD ISOgrk3 -->
    Array("\u03BE", "&xi;"), // greek small letter xi, U+03BE ISOgrk3 -->
    Array("\u03BF", "&omicron;"), // greek small letter omicron, U+03BF NEW -->
    Array("\u03C0", "&pi;"), // greek small letter pi, U+03C0 ISOgrk3 -->
    Array("\u03C1", "&rho;"), // greek small letter rho, U+03C1 ISOgrk3 -->
    Array("\u03C2", "&sigmaf;"), // greek small letter final sigma, U+03C2 ISOgrk3 -->
    Array("\u03C3", "&sigma;"), // greek small letter sigma, U+03C3 ISOgrk3 -->
    Array("\u03C4", "&tau;"), // greek small letter tau, U+03C4 ISOgrk3 -->
    Array("\u03C5", "&upsilon;"), // greek small letter upsilon, U+03C5 ISOgrk3 -->
    Array("\u03C6", "&phi;"), // greek small letter phi, U+03C6 ISOgrk3 -->
    Array("\u03C7", "&chi;"), // greek small letter chi, U+03C7 ISOgrk3 -->
    Array("\u03C8", "&psi;"), // greek small letter psi, U+03C8 ISOgrk3 -->
    Array("\u03C9", "&omega;"), // greek small letter omega, U+03C9 ISOgrk3 -->
    Array("\u03D1", "&thetasym;"), // greek small letter theta symbol, U+03D1 NEW -->
    Array("\u03D2", "&upsih;"), // greek upsilon with hook symbol, U+03D2 NEW -->
    Array("\u03D6", "&piv;"), // greek pi symbol, U+03D6 ISOgrk3 -->
    // <!-- General Punctuation -->
    Array("\u2022", "&bull;"), // bullet = black small circle, U+2022 ISOpub -->
    // <!-- bullet is NOT the same as bullet operator, U+2219 -->
    Array("\u2026", "&hellip;"), // horizontal ellipsis = three dot leader, U+2026 ISOpub -->
    Array("\u2032", "&prime;"), // prime = minutes = feet, U+2032 ISOtech -->
    Array("\u2033", "&Prime;"), // double prime = seconds = inches, U+2033 ISOtech -->
    Array("\u203E", "&oline;"), // overline = spacing overscore, U+203E NEW -->
    Array("\u2044", "&frasl;"), // fraction slash, U+2044 NEW -->
    // <!-- Letterlike Symbols -->
    Array("\u2118", "&weierp;"), // script capital P = power set= Weierstrass p, U+2118 ISOamso -->
    Array("\u2111", "&image;"), // blackletter capital I = imaginary part, U+2111 ISOamso -->
    Array("\u211C", "&real;"), // blackletter capital R = real part symbol, U+211C ISOamso -->
    Array("\u2122", "&trade;"), // trade mark sign, U+2122 ISOnum -->
    Array("\u2135", "&alefsym;"), // alef symbol = first transfinite cardinal, U+2135 NEW -->
    // <!-- alef symbol is NOT the same as hebrew letter alef, U+05D0 although the
    // same glyph could be used to depict both characters -->
    // <!-- Arrows -->
    Array("\u2190", "&larr;"), // leftwards arrow, U+2190 ISOnum -->
    Array("\u2191", "&uarr;"), // upwards arrow, U+2191 ISOnum-->
    Array("\u2192", "&rarr;"), // rightwards arrow, U+2192 ISOnum -->
    Array("\u2193", "&darr;"), // downwards arrow, U+2193 ISOnum -->
    Array("\u2194", "&harr;"), // left right arrow, U+2194 ISOamsa -->
    Array("\u21B5", "&crarr;"), // downwards arrow with corner leftwards= carriage return, U+21B5 NEW -->
    Array("\u21D0", "&lArr;"), // leftwards double arrow, U+21D0 ISOtech -->
    // <!-- ISO 10646 does not say that lArr is the same as the 'is implied by'
    // arrow but also does not have any other character for that function.
    // So ? lArr canbe used for 'is implied by' as ISOtech suggests -->
    Array("\u21D1", "&uArr;"), // upwards double arrow, U+21D1 ISOamsa -->
    Array("\u21D2", "&rArr;"), // rightwards double arrow, U+21D2 ISOtech -->
    // <!-- ISO 10646 does not say this is the 'implies' character but does not
    // have another character with this function so ?rArr can be used for
    // 'implies' as ISOtech suggests -->
    Array("\u21D3", "&dArr;"), // downwards double arrow, U+21D3 ISOamsa -->
    Array("\u21D4", "&hArr;"), // left right double arrow, U+21D4 ISOamsa -->
    // <!-- Mathematical Operators -->
    Array("\u2200", "&forall;"), // for all, U+2200 ISOtech -->
    Array("\u2202", "&part;"), // partial differential, U+2202 ISOtech -->
    Array("\u2203", "&exist;"), // there exists, U+2203 ISOtech -->
    Array("\u2205", "&empty;"), // empty set = null set = diameter, U+2205 ISOamso -->
    Array("\u2207", "&nabla;"), // nabla = backward difference, U+2207 ISOtech -->
    Array("\u2208", "&isin;"), // element of, U+2208 ISOtech -->
    Array("\u2209", "&notin;"), // not an element of, U+2209 ISOtech -->
    Array("\u220B", "&ni;"), // contains as member, U+220B ISOtech -->
    // <!-- should there be a more memorable name than 'ni'? -->
    Array("\u220F", "&prod;"), // n-ary product = product sign, U+220F ISOamsb -->
    // <!-- prod is NOT the same character as U+03A0 'greek capital letter pi'
    // though the same glyph might be used for both -->
    Array("\u2211", "&sum;"), // n-ary summation, U+2211 ISOamsb -->
    // <!-- sum is NOT the same character as U+03A3 'greek capital letter sigma'
    Array("\u2212", "&minus;"), // minus sign, U+2212 ISOtech -->
    Array("\u2217", "&lowast;"), // asterisk operator, U+2217 ISOtech -->
    Array("\u221A", "&radic;"), // square root = radical sign, U+221A ISOtech -->
    Array("\u221D", "&prop;"), // proportional to, U+221D ISOtech -->
    Array("\u221E", "&infin;"), // infinity, U+221E ISOtech -->
    Array("\u2220", "&ang;"), // angle, U+2220 ISOamso -->
    Array("\u2227", "&and;"), // logical and = wedge, U+2227 ISOtech -->
    Array("\u2228", "&or;"), // logical or = vee, U+2228 ISOtech -->
    Array("\u2229", "&cap;"), // intersection = cap, U+2229 ISOtech -->
    Array("\u222A", "&cup;"), // union = cup, U+222A ISOtech -->
    Array("\u222B", "&int;"), // integral, U+222B ISOtech -->
    Array("\u2234", "&there4;"), // therefore, U+2234 ISOtech -->
    Array("\u223C", "&sim;"), // tilde operator = varies with = similar to, U+223C ISOtech -->
    // <!-- tilde operator is NOT the same character as the tilde, U+007E, although
    // the same glyph might be used to represent both -->
    Array("\u2245", "&cong;"), // approximately equal to, U+2245 ISOtech -->
    Array("\u2248", "&asymp;"), // almost equal to = asymptotic to, U+2248 ISOamsr -->
    Array("\u2260", "&ne;"), // not equal to, U+2260 ISOtech -->
    Array("\u2261", "&equiv;"), // identical to, U+2261 ISOtech -->
    Array("\u2264", "&le;"), // less-than or equal to, U+2264 ISOtech -->
    Array("\u2265", "&ge;"), // greater-than or equal to, U+2265 ISOtech -->
    Array("\u2282", "&sub;"), // subset of, U+2282 ISOtech -->
    Array("\u2283", "&sup;"), // superset of, U+2283 ISOtech -->
    // <!-- note that nsup, 'not a superset of, U+2283' is not covered by the
    // Symbol font encoding and is not included. Should it be, for symmetry?
    // It is in ISOamsn -->,
    Array("\u2284", "&nsub;"), // not a subset of, U+2284 ISOamsn -->
    Array("\u2286", "&sube;"), // subset of or equal to, U+2286 ISOtech -->
    Array("\u2287", "&supe;"), // superset of or equal to, U+2287 ISOtech -->
    Array("\u2295", "&oplus;"), // circled plus = direct sum, U+2295 ISOamsb -->
    Array("\u2297", "&otimes;"), // circled times = vector product, U+2297 ISOamsb -->
    Array("\u22A5", "&perp;"), // up tack = orthogonal to = perpendicular, U+22A5 ISOtech -->
    Array("\u22C5", "&sdot;"), // dot operator, U+22C5 ISOamsb -->
    // <!-- dot operator is NOT the same character as U+00B7 middle dot -->
    // <!-- Miscellaneous Technical -->
    Array("\u2308", "&lceil;"), // left ceiling = apl upstile, U+2308 ISOamsc -->
    Array("\u2309", "&rceil;"), // right ceiling, U+2309 ISOamsc -->
    Array("\u230A", "&lfloor;"), // left floor = apl downstile, U+230A ISOamsc -->
    Array("\u230B", "&rfloor;"), // right floor, U+230B ISOamsc -->
    Array("\u2329", "&lang;"), // left-pointing angle bracket = bra, U+2329 ISOtech -->
    // <!-- lang is NOT the same character as U+003C 'less than' or U+2039 'single left-pointing angle quotation
    // mark' -->
    Array("\u232A", "&rang;"), // right-pointing angle bracket = ket, U+232A ISOtech -->
    // <!-- rang is NOT the same character as U+003E 'greater than' or U+203A
    // 'single right-pointing angle quotation mark' -->
    // <!-- Geometric Shapes -->
    Array("\u25CA", "&loz;"), // lozenge, U+25CA ISOpub -->
    // <!-- Miscellaneous Symbols -->
    Array("\u2660", "&spades;"), // black spade suit, U+2660 ISOpub -->
    // <!-- black here seems to mean filled as opposed to hollow -->
    Array("\u2663", "&clubs;"), // black club suit = shamrock, U+2663 ISOpub -->
    Array("\u2665", "&hearts;"), // black heart suit = valentine, U+2665 ISOpub -->
    Array("\u2666", "&diams;"), // black diamond suit, U+2666 ISOpub -->
    // <!-- Latin Extended-A -->
    Array("\u0152", "&OElig;"), // -- latin capital ligature OE, U+0152 ISOlat2 -->
    Array("\u0153", "&oelig;"), // -- latin small ligature oe, U+0153 ISOlat2 -->
    // <!-- ligature is a misnomer, this is a separate character in some languages -->
    Array("\u0160", "&Scaron;"), // -- latin capital letter S with caron, U+0160 ISOlat2 -->
    Array("\u0161", "&scaron;"), // -- latin small letter s with caron, U+0161 ISOlat2 -->
    Array("\u0178", "&Yuml;"), // -- latin capital letter Y with diaeresis, U+0178 ISOlat2 -->
    // <!-- Spacing Modifier Letters -->
    Array("\u02C6", "&circ;"), // -- modifier letter circumflex accent, U+02C6 ISOpub -->
    Array("\u02DC", "&tilde;"), // small tilde, U+02DC ISOdia -->
    Array("\u2002", "&ensp;"), // en space, U+2002 ISOpub -->
    Array("\u2003", "&emsp;"), // em space, U+2003 ISOpub -->
    Array("\u2009", "&thinsp;"), // thin space, U+2009 ISOpub -->
    Array("\u200C", "&zwnj;"), // zero width non-joiner, U+200C NEW RFC 2070 -->
    Array("\u200D", "&zwj;"), // zero width joiner, U+200D NEW RFC 2070 -->
    Array("\u200E", "&lrm;"), // left-to-right mark, U+200E NEW RFC 2070 -->
    Array("\u200F", "&rlm;"), // right-to-left mark, U+200F NEW RFC 2070 -->
    Array("\u2013", "&ndash;"), // en dash, U+2013 ISOpub -->
    Array("\u2014", "&mdash;"), // em dash, U+2014 ISOpub -->
    Array("\u2018", "&lsquo;"), // left single quotation mark, U+2018 ISOnum -->
    Array("\u2019", "&rsquo;"), // right single quotation mark, U+2019 ISOnum -->
    Array("\u201A", "&sbquo;"), // single low-9 quotation mark, U+201A NEW -->
    Array("\u201C", "&ldquo;"), // left double quotation mark, U+201C ISOnum -->
    Array("\u201D", "&rdquo;"), // right double quotation mark, U+201D ISOnum -->
    Array("\u201E", "&bdquo;"), // double low-9 quotation mark, U+201E NEW -->
    Array("\u2020", "&dagger;"), // dagger, U+2020 ISOpub -->
    Array("\u2021", "&Dagger;"), // double dagger, U+2021 ISOpub -->
    Array("\u2030", "&permil;"), // per mille sign, U+2030 ISOtech -->
    Array("\u2039", "&lsaquo;"), // single left-pointing angle quotation mark, U+2039 ISO proposed -->
    // <!-- lsaquo is proposed but not yet ISO standardized -->
    Array("\u203A", "&rsaquo;"), // single right-pointing angle quotation mark, U+203A ISO proposed -->
    // <!-- rsaquo is proposed but not yet ISO standardized -->
    Array("\u20AC", "&euro;")
  ) // -- euro sign, U+20AC NEW -->

  /**
    * Reverse of {@link #HTML40_EXTENDED_ESCAPE ( )} for unescaping purposes.
    *
    * @return the mapping table
    */
  def HTML40_EXTENDED_UNESCAPE: Array[Array[CharSequence]] = _HTML40_EXTENDED_UNESCAPE.clone

  private val _HTML40_EXTENDED_UNESCAPE = invert(HTML40_EXTENDED_ESCAPE)

  /**
    * Mapping to escape the basic XML and HTML character entities.
    *
    * Namely: {@code " & < >}
    *
    * @return the mapping table
    */
  def BASIC_ESCAPE: Array[Array[CharSequence]] = _BASIC_ESCAPE.clone

  private val _BASIC_ESCAPE: Array[Array[CharSequence]] = Array(
    Array("\"", "&quot;"), // " - double-quote
    Array("&", "&amp;"), // & - ampersand
    Array("<", "&lt;"), // < - less-than
    Array(">", "&gt;")
  ) // > - greater-than

  /**
    * Reverse of {@link #BASIC_ESCAPE ( )} for unescaping purposes.
    *
    * @return the mapping table
    */
  def BASIC_UNESCAPE: Array[Array[CharSequence]] = _BASIC_UNESCAPE.clone

  private val _BASIC_UNESCAPE: Array[Array[CharSequence]] = invert(BASIC_ESCAPE)

  /**
    * Mapping to escape the apostrophe character to its XML character entity.
    *
    * @return the mapping table
    */
  def APOS_ESCAPE: Array[Array[CharSequence]] = _APOS_ESCAPE.clone

  private val _APOS_ESCAPE: Array[Array[CharSequence]] = Array(Array("'", "&apos;")) // XML apostrophe

  /**
    * Reverse of {@link #APOS_ESCAPE ( )} for unescaping purposes.
    *
    * @return the mapping table
    */
  def APOS_UNESCAPE: Array[Array[CharSequence]] = _APOS_UNESCAPE.clone

  private val _APOS_UNESCAPE: Array[Array[CharSequence]] = invert(APOS_ESCAPE)

  /**
    * Mapping to escape the Java control characters.
    *
    * Namely: {@code \b \n \t \f \r}
    *
    * @return the mapping table
    */
  def JAVA_CTRL_CHARS_ESCAPE: Array[Array[CharSequence]] = _JAVA_CTRL_CHARS_ESCAPE.clone

  private val _JAVA_CTRL_CHARS_ESCAPE: Array[Array[CharSequence]] =
    Array(Array("\b", "\\b"), Array("\n", "\\n"), Array("\t", "\\t"), Array("\f", "\\f"), Array("\r", "\\r"))

  /**
    * Reverse of {@link #JAVA_CTRL_CHARS_ESCAPE ( )} for unescaping purposes.
    *
    * @return the mapping table
    */
  def JAVA_CTRL_CHARS_UNESCAPE: Seq[Array[CharSequence]] = _JAVA_CTRL_CHARS_UNESCAPE.clone

  private val _JAVA_CTRL_CHARS_UNESCAPE = invert(JAVA_CTRL_CHARS_ESCAPE)

  /**
    * Used to invert an escape array into an unescape array
    *
    * @param array String[][] to be inverted
    * @return String[][] inverted array
    */
  def invert(array: Array[Array[CharSequence]]): Array[Array[CharSequence]] = {
    val newarray = Array.ofDim[CharSequence](array.length, 2)
    for (i <- 0 until array.length) {
      newarray(i)(0) = array(i)(1)
      newarray(i)(1) = array(i)(0)
    }
    newarray
  }
}
