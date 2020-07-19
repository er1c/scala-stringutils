/*
 * Copyright (c) 2020 the Scala StringUtils contributors.
 * See the project homepage at: https://er1c.github.io/scala-stringutils/
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package stringutils

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

final class TestASCIIFolding extends AnyFunSuite with Matchers {

  private val ascii: String = (0 to 127).map { _.toChar }.mkString

  test("foldAsChar - 0-127 ascii") {
    ASCIIFolding.foldAsChar('a') shouldBe 'a'
    ASCIIFolding.foldAsChar('*') shouldBe '*'

    ascii.foreach { ch: Char => ASCIIFolding.foldAsChar(ch) shouldBe ch }
  }

  test("foldAsChar - accents") {
    ASCIIFolding.foldAsChar('\u204E') shouldBe '*'
    ASCIIFolding.foldAsChar('\u2052') shouldBe '%'
    ASCIIFolding.foldAsChar('Æ') shouldBe 'Æ'
  }

  test("fold(Char) - 0-127 ascii") {
    ASCIIFolding.fold('a') shouldBe "a"
    ASCIIFolding.fold('*') shouldBe "*"

    ascii.foreach { ch: Char => ASCIIFolding.fold(ch) shouldBe ch.toString }
  }

  test("fold - accents") {
    ASCIIFolding.fold('\u204E') shouldBe "*"
    ASCIIFolding.fold('\u2052') shouldBe "%"
    ASCIIFolding.fold('Æ') shouldBe "AE"
  }

  test("get - 0-127 ASCII") {
    ascii.foreach { ch: Char => ASCIIFolding.get(ch) shouldBe None }
  }

  test("get - accents") {
    ASCIIFolding.get('\u204E') shouldBe Some("*")
    ASCIIFolding.get('\u2052') shouldBe Some("%")
    ASCIIFolding.get('Æ') shouldBe Some("AE")
  }

  test("getOrNull - 0-127 ASCII") {
    ascii.foreach { ch: Char => ASCIIFolding.getOrNull(ch) shouldBe null }
  }

  test("getOrNull - accents") {
    ASCIIFolding.getOrNull('\u204E') shouldBe "*"
    ASCIIFolding.getOrNull('\u2052') shouldBe "%"
    ASCIIFolding.getOrNull('Æ') shouldBe "AE"
  }

  test("fold - ascii") {
    ASCIIFolding.fold(ascii) shouldBe theSameInstanceAs(ascii)
    ASCIIFolding.fold("foobar") shouldBe theSameInstanceAs("foobar")
    ASCIIFolding.fold("!@#$%^&*()_+") shouldBe theSameInstanceAs("!@#$%^&*()_+")
  }

  test("fold - non-ascii") {
    ASCIIFolding.fold("\u204E") shouldBe "*"
    ASCIIFolding.fold("\u204E" + ascii) shouldBe "*" + ascii
    ASCIIFolding.fold("\u2052") shouldBe "%"
    ASCIIFolding.fold("Æ") shouldBe "AE"
    ASCIIFolding.fold(ascii + "Æ") shouldBe ascii + "AE"
    ASCIIFolding.fold("Æ" + ascii) shouldBe "AE" + ascii
    ASCIIFolding.fold(ascii + "Foo Bar \u204E \u2052 Æ") shouldBe ascii + "Foo Bar * % AE"
  }
}
