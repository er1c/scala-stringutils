package org.apache.commons

package object lang3 {
  /** Used to swallow unused warnings. */
  @inline private[lang3] def void(as: Any*): Unit = (as, ())._2
}
