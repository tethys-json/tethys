/*
  This is copy of jsoniter-scala WriterConfig
  https://github.com/plokhotnyuk/jsoniter-scala/blob/master/jsoniter-scala-core/shared/src/main/scala/com/github/plokhotnyuk/jsoniter_scala/core/WriterConfig.scala
  MIT License

  Copyright (c) 2017 Andriy Plokhotnyuk, and respective contributors

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
 */
package tethys.writers.tokens

/** Configuration for [[tethys.writers.tokens.DefaultTokenWriter]] that contains
  * params for formatting of output JSON and for tuning of preferred size for
  * internal byte buffer that is created on the writer instantiation and reused
  * in runtime for serialization of messages using any type of output except
  * pre-allocated byte arrays or heap byte buffers supplied as arguments. <br/>
  * All configuration params already initialized to default values, but in some
  * cases they should be altered: <ul> <li>turn on pretty printing by specifying
  * of indention step that is greater than 0</li> <li>turn on escaping of
  * Unicode characters to serialize with only ASCII characters</li> <li>increase
  * preferred size of an internal byte buffer to reduce allocation rate of grown
  * and then reduced internal buffers when serialized output size is greater
  * than 32Kb</li> </ul>
  * @param indentionStep
  *   a size of indention for pretty-printed formatting or 0 for compact output
  * @param escapeUnicode
  *   a flag to turn on hexadecimal escaping of all non-ASCII chars
  * @param preferredBufSize
  *   a preferred size (in bytes) of an internal byte buffer when writing to any
  *   type of output except pre-allocated byte arrays or heap byte buffers
  *   supplied as arguments
  */
case class TokenWriterConfig private[tethys] (
    private[tethys] val indentionStep: Int,
    private[tethys] val preferredBufSize: Int,
    private[tethys] val escapeUnicode: Boolean
) extends Serializable {

  /** Pretty prints output with 2 spaces indentation. Implementation is backend
    * specific and result may vary
    */
  def withDefaultPrettyPrinter: TokenWriterConfig =
    copy(indentionStep = 2)

  /** Escapes all non-ASCII characters. Implementation is backend specific and
    * result may vary.
    */
  def withEscapeUnicode(escapeUnicode: Boolean): TokenWriterConfig =
    copy(escapeUnicode = escapeUnicode)

  /** Sets preferred buffer size. Not all backends support this setting
    */
  def withBufferSize(size: Int): TokenWriterConfig = {
    if (size <= 0)
      throw new IllegalArgumentException(
        "'preferredBufSize' should be not less than 1"
      )
    copy(preferredBufSize = size)
  }
}

object TokenWriterConfig {
  implicit val default: TokenWriterConfig = new TokenWriterConfig(
    indentionStep = 0,
    preferredBufSize = 32768,
    escapeUnicode = false
  )
}
