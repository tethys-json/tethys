package tethys.readers.tokens

class JsonReaderException private[tokens](msg: String, cause: Throwable, withStackTrace: Boolean)
  extends RuntimeException(msg, cause, true, withStackTrace)