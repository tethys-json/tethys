package tethys.writers.tokens

class TokenWriterException(
    msg: String,
    cause: Throwable
) extends RuntimeException(msg, cause, true, true)
