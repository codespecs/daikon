package utilMDE;

import java.io.*;

// TODO:  A better name would be LineNumberException.
// And then it needn't really extend IOException.

// TODO:  Maybe the constructors should take a Reader and check at run time
// whether it's a LineNumberReader.  Easier for clients, but easier to
// forget to provide the right type of Reader, too.


/**
 * This class extends IOException by also reporting a file name and line
 * number at which the exception occurred.  It requires use of a
 * {@link LineNumberReader}.
 **/
public class FileIOException extends IOException {
  static final long serialVersionUID = 20050923L;

  public final String fileName;
  public final int lineNumber;

  ///
  /// Empty constructor
  ///

  public FileIOException() {
    super();
    fileName = null;
    lineNumber = -1;
  }

  ///////////////////////////////////////////////////////////////////////////
  /// Without a message (with a Throwable instead)
  ///

  public FileIOException(Throwable cause) {
    super(cause.getMessage());
    initCause(cause);
    fileName = null;
    lineNumber = -1;
  }

  ///////////////////////////////////////////////////////////////////////////
  /// Without a Reader
  ///

  public FileIOException(String s) {
    this(s, null, /*filename=*/ (String)null);
  }

  public FileIOException(String s, Throwable cause) {
    this(s, null, /*filename=*/ (String)null, cause);
  }

  // Design choice:  require filename and linenumber, don't support
  // interface with just one or the other.

  public FileIOException(String s, String fileName, int lineNumber) {
    super(s);
    this.fileName = fileName;
    this.lineNumber = lineNumber;
  }

  public FileIOException(String s, Throwable cause, String fileName, int lineNumber) {
    super(s, cause);
    this.fileName = fileName;
    this.lineNumber = lineNumber;
  }

  ///////////////////////////////////////////////////////////////////////////
  /// Without a filename or File
  ///

  // I cannot infer the filename from the reader, because LineNumberReader
  // gives no access to the underlying stream.

  public FileIOException(LineNumberReader reader, Throwable cause) {
    this(reader, /*fileName=*/ (String)null, cause);
  }

  public FileIOException(String s, LineNumberReader reader) {
    this(s, reader, /*fileName=*/ (String)null);
  }

  public FileIOException(String s, LineNumberReader reader, Throwable cause) {
    this(s, reader, /*fileName=*/ (String)null, cause);
  }

  ///////////////////////////////////////////////////////////////////////////
  /// With a filename
  ///

  public FileIOException(String s, LineNumberReader reader, String fileName) {
    this(s, reader, fileName, /*cause=*/ null);
  }

  public FileIOException(LineNumberReader reader, String fileName, Throwable cause) {
    this(cause.getMessage(), reader, fileName, cause);
  }

  public FileIOException(String s, LineNumberReader reader, String fileName, Throwable cause) {
    super(s);
    if (cause != null) initCause(cause);
    this.fileName = fileName;
    this.lineNumber = getLineNumber(reader);
  }

  ///////////////////////////////////////////////////////////////////////////
  /// With a File
  ///

  public FileIOException(String s, LineNumberReader reader, File file) {
    this(s, reader, file.getName());
  }

  public FileIOException(String s, LineNumberReader reader, File file, Throwable cause) {
    this(s, reader, file.getName(), cause);
  }

  public FileIOException(LineNumberReader reader, File file, Throwable cause) {
    this(cause.getMessage(), reader, file.getName(), cause);
  }


  ///////////////////////////////////////////////////////////////////////////
  /// Utility and helper methods
  ///

  public String toString() {
    String result = super.toString();
    if (fileName != null) {
      result += " in file " + fileName;
    }
    if (lineNumber != -1) {
      result += " at line " + lineNumber;
    }
    return result;
  }

  // Infers the line number from the "reader" field.
  // Assumes the "reader" field is already set.
  // Not a setter method because field lineNumber is final, but
  // still clearer to abstract out.
  private int getLineNumber(LineNumberReader reader) {
    if (reader != null) {
      return reader.getLineNumber();
    } else {
      return -1;
    }
  }

}
