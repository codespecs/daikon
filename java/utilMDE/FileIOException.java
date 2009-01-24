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

  public final /*@Nullable*/ String fileName;
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

  // If cause is null, the super call throws a null pointer exception.
  // This looks like a JDK bug.  -MDE 12/9/2008
  public FileIOException(/*@Nullable*/ Throwable cause) {
    // The "super(Throwable)" constructor exists in Java 6 and later.
    // For backward compatibility, use the initCause method instead.
    initCause(cause);
    fileName = null;
    lineNumber = -1;
  }

  ///////////////////////////////////////////////////////////////////////////
  /// Without a Reader
  ///

  public FileIOException(/*@Nullable*/ String s) {
    super(s);
    fileName = null;
    lineNumber = -1;
  }

  public FileIOException(/*@Nullable*/ String s, /*@Nullable*/ Throwable cause) {
    // The "super(String, Throwable) constructor exists in Java 6 and later.
    // For backward compatibility, use the initCause method instead.
    super(s);
    initCause(cause);
    fileName = null;
    lineNumber = -1;
  }

  // Design choice:  require filename and linenumber, don't support
  // interface with just one or the other.

  public FileIOException(/*@Nullable*/ String s, /*@Nullable*/ String fileName, int lineNumber) {
    super(s);
    this.fileName = fileName;
    this.lineNumber = lineNumber;
  }

  public FileIOException(/*@Nullable*/ String s, /*@Nullable*/ Throwable cause, /*@Nullable*/ String fileName, int lineNumber) {
    // The "super(String, Throwable) constructor exists in Java 6 and later.
    // For backward compatibility, use the initCause method instead.
    super(s);
    initCause(cause);
    this.fileName = fileName;
    this.lineNumber = lineNumber;
  }

  ///////////////////////////////////////////////////////////////////////////
  /// Without a filename or File
  ///

  // I cannot infer the filename from the reader, because LineNumberReader
  // gives no access to the underlying stream.

  public FileIOException(/*@Nullable*/ LineNumberReader reader, /*@Nullable*/ Throwable cause) {
    this(reader, /*fileName=*/ (/*@Nullable*/ String)null, cause);
  }

  public FileIOException(/*@Nullable*/ String s, /*@Nullable*/ LineNumberReader reader) {
    this(s, reader, /*fileName=*/ (/*@Nullable*/ String)null);
  }

  public FileIOException(/*@Nullable*/ String s, /*@Nullable*/ LineNumberReader reader, /*@Nullable*/ Throwable cause) {
    this(s, reader, /*fileName=*/ (/*@Nullable*/ String)null, cause);
  }

  ///////////////////////////////////////////////////////////////////////////
  /// With a filename
  ///

  public FileIOException(/*@Nullable*/ String s, /*@Nullable*/ LineNumberReader reader, /*@Nullable*/ String fileName) {
    super(s);
    this.fileName = fileName;
    this.lineNumber = getLineNumber(reader);
  }

  public FileIOException(/*@Nullable*/ LineNumberReader reader, /*@Nullable*/ String fileName, /*@Nullable*/ Throwable cause) {
    // The "super(Throwable) constructor exists in Java 6 and later.
    // For backward compatibility, use the initCause method instead.
    initCause(cause);
    this.fileName = fileName;
    this.lineNumber = getLineNumber(reader);
  }

  public FileIOException(/*@Nullable*/ String s, /*@Nullable*/ LineNumberReader reader, /*@Nullable*/ String fileName, /*@Nullable*/ Throwable cause) {
    // The "super(String, Throwable) constructor exists in Java 6 and later.
    // For backward compatibility, use the initCause method instead.
    super(s);
    initCause(cause);
    this.fileName = fileName;
    this.lineNumber = getLineNumber(reader);
  }

  ///////////////////////////////////////////////////////////////////////////
  /// With a File
  ///

  public FileIOException(/*@Nullable*/ String s, /*@Nullable*/ LineNumberReader reader, File file) {
    this(s, reader, file.getName());
  }

  public FileIOException(/*@Nullable*/ String s, /*@Nullable*/ LineNumberReader reader, File file, /*@Nullable*/ Throwable cause) {
    this(s, reader, file.getName(), cause);
  }

  public FileIOException(/*@Nullable*/ LineNumberReader reader, File file, /*@Nullable*/ Throwable cause) {
    // The "super(Throwable) constructor exists in Java 6 and later.
    // For backward compatibility, use the initCause method instead.
    initCause(cause);
    this.fileName = file.getName();
    this.lineNumber = getLineNumber(reader);
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
  private int getLineNumber(/*@Nullable*/ LineNumberReader reader) /*@Raw*/ {
    if (reader != null) {
      return reader.getLineNumber();
    } else {
      return -1;
    }
  }

}
