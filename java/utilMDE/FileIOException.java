package utilMDE;

import java.io.*;

/**
 * This class extends IOException by also reporting a file name and line
 * number at which the exception occurred.
 **/
public class FileIOException extends IOException {

  public final String fileName;
  public final int lineNumber;

  public FileIOException() {
    super();
    fileName = null;
    lineNumber = -1;
  }

  public FileIOException(String s) {
    this(s, null, (String)null);
  }

  public FileIOException(String s, LineNumberReader reader, String fileName) {
    super(s);
    this.fileName = fileName;
    if (reader != null) {
      this.lineNumber = reader.getLineNumber();
    } else {
      this.lineNumber = -1;
    }
  }

  public FileIOException(String s, LineNumberReader reader, File file) {
    this(s, reader, file.getName());
  }

  public String toString() {
    if (fileName == null || lineNumber == -1) {
      return super.toString();
    } else {
      return "Error: " + super.toString()
        + " on line " + lineNumber
        + " of file " + fileName;
    }
  }

}
