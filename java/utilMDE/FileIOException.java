package utilMDE;

import java.io.*;

/**
 * This class extends IOException by also reporting a file name and line
 * number at which the exception occurred.
 **/
public class FileIOException extends IOException {

  public final LineNumberReader reader;
  public final String fileName;

  public FileIOException() {
    super();
    reader = null;
    fileName = null;
  }

  public FileIOException(String s) {
    this(s, null, (String)null);
  }

  public FileIOException(String s, LineNumberReader reader, String fileName) {
    super(s);
    this.reader = reader;
    this.fileName = fileName;
  }

  public FileIOException(String s, LineNumberReader reader, File file) {
    this(s, reader, file.getName());
  }

  public String toString() {
    if (reader == null || fileName == null) {
      return super.toString();
    } else {
      return "\nError: " + super.toString() + " on line " + reader.getLineNumber() +
        " of file " + fileName;
    }
  }

}
