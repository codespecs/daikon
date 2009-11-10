package daikon.tools;
import java.io.*;
import java.util.*;
import utilMDE.*;
import daikon.*;
import daikon.config.Configuration;
import java.util.regex.*;
import java.util.zip.*;
import gnu.getopt.*;

import static daikon.PptTopLevel.PptFlags;
import static daikon.VarInfo.VarFlags;
import static daikon.VarInfo.LangFlags;


/**
 * This tool converts between the textual and binary Daikon file formats.
 * Its input is any number of files in either format.  By default it
 * converts each file to binary format.  If the --textoutput argument is
 * supplied, it converts each file to textual format.
 */
public class DtraceConvert {

  @Option("-d produce txt output to the terminal")
  public static boolean dbg = false;

  @Option("-b convert the given dtrace to a binary file")
  public static boolean binary = false;

  @Option("-t convert the given dtrace to a txt file")
  public static boolean text = false;

  @Option("-c produce a compressed output file")
  public static boolean compressed = false;

  private static boolean bin2txt = false;
  private static boolean txt2bin = false;

  public static void main (String[] args) {
    try {
      mainHelper(args);
    } catch (daikon.Daikon.TerminationMessage e) {
      System.err.println(e.getMessage());
      System.exit(1);
    }
    // Any exception other than daikon.Daikon.TerminationMessage gets
    // propagated.  This simplifies debugging by showing the stack trace.
  }

  /**
   * This does the work of main, but it never calls System.exit, so it
   * is appropriate to be called progrmmatically.
   * Termination of the program with a message to the user is indicated by
   * throwing daikon.Daikon.TerminationMessage.
   * @see #main(String[])
   * @see daikon.Daikon.TerminationMessage
   **/
  public static void mainHelper( final String[] args ) {
    DtraceConvert myInstance = new DtraceConvert( );
    Options options = new Options( "DtraceConvert [options] infiles...",
                                  DtraceConvert.class );
    String [] file_args = options.parse_or_usage (args);

    if ( file_args.length == 0 ) {
      throw new daikon.Daikon.TerminationMessage( "No trace files specified." );
    }
    for ( String filename : file_args ) {
      // ...
    }
  }

}
