package daikon.tools;
import java.io.*;
import java.util.*;
import utilMDE.*;
import daikon.*;
import daikon.config.Configuration;
import java.util.regex.*;
import gnu.getopt.*;

/**
 * This tool converts between the textual and binary Daikon file formats.
 * Its input is any number of files in either format.  By default it
 * converts each file to binary format.  If the --textoutput argument is
 * supplied, it converts each file to textual format.
 */
public class DtraceConvert {

  @Option("produce output in textual format")
  public static boolean textOutput = false;

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
  public static void mainHelper(final String[] args) {
    DtraceConvert myInstance = new DtraceConvert();
    Options options = new Options("DtraceConvert [options] infiles...",
                                  DtraceConvert.class);
    String[] file_args = options.parse_or_usage (args);

    if (file_args.length == 0) {
      throw new daikon.Daikon.TerminationMessage("No trace files specified.");
    }
    for (String filename : args) {
      dtraceConvert (filename);
    }
  }

  public static void dtraceConvert (String filename) {

    PptMap ppts = new PptMap();

    try {
      FileIO.ParseState state =
        new FileIO.ParseState (filename, false, true, ppts);

      while (state.status != FileIO.ParseStatus.EOF) {
        FileIO.read_data_trace_record_setstate (state);
        switch(state.status) {

        case SAMPLE:
          // This should eventually call
          // processor.process_sample (state.all_ppts, state.ppt, state.vt, state.nonce);
          // but for the time being, it would be enough to hard-code the logic here.
          System.out.println("Got a sample");
          break;

        case DECL:
          System.out.println("Got a decl");
          break;
        case DECL_VERSION:
          System.out.println("Got a decl_version");
          break;
        case COMPARABILITY:
          System.out.println("Got a comparability");
          break;
        case LIST_IMPLEMENTORS:
          System.out.println("Got a list_implementors");
          break;
        case INPUT_LANGUAGE:
          System.out.println("Got a input_language");
          break;

        case NULL:
          System.out.println("Got a null");
          break;
        case COMMENT:
          System.out.println("Got a comment");
          break;
        case EOF:
          System.out.println("Got a eof");
          break;
        case TRUNCATED:
          System.out.println("Got a truncated");
          break;
        case ERROR:
          System.out.println("Got a error");
          break;

        default:
          throw new Error();
        }
      }
    } catch (IOException e) {
      System.out.println();
      e.printStackTrace();
      throw new Error(e);
    }
  }

}
