package daikon;

import java.io.*;
import gnu.getopt.*;

public class PrintInvariants {

  public static final String lineSep = Global.lineSep;

  private static String usage =
    "Usage: java daikon.PrintInvariants [OPTION]... FILE" +
    lineSep +
    "  -h  Display this usage message" +
    lineSep +
    "  --" + Daikon.suppress_cont_SWITCH +
    lineSep +
    "      Suppress display of implied invariants (by controlling ppt)." +
    lineSep +
    "  --" + Daikon.suppress_post_SWITCH +
    lineSep +
    "      Suppress display of obvious postconditions on prestate." +
    lineSep +
    "  --" + Daikon.esc_output_SWITCH +
    lineSep +
    "      Write output in ESC-like format." +
    lineSep +
    "  --" + Daikon.simplify_output_SWITCH +
    lineSep +
    "      Write output in Simplify format." +
    lineSep +
    "  --" + Daikon.output_num_samples_SWITCH +
    lineSep +
    "      Output numbers of values and samples for invariants and " +
    "program points; for debugging." +
    lineSep;

  public static void main(String[] args) throws FileNotFoundException,
  StreamCorruptedException, OptionalDataException, IOException,
  ClassNotFoundException {
    LongOpt[] longopts = new LongOpt[] {
      new LongOpt(Daikon.suppress_cont_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(Daikon.suppress_post_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(Daikon.esc_output_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(Daikon.simplify_output_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(Daikon.output_num_samples_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
    };
    Getopt g = new Getopt("daikon.PrintInvariants", args, "h", longopts);
    int c;
    while ((c = g.getopt()) != -1) {
      switch(c) {
      case 0:
        // got a long option
        String option_name = longopts[g.getLongind()].getName();
	if (Daikon.suppress_cont_SWITCH.equals(option_name)) {
	  Daikon.suppress_implied_controlled_invariants = true;
	} else if (Daikon.suppress_post_SWITCH.equals(option_name)) {
	  Daikon.suppress_implied_postcondition_over_prestate_invariants =
            true;
	} else if (Daikon.esc_output_SWITCH.equals(option_name)) {
	  Daikon.output_style = Daikon.OUTPUT_STYLE_ESC;
	} else if (Daikon.simplify_output_SWITCH.equals(option_name)) {
	  Daikon.output_style = Daikon.OUTPUT_STYLE_SIMPLIFY;
	} else if (Daikon.output_num_samples_SWITCH.equals(option_name)) {
	  Daikon.output_num_samples = true;
	} else {
	  throw new RuntimeException("Unknown long option received: " +
                                     option_name);
	}
        break;
      case 'h':
        System.out.println(usage);
        System.exit(1);
        break;
      case '?':
        break; // getopt() already printed an error
      default:
        System.out.println("getopt() returned " + c);
        break;
      }
    }
    // The index of the first non-option argument -- the name of the file
    int fileIndex = g.getOptind();
    if (args.length - fileIndex != 1) {
        System.out.println(usage);
        System.exit(1);      
    }
    String filename = args[fileIndex];
    PptMap ppts = FileIO.read_invariant_file(filename);
    Daikon.print_invariants(ppts);
  }

}
