package daikon;

import java.io.*;
import gnu.getopt.*;
import utilMDE.UtilMDE;

public class PrintInvariants {

  public static final String lineSep = Global.lineSep;

  private static String usage =
    UtilMDE.join(new String[] {
      "Usage: java daikon.PrintInvariants [OPTION]... FILE",
      "  -h, --" + Daikon.help_SWITCH,
      "      Display this usage message",
      "  --" + Daikon.suppress_cont_SWITCH,
      "      Suppress display of implied invariants (by controlling ppt).",
      "  --" + Daikon.suppress_post_SWITCH,
      "      Suppress display of obvious postconditions on prestate.",
      "  --" + Daikon.suppress_redundant_SWITCH,
      "      Suppress display of logically redundant invariants.", 
      "  --" + Daikon.esc_output_SWITCH,
      "      Write output in ESC-like format.",
      "  --" + Daikon.simplify_output_SWITCH,
      "      Write output in Simplify format.",
      "  --" + Daikon.output_num_samples_SWITCH,
      "      Output numbers of values and samples for invariants and " +
      "program points; for debugging."}, lineSep);

  public static void main(String[] args) throws FileNotFoundException,
  StreamCorruptedException, OptionalDataException, IOException,
  ClassNotFoundException {
    daikon.Logger.setupLogs (daikon.Logger.INFO);
    LongOpt[] longopts = new LongOpt[] {
      new LongOpt(Daikon.suppress_cont_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(Daikon.suppress_post_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(Daikon.suppress_redundant_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
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
        if (Daikon.help_SWITCH.equals(option_name)) {
          System.out.println(usage);
          System.exit(1);
        } else if (Daikon.suppress_cont_SWITCH.equals(option_name)) {
	  Daikon.suppress_implied_controlled_invariants = true;
	} else if (Daikon.suppress_post_SWITCH.equals(option_name)) {
	  Daikon.suppress_implied_postcondition_over_prestate_invariants = true;
	} else if (Daikon.suppress_redundant_SWITCH.equals(option_name)) {
	  Daikon.suppress_redundant_invariants_with_simplify = true;
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
    PptMap ppts = FileIO.read_serialized_pptmap(new File(filename),
					       true // use saved config
					       );
    Daikon.print_invariants(ppts);
  }

}
