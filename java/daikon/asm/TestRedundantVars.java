package daikon.asm;

import java.io.FileReader;
import java.io.IOException;
import java.io.LineNumberReader;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;


/**
 * Input: 3 text files associated with the same dtrace file:
 *
 *
 *   args[0] : a file that we'll call `without.txt', the resulting
 *   text of running `daikon.PrintInvariants --config_option
 *   daikon.PrintInvariants.print_all=true' on an invariant file
 *   created without redundant variable analysis.
 *
 *   args[1] : a file that we'll call `with.txt', the resulting text
 *   of running `daikon.PrintInvariants --config_option
 *   daikon.PrintInvariants.print_all=true' on an invriant file
 *   created with redundant variable analysis.
 *
 *   args[2] : a file that we'll call
 *   `reds.txt', the file with redundant var info produced when running
 *   daikon with redundant variable analysis.
 *
 * Output: checks the following:
 *
 * 1. If inv is in without.txt and does not mention an rvar, it is
 *    also in with.txt.
 *
 * 2. If inv is in without.txt and mentions an rvar, it is not in
 *    with.txt
 *
 * 3. If inv is in with.txt, it does not mention an rvar.
 *
 * 4. If inv is in with.txt, it is also in without.txt.
 */
public class TestRedundantVars {

    private static PptFile without;
    private static PptFile with;
    private static PptFile reds;

    public static void main(String[] args) {

        System.out.print("Reading invariant list file 1...");
        without = PptFile.getPptFile(args[0]);
        System.out.println(without.records.size() + " records.");
        System.out.print("Reading invariant list file 2...");
        with =    PptFile.getPptFile(args[1]);
        System.out.println(with.records.size() + " records.");
        System.out.print("Reading rvars list file...");
        reds =    PptFile.getPptFile(args[2]);
        System.out.println(reds.records.size() + " records.");


        boolean success = true;

        for (Map.Entry<String, List<String>> e : reds.records.entrySet()) {

            if (e.getValue().size() == 0) {
                // No redundant vars for this ppt.
                continue;
            }

            if (!process_ppt(e.getKey())) {
                success = false;
            }
        }

        if (!success)
            System.exit(1);
    }

    // Returns true iff all tests pass.
    private static boolean process_ppt(String ppt) {

        List<String> invsWithout = without.records.get(ppt);
        List<String> invsWith = with.records.get(ppt);

        if (invsWithout == null) {
            assert invsWith == null : ppt.toString();
            return true; // We don't consider missing ppts as failures.
        }

        assert invsWith != null : "@SuppressWarnings(nullness): same nullness as invsWithout, which was checked";

        List<String> redVars = reds.records.get(ppt);
        assert redVars != null : ppt;
        assert redVars != null : "@SuppressWarnings(nullness)";

        boolean success = true;

        for (String invWithout : invsWithout) {
            if (mentionsVar(invWithout, redVars)) {
                if (invsWith.contains(invWithout)) {
                    System.out.println(context("Invariant from daikon-w/o-anal mentions rvars, but also present in daikon-w-anal", ppt, invWithout, invsWith, invsWithout, redVars));
                    success = false;
                }
            } else {
                if (!( invsWith.contains(invWithout))) {
                    System.out.println(context("Invariant from daikon-w/o-anal does not mention rvars, but not present in daikon-w-anal", ppt, invWithout, invsWith, invsWithout, redVars));
                    success = false;
                }
            }
        }

        for (String invWith : invsWith) {
            if (mentionsVar(invWith, redVars)) {
                System.out.println(context("Invariant from daikon-w-anal mentiones rvars", ppt, invWith, invsWith, invsWithout, redVars));
                success = false;
            }
            if (!(invsWithout.contains(invWith))) {
                System.out.println(context("Invariant from daikon-w-anal not present in daikon-w/o-anal", ppt, invWith, invsWith, invsWithout, redVars));
                success = false;
            }
        }

        return success;
    }

    private static String context(String message, String ppt, String inv, List<String> invsWith, List<String> invsWithout,
                                  List<String> redVars) {

        StringBuilder b = new StringBuilder();
        b.append("MSG:" + message);
        b.append("PPT: " + ppt + "\n");
        b.append("INV: " + inv + "\n");
        b.append("\n\nINVS WITH:\n");
        for (String s :  invsWith) b.append(s + "\n");
        b.append("\n\nINVS WITHOUT:\n");
        for (String s :  invsWithout) b.append(s + "\n");
        b.append("\n\nRVARS:\n");
        for (String s : redVars) b.append(s + "\n");
        return b.toString();
    }

    private static boolean mentionsVar(String line, List<String> vars) {

      for (String var : vars) {
          String varName = var.substring(0, var.indexOf('('));
          if (line.contains(varName))
              return true;
      }
      return false;
    }

}
