package daikon.tools.compare;

import java.util.*;
import java.io.*;
import org.apache.log4j.Logger;
import utilMDE.Assert;
import utilMDE.UtilMDE;
import daikon.*;
import daikon.inv.*;
import daikon.simplify.*;
import daikon.inv.Invariant.OutputFormat;

public class LogicalCompare {
  public static final Logger debug
    = Logger.getLogger("daikon.compare.LogicalCompare");

  //  public static final String lineSep = Global.lineSep;
//   private static String usage =
//     UtilMDE.join(new String[] {
//       "Usage: java daikon.ExtractConsequent [OPTION]... FILE",
//       "  -h, --" + Daikon.help_SWITCH,
//       "      Display this usage message",
//       "  --" + Daikon.suppress_cont_SWITCH,
//       "      Suppress display of implied invariants (by controlling ppt).",
//       "  --" + Daikon.suppress_post_SWITCH,
//       "      Suppress display of obvious postconditions on prestate.",
//       "  --" + Daikon.suppress_redundant_SWITCH,
//       "      Suppress display of logically redundant invariants."}, lineSep);
  private static Vector filterSimplifyFormat(List/*<Invariant>*/ invs) {
    Vector/*<Invariant>*/ new_invs = new Vector/*<Invariant>*/();
    for (int i = 0; i < invs.size(); i++) {
      Invariant inv = (Invariant)invs.get(i);
      String simp = inv.format_using(OutputFormat.SIMPLIFY);
      if (simp.indexOf("format_simplify") == -1 &&
          simp.indexOf("OutputFormat:Simplify") == -1)
        new_invs.add(inv);
      else
        System.out.println("Can't handle " + inv.format() + ": " + simp);
    }
    return new_invs;
  }

  private static List filterPostIntoPre(List/*<Invariant>*/ invs) {
    List/*<Invariant>*/ new_invs = new Vector/*<Invariant>*/();
    for (int i = 0; i < invs.size(); i++) {
      Invariant inv = (Invariant)invs.get(i);
      if (inv.isAllPrestate())
        new_invs.add(inv);
    }
    return new_invs;
  }

  private static List translateAddOrig(List/*<Invariant>*/ invs) {
    List/*<Invariant>*/ new_invs = new Vector/*<Invariant>*/();
    for (int i = 0; i < invs.size(); i++) {
      Invariant inv = ((Invariant)invs.get(i));
      // XXX might want to save the old inv.ppt
      inv.ppt = PptSlice0.makeFakePrestate(inv.ppt);
      new_invs.add(inv);
    }
    return new_invs;
  }

  private static List filterPreOutOfPost(List/*<Invariant>*/ invs) {
    List/*<Invariant>*/ new_invs = new Vector/*<Invariant>*/();
    for (int i = 0; i < invs.size(); i++) {
      Invariant inv = (Invariant)invs.get(i);
      if (!inv.isAllPrestate())
        new_invs.add(inv);
    }
    return new_invs;
  }

  private static SessionManager simplifySession;

  private static void assume(Invariant inv) {
    try {
      simplifySession.request(new CmdAssume(inv.format_using(OutputFormat.SIMPLIFY)));
    } catch (TimeoutException e) {
      Assert.assertTrue(false);
    }
  }

  private static void unAssume() {
    try {
      simplifySession.request(CmdUndoAssume.single);
    } catch (TimeoutException e) {
      Assert.assertTrue(false);
    }
  }

  private static void assumeAll(List/*<Invariant>*/ invs) {
    for (int i = 0; i < invs.size(); i++) {
      assume((Invariant)invs.get(i));
    }
  }

  private static void unAssumeAll(List/*<Invariant>*/ invs) {
    for (int i = 0; i < invs.size(); i++) {
      unAssume();
    }
  }

  private static boolean check(Invariant inv) {
    CmdCheck cc = new CmdCheck(inv.format_using(OutputFormat.SIMPLIFY));
    //System.out.print("[");
    try {
      simplifySession.request(cc);
      //System.out.print("]");
      return cc.valid;
    } catch (TimeoutException e) {
      return false;
    }
  }

  private static String getCounterexample(Invariant inv) {
    CmdCheck cc = new CmdCheck(inv.format_using(OutputFormat.SIMPLIFY));
    //System.out.print("[");
    try {
      Assert.assertTrue(cc != null);
      simplifySession.request(cc);
      //System.out.print("]");
    } catch (TimeoutException e) {
      Assert.assertTrue(false, "Unexpected timeout on " + inv.format());
      return null;
    }
    if (cc.valid) {
      return null;
    } else {
      if (cc.counterexample != null)
        return cc.counterexample;
      else
        return "(counterexamples disabled)";
    }
  }

  private static boolean allExceptImply(Invariant[] invs, boolean[] excluded,
                                        int min, int max, Invariant conseq) {
    int assumed = 0;
    for (int i = 0; i < invs.length; i++) {
      if (!excluded[i] && (i < min || i > max)) {
        assume(invs[i]);
        assumed++;
      }
    }
    boolean valid = check(conseq);
    for (int i = 0; i < assumed; i++) {
      unAssume();
    }
    return valid;
  }

  private static boolean allTrue(boolean[] bools, int min, int max) {
    for (int i = min; i <= max; i++) {
      if (!bools[i])
        return false;
    }
    return true;
  }

  private static Vector minimizeAssumptions(Invariant[] invs,
                                            Invariant consequence) {
    boolean[] excluded = new boolean[invs.length];

    for (int size = invs.length / 2; size > 1; size /= 2) {
      for (int start = 0; start < invs.length; start += size) {
        int end = Math.min(start + size - 1, invs.length - 1);
        if (!allTrue(excluded, start, end) &&
            allExceptImply(invs, excluded, start, end, consequence)) {
          for (int i = start; i <= end; i++)
            excluded[i] = true;
        }
      }
    }

    boolean reduced;
    do {
      reduced = false;
      for (int i = 0; i < invs.length; i++) {
        if (!excluded[i]) {
          if (allExceptImply(invs, excluded, i, i, consequence)) {
            excluded[i] = true;
            reduced = true;
          }
        }
      }
    } while (reduced);
    Vector/*<Invariant>*/ new_invs = new Vector/*<Invariant>*/();
    for (int i = 0; i < invs.length; i++) {
      if (!excluded[i])
        new_invs.add(invs[i]);
    }
    return new_invs;
  }

  private static void printInvariants(List/*<Invariant>*/ invs) {
    for (int i = 0; i < invs.size(); i++) {
      Invariant inv = (Invariant)invs.get(i);
      //System.out.println(inv.format() + " ==> " + inv.format_using(OutputFormat.SIMPLIFY));
      System.out.println(inv.format());
    }

    for (int i = 0; i < invs.size(); i++) {
      Invariant inv = (Invariant)invs.get(i);
      System.out.println("(BG_PUSH "
                         + inv.format_using(OutputFormat.SIMPLIFY) +")");
    }
  }

  private static void evaluateImplications(List assumptions,
                                           List consequences) {
    for (int i = 0; i < consequences.size(); i++) {
      Invariant inv = (Invariant)consequences.get(i);
      assumeAll(assumptions);
      String counterexample = getCounterexample(inv);
      boolean valid = (counterexample == null);
      unAssumeAll(assumptions);
      if (valid) {
        Invariant[] ass_ary = (Invariant[])
          assumptions.toArray(new Invariant[1]);
        Vector/*<Invariant>*/ assume = minimizeAssumptions(ass_ary, inv);
        System.out.println();
        for (int j = 0; j < assume.size(); j++)
          System.out.println(((Invariant)assume.elementAt(j)).format());
        System.out.println("----------------------------------");
        System.out.println(inv.format());
        System.out.println();
        for (int j = 0; j < assume.size(); j++)
          System.out.println("    "  + ((Invariant)assume.elementAt(j))
                             .format_using(OutputFormat.SIMPLIFY));
        System.out.println("    ------------------------------------------");
        System.out.println("    " + inv.format_using(OutputFormat.SIMPLIFY));
      } else {
        System.out.println();
        System.out.print("Invalid: ");
        System.out.println(inv.format());
        System.out.println("    " + inv.format_using(OutputFormat.SIMPLIFY));
        System.out.print(counterexample);
      }
    }
  }

  public static void main(String[] args)
    throws FileNotFoundException, IOException, ClassNotFoundException
  {
    daikon.LogHelper.setupLogs(daikon.LogHelper.INFO);
    //LogHelper.setPriority("daikon.simplify", LogHelper.DEBUG);
    String app_filename = args[0];
    String test_filename = args[1];
    String enter_ppt_name = args[2];
    String exit_ppt_name = args[3];
    System.out.println("Comparing " + enter_ppt_name + " and " + exit_ppt_name
                       + " in " + app_filename + " and " + test_filename);
    PptMap app_ppts = FileIO.read_serialized_pptmap(new File(app_filename),
                                                    true // use saved config
                                                    );
    PptMap test_ppts = FileIO.read_serialized_pptmap(new File(test_filename),
                                                     true // use saved config
                                                     );
    PptTopLevel app_enter_ppt = app_ppts.get(enter_ppt_name);
    PptTopLevel test_enter_ppt = test_ppts.get(enter_ppt_name);
    PptTopLevel app_exit_ppt = app_ppts.get(exit_ppt_name);
    PptTopLevel test_exit_ppt = test_ppts.get(exit_ppt_name);
    Assert.assertTrue(app_enter_ppt != null);
    Assert.assertTrue(test_enter_ppt != null);
    Assert.assertTrue(app_exit_ppt != null);
    Assert.assertTrue(test_exit_ppt != null);

    List/*<Invariant>*/ a_pre = app_enter_ppt.getInvariants();
    List/*<Invariant>*/ t_pre = test_enter_ppt.getInvariants();
    List/*<Invariant>*/ a_post = app_exit_ppt.getInvariants();
    List/*<Invariant>*/ t_post = test_exit_ppt.getInvariants();

    a_pre = filterSimplifyFormat(a_pre);
    t_pre = filterSimplifyFormat(t_pre);
    a_post = filterSimplifyFormat(a_post);
    t_post = filterSimplifyFormat(t_post);

    simplifySession = SessionManager.attemptProverStartup();
    simplifySession.setTimeout(1000*60); // one minute
    Assert.assertTrue(simplifySession != null);

    System.out.println("Apre (real) is:");
    printInvariants(a_pre);
    System.out.println();

    System.out.println("Tpre consists of " + t_pre.size() + " invariants.");
    evaluateImplications(a_pre, t_pre);
    System.out.println("==============================================================================");

    Vector/*<Invariant>*/ post_assumptions = new Vector/*<Invariant>*/();

    //post_assumptions.addAll(filterPostIntoPre(a_post));
    post_assumptions.addAll(translateAddOrig(a_pre));

    System.out.println("Apre (translated) is:");
    printInvariants(post_assumptions);
    System.out.println();

    post_assumptions.addAll(t_post);

    a_post = filterPreOutOfPost(a_post);

    System.out.println("Apost is:");
    printInvariants(a_post);
    System.out.println();

    //    Assert.assertTrue(false);

    System.out.println("Apre /\\ Tpost is");
    printInvariants(post_assumptions);

    System.out.println("Apost consists of " + a_post.size() + " invariants.");

    evaluateImplications(post_assumptions, a_post);

//     System.out.println("Apre consists of " + a_pre.size() + " invariants:");
//     }
  }
}
