package daikon.tools.compare;

import java.util.*;
import java.io.*;
import org.apache.log4j.Logger;
import utilMDE.Assert;
import utilMDE.UtilMDE;
import daikon.*;
import daikon.inv.*;
import daikon.inv.unary.scalar.*;
import daikon.inv.unary.sequence.*;
import daikon.inv.unary.string.*;
import daikon.simplify.*;
import daikon.inv.Invariant.OutputFormat;

public class LogicalCompare {
  public static final Logger debug
    = Logger.getLogger("daikon.compare.LogicalCompare");

  // Stopgap waiting for real options reading support
  private static int flags;

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
  private static Vector filterSimplifyFormat(Vector/*<Invariant>*/ invs) {
    Vector/*<Invariant>*/ new_invs = new Vector/*<Invariant>*/();
    for (int i = 0; i < invs.size(); i++) {
      Invariant inv = (Invariant)invs.elementAt(i);
      String simp = inv.format_using(OutputFormat.SIMPLIFY);
      if (simp.indexOf("format_simplify") == -1 &&
          simp.indexOf("OutputFormat:Simplify") == -1)
        new_invs.add(inv);
      else
        System.out.println("Can't handle " + inv.format() + ": " + simp);
    }
    return new_invs;
  }

  private static Vector filterTrueHeuristics(Vector/*<Invariant>*/ invs) {
    Vector/*<Invariant>*/ new_invs = new Vector/*<Invariant>*/();
    for (int i = 0; i < invs.size(); i++) {
      Invariant inv = (Invariant)invs.elementAt(i);
      //      if (true)
      //      if (!inv.isObvious())
      //      if (inv.justified() && !inv.isObvious())
      //      if (inv.isObvious())
      if (!(inv.hasUninterestingConstant() && !(inv instanceof OneOf)
            /*&&
            (inv instanceof LowerBound || inv instanceof UpperBound ||
            inv instanceof EltLowerBound || inv instanceof EltUpperBound)*/)) {
        new_invs.add(inv);
      }
    }
    return new_invs;
  }

  private static boolean shouldDiscardInvariant( Invariant inv ) {
    for (int i = 0; i < inv.ppt.var_infos.length; i++) {
      VarInfo vi = inv.ppt.var_infos[i];
      // ppt has to be a PptSlice, not a PptTopLevel
      PrintInvariants.debugFiltering.debug("\tconsidering DPF for " + vi.name.name() + "\n");
      if (vi.isDerivedParamAndUninteresting()) {
        // Exception: let invariants like "orig(arg) == arg" through.
        if (IsEqualityComparison.it.accept( inv )) {
          Comparison comp = (Comparison)inv;
          VarInfo var1 = comp.var1();
          VarInfo var2 = comp.var2();
          boolean vars_are_same = var1.name.applyPrestate().equals(var2.name) || var2.name.applyPrestate().equals(var1.name);
          PrintInvariants.debugFiltering.debug("\t\tvars are same? " + String.valueOf(vars_are_same) + "\n");
          //if (vars_are_same) return false;
        }
//         if (inv instanceof OneOf || inv instanceof OneOfString ||
//             inv instanceof OneOfString)
//           return false;
        //System.err.println("Because of " + vi.name.name() + ",");
        return true;
      }
    }
    return false;
  }

  private static Vector filterRemoveImplementation(Vector invs) {
    Vector/*<Invariant>*/ new_invs = new Vector/*<Invariant>*/();
    for (int i = 0; i < invs.size(); i++) {
      Invariant inv = (Invariant)invs.elementAt(i);
      if (!shouldDiscardInvariant(inv))
        new_invs.add(inv);
    }
    return new_invs;
  }

  private static Vector filterRemoveImplications(Vector/*<Invariant>*/ invs) {
    Vector/*<Invariant>*/ new_invs = new Vector/*<Invariant>*/();
    for (int i = 0; i < invs.size(); i++) {
      Invariant inv = (Invariant)invs.elementAt(i);
      if (!(inv instanceof Implication))
        new_invs.add(inv);
    }
    return new_invs;
  }

  private static Vector translateStraight(Vector/*<Invariant>*/ invs) {
    Vector/*<Lemma>*/ lemmas = new Vector();
    for (int i = 0; i < invs.size(); i++) {
      Invariant inv = (Invariant)invs.elementAt(i);
      lemmas.add(new InvariantLemma(inv));
    }
    return lemmas;
  }

  private static Vector translateRemovePre(Vector/*<Invariant>*/ invs) {
    Vector/*<Lemma>*/ lemmas = new Vector();
    for (int i = 0; i < invs.size(); i++) {
      Invariant inv = (Invariant)invs.elementAt(i);
      if (!inv.isAllPrestate())
        lemmas.add(new InvariantLemma(inv));
    }
    return lemmas;
  }

  private static Vector translateAddOrig(Vector/*<Invariant>*/ invs) {
    Vector/*<Lemma>*/ lemmas = new Vector();
    for (int i = 0; i < invs.size(); i++) {
      Invariant inv = (Invariant)invs.elementAt(i);
      lemmas.add(InvariantLemma.makeLemmaAddOrig(inv));
    }
    return lemmas;
  }

  private static LemmaStack lemmas;

  private static void printInvariants(Vector/*<Invariant>*/ invs) {
    for (int i = 0; i < invs.size(); i++) {
      Invariant inv = (Invariant)invs.elementAt(i);
      System.out.println(inv.format());
    }

    for (int i = 0; i < invs.size(); i++) {
      Invariant inv = (Invariant)invs.elementAt(i);
      System.out.println("(BG_PUSH " +
                         inv.format_using(OutputFormat.SIMPLIFY) +")");
    }
  }

  private static int evaluateImplications(Vector assumptions,
                                          Vector consequences)
  throws SimplifyError {
    int invalidCount = 0;
    int mark = lemmas.markLevel();
    lemmas.pushLemmas(assumptions);
    if (lemmas.checkForContradiction() == 'T') {
      System.out.println("Contradictory assumptions:");
      Vector min = lemmas.minimizeContradiction();
      LemmaStack.printLemmas(System.out, min);
      Assert.assertTrue(false, "Aborting");
    }

    if ((flags & 8) != 0) {
      lemmas.dumpLemmas(System.out);
    }

    for (int i = 0; i < consequences.size(); i++) {
      Lemma inv = (Lemma)consequences.elementAt(i);
      char result = lemmas.checkLemma(inv);

      if (result == 'T') {
        if ((flags & 1) != 0) {
          Lemma[] ass_ary = (Lemma[])assumptions.toArray(new Lemma[1]);
          Vector/*<Lemma>*/ assume = lemmas.minimizeProof(inv);
          System.out.println();
          for (int j = 0; j < assume.size(); j++)
            System.out.println(((Lemma)assume.elementAt(j)).summarize());
          System.out.println("----------------------------------");
          System.out.println(inv.summarize());
          if ((flags & 2) != 0) {
            System.out.println();
            for (int j = 0; j < assume.size(); j++)
              System.out.println("    "  + ((Lemma)assume.elementAt(j))
                                 .formula);
            System.out.println("    ----------------------"
                               + "--------------------");
            System.out.println("    " + inv.formula);
          }
        } else if ((flags & 16) != 0) {
          System.out.println();
          System.out.print("Valid: ");
          System.out.println(inv.summary);
          if ((flags & 2) != 0)
            System.out.println("    " + inv.formula);
        }
      } else if (result == 'F') {
        invalidCount++;
        System.out.println();
        System.out.print("Invalid: ");
        System.out.println(inv.summary);
        if ((flags & 2) != 0)
          System.out.println("    " + inv.formula);
      } else {
        Assert.assertTrue(result == '?');
        System.out.println();
        System.out.print("Timeout: ");
        System.out.println(inv.summary);
        if ((flags & 2) != 0)
          System.out.println("    " + inv.formula);
      }
    }
    lemmas.popToMark(mark);
    return invalidCount;
  }

  private static Vector invariants_vector(PptTopLevel ppt) {
    return new Vector(ppt.getInvariants());
  }

  public static void main(String[] args)
    throws FileNotFoundException, IOException, ClassNotFoundException,
           SimplifyError
  {
    daikon.LogHelper.setupLogs(daikon.LogHelper.INFO);
    // LogHelper.setPriority("daikon.simplify", LogHelper.DEBUG);
    daikon.inv.Invariant.dkconfig_simplify_define_predicates = true;

    String app_filename = args[0];
    String test_filename = args[1];
    String enter_ppt_name = args[2];
    String exit_ppt_name = args[3];
    flags = Integer.parseInt(args[4]);

    if ((flags & 64) == 0)
      Session.dkconfig_simplify_max_iterations = 2147483647;

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
    Assert.assertTrue(app_enter_ppt != null);
    Assert.assertTrue(test_enter_ppt != null);

    PptTopLevel app_exit_ppt = app_ppts.get(exit_ppt_name);
    PptTopLevel test_exit_ppt = test_ppts.get(exit_ppt_name);
    Assert.assertTrue(app_exit_ppt != null);
    Assert.assertTrue(test_exit_ppt != null);

    Vector a_pre = invariants_vector(app_enter_ppt);
    Vector t_pre = invariants_vector(test_enter_ppt);
    Vector a_post = invariants_vector(app_exit_ppt);
    Vector t_post = invariants_vector(test_exit_ppt);

    a_pre = filterRemoveImplications(a_pre);
    t_pre = filterRemoveImplications(t_pre);

    a_post = filterRemoveImplementation(a_post);
    t_post = filterRemoveImplementation(t_post);

    a_pre = filterTrueHeuristics(a_pre);
    t_pre = filterTrueHeuristics(t_pre);
    a_post = filterTrueHeuristics(a_post);
    t_post = filterTrueHeuristics(t_post);

    a_pre = filterSimplifyFormat(a_pre);
    t_pre = filterSimplifyFormat(t_pre);
    a_post = filterSimplifyFormat(a_post);
    t_post = filterSimplifyFormat(t_post);

    lemmas = new LemmaStack();

    if ((flags & 4) != 0) {
      System.out.println("Apre (real) is:");
      printInvariants(a_pre);
      System.out.println();
    }

    System.out.println("Tpre consists of " + t_pre.size() + " invariants.");
    Vector/*<Lemma>*/ pre_assumptions = new Vector();
    pre_assumptions.addAll(translateStraight(a_pre));
    Vector/*<Lemma>*/ pre_conclusions = new Vector();
    pre_conclusions.addAll(translateStraight(t_pre));
    Collections.sort(pre_conclusions);

    int bad_pre = evaluateImplications(pre_assumptions, pre_conclusions);
    if (bad_pre > 0 && (flags & 32) == 0) {
      System.out.println("Precondition failure, skipping postconditions");
      return;
    }

    System.out.println("==============================================================================");

    Vector/*<Lemma>*/ post_assumptions = new Vector();
    Vector/*<Lemma>*/ post_conclusions = new Vector();

    // post_assumptions.addAll(filterPostIntoPre(a_post));
    post_assumptions.addAll(Lemma.lemmasVector());
    post_assumptions.addAll(translateAddOrig(a_pre));

    if ((flags & 4) != 0) {
      System.out.println("Apre (translated) is:");
      LemmaStack.printLemmas(System.out, post_assumptions);
      System.out.println();
    }

    post_assumptions.addAll(translateStraight(t_post));

    post_conclusions.addAll(translateRemovePre(a_post));

    Collections.sort(post_conclusions);

    if ((flags & 4) != 0) {
      System.out.println("Apost is:");
      //printInvariants(a_post);
      LemmaStack.printLemmas(System.out, translateRemovePre(a_post));
      System.out.println();
    }

    //    Assert.assertTrue(false);

    if ((flags & 4) != 0) {
      System.out.println("Apre /\\ Tpost is");
      LemmaStack.printLemmas(System.out, post_assumptions);
    }

    System.out.println("Apost consists of " + post_conclusions.size()
                       + " invariants.");

    evaluateImplications(post_assumptions, post_conclusions);
  }
}
