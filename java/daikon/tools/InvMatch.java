package daikon.tools;

import daikon.*;
import daikon.inv.*;
import daikon.inv.binary.twoScalar.*;
import daikon.inv.unary.scalar.*;
import gnu.getopt.*;
import java.io.*;
import java.util.*;
import plume.*;

/*>>>
import org.checkerframework.checker.nullness.qual.*;
import org.checkerframework.dataflow.qual.*;
*/

/**
 * Functions that look for relationships between the invariants at different program points.
 * Relationships between individual invariants are created with InvTranslate. Currently, only
 * invariants of the same class can be related to one another.
 */
public class InvMatch {

  /**
   * Main program for testing purposes.
   *
   * @param args command-line arguments, ignored
   */
  @SuppressWarnings("nullness") // testing method, not worth type-checking
  public static void main(String[] args) throws IOException {

    // Read in the sample decls file
    Set<File> decl_files = new HashSet<File>(1);
    decl_files.add(new File("daikon/test/SampleTester.decls"));
    PptMap all_ppts = FileIO.read_declaration_files(decl_files);

    // Setup everything to run
    PptSliceEquality.dkconfig_set_per_var = true;
    Daikon.setup_NISuppression();

    // Get the exit ppts
    PptTopLevel ppt35 = all_ppts.get("foo.f():::EXIT35");
    PptTopLevel ppt40 = all_ppts.get("foo.g():::EXIT40");

    // Get variables at both program points
    VarInfo ppt35_x = ppt35.var_infos[0];
    VarInfo ppt35_y = ppt35.var_infos[1];
    VarInfo ppt35_z = ppt35.var_infos[2];
    VarInfo ppt40_p = ppt40.var_infos[0];
    VarInfo ppt40_q = ppt40.var_infos[1];
    VarInfo ppt40_r = ppt40.var_infos[2];

    // Build EXIT35 slices
    PptSlice slice_xy_35 = ppt35.get_or_instantiate_slice(new VarInfo[] {ppt35_x, ppt35_y});
    PptSlice slice_yz_35 = ppt35.get_or_instantiate_slice(new VarInfo[] {ppt35_y, ppt35_z});
    PptSlice slice_x_35 = ppt35.get_or_instantiate_slice(new VarInfo[] {ppt35_x});

    // Build EXIT40 slices
    PptSlice slice_pq_40 = ppt40.get_or_instantiate_slice(new VarInfo[] {ppt40_p, ppt40_q});
    PptSlice slice_qr_40 = ppt40.get_or_instantiate_slice(new VarInfo[] {ppt40_q, ppt40_r});
    PptSlice slice_pr_40 = ppt40.get_or_instantiate_slice(new VarInfo[] {ppt40_p, ppt40_r});
    PptSlice slice_p_40 = ppt40.get_or_instantiate_slice(new VarInfo[] {ppt40_p});

    // Create invariants at EXIT35
    Invariant inv_x_lt_y = IntLessThan.get_proto().instantiate(slice_xy_35);
    slice_xy_35.addInvariant(inv_x_lt_y);
    OneOfScalar inv_x_eq_5 = (OneOfScalar) OneOfScalar.get_proto().instantiate(slice_x_35);
    inv_x_eq_5.add_modified(5, 1);
    slice_x_35.addInvariant(inv_x_eq_5);
    Invariant inv_y_gt_z = IntGreaterThan.get_proto().instantiate(slice_yz_35);
    slice_yz_35.addInvariant(inv_y_gt_z);

    // Create invariants at EXIT40
    Invariant inv_p_lt_q = IntLessThan.get_proto().instantiate(slice_pq_40);
    slice_pq_40.addInvariant(inv_p_lt_q);
    OneOfScalar inv_p_eq_3 = (OneOfScalar) OneOfScalar.get_proto().instantiate(slice_p_40);
    inv_p_eq_3.add_modified(3, 1);
    slice_p_40.addInvariant(inv_p_eq_3);
    Invariant inv_q_gt_r = IntGreaterThan.get_proto().instantiate(slice_qr_40);
    slice_qr_40.addInvariant(inv_q_gt_r);
    Invariant inv_p_gt_r = IntGreaterThan.get_proto().instantiate(slice_pr_40);
    slice_pr_40.addInvariant(inv_p_gt_r);

    // InvTranslate xlate = new InvTranslate(inv_x_lt_y, inv_p_lt_q);

    // Try to matchup the program points
    List<List<InvTranslate>> valid_translations = match_ppt(ppt35, ppt40);

    // Dump all of the valid translations
    System.out.println("Valid Translations:");
    for (List<InvTranslate> current_translation : valid_translations) {
      System.out.println("  Translation: ");
      for (InvTranslate xlate : current_translation) {
        System.out.printf("    %s%n", xlate);
      }
    }

    List<InvTranslate> best_translation = best_translation(valid_translations);
    System.out.println(Global.lineSep + "Best Translation");
    for (InvTranslate xlate : best_translation) {
      System.out.printf("  %s%n", xlate);
    }
  }

  /**
   * Compares the invariants between the two program points specified and returns a list of possible
   * translations. A possible translation is a consistent set of translations (one for each
   * invariant in ppt1) A set of translations is consistent if all of the variable mappings are
   * consistent (i.e., no variable maps to more than one variable).
   */
  static List<List<InvTranslate>> match_ppt(PptTopLevel ppt1, PptTopLevel ppt2) {

    List<List<InvTranslate>> xlate_list = new ArrayList<List<InvTranslate>>();

    for (Iterator<Invariant> i = ppt1.invariants_iterator(); i.hasNext(); ) {
      Invariant inv1 = i.next();
      List<InvTranslate> inv_xlate_list = new ArrayList<InvTranslate>();
      xlate_list.add(inv_xlate_list);
      for (Iterator<Invariant> j = ppt2.invariants_iterator(); j.hasNext(); ) {
        Invariant inv2 = j.next();
        InvTranslate xlate = new InvTranslate(inv1, inv2);
        if (xlate.quality > 0) inv_xlate_list.add(xlate);
      }
      // What is the purpose of this?  Maybe it separates the translation
      // results for different invariants.  How/where is it used/checked?
      // I see places where a null is just skipped over, but not where it
      // has an effect.
      @SuppressWarnings(
          "nullness") // can't figure out what this is for; maybe change the declaration
      /*@NonNull*/ InvTranslate dummy = null;
      inv_xlate_list.add(dummy);
    }

    // Debug print all of the translations
    if (true) {
      Iterator<Invariant> invi = ppt1.invariants_iterator();
      for (List<InvTranslate> inv_xlate_list : xlate_list) {
        Invariant inv = invi.next();
        System.out.printf("%s translations:%n", inv.format());
        for (InvTranslate xlate : inv_xlate_list) {
          System.out.printf("  %s%n", xlate);
        }
      }
    }

    List<List<InvTranslate>> valid_translations = new ArrayList<List<InvTranslate>>();
    List<InvTranslate> current_translation = new ArrayList<InvTranslate>();
    consider_xlate(valid_translations, current_translation, xlate_list, 0);

    return valid_translations;
  }

  /**
   * Recursive routine that tries all possible combination of translations.
   *
   * @param valid_translations list of valid translations (updated)
   * @param current_translation the current translation that is being built
   * @param xlate_list the list of possible translations for each invariant
   * @param index the current index in xlate_list
   */
  public static void consider_xlate(
      List<List<InvTranslate>> valid_translations,
      List<InvTranslate> current_translation,
      List<List<InvTranslate>> xlate_list,
      int index) {

    List<InvTranslate> inv_xlate_list = xlate_list.get(index);
    for (InvTranslate xlate : inv_xlate_list) {

      List<InvTranslate> new_translation = new ArrayList<InvTranslate>();
      new_translation.addAll(current_translation);
      new_translation.add(xlate);
      if (!is_good_translation(new_translation)) {
        continue;
      }

      if ((index + 1) == xlate_list.size()) {
        valid_translations.add(new_translation);
      } else {
        consider_xlate(valid_translations, new_translation, xlate_list, index + 1);
      }
    }
  }
  /*@Pure*/
  public static boolean is_good_translation(List<InvTranslate> translation_list) {

    Map<String, String> var_map = new LinkedHashMap<String, String>();

    for (InvTranslate xlate : translation_list) {
      if (xlate == null) {
        continue;
      }
      for (String key : xlate.var_map.keySet()) {
        String val = xlate.var_map.get(key);
        String cur_val = var_map.get(key);
        if (cur_val == null) {
          var_map.put(key, val);
        } else if (!cur_val.equals(val)) {
          return false;
        }
      }
    }
    return true;
  }

  public static /*@Nullable*/ List<InvTranslate> best_translation(
      List<List<InvTranslate>> valid_translations) {

    // Determine the best translation and print it out.
    List<InvTranslate> best_translation = null;
    int best_quality = 0;
    for (List<InvTranslate> current_translation : valid_translations) {
      int quality = 0;
      for (InvTranslate xlate : current_translation) {
        if (xlate != null) quality += xlate.quality;
      }
      if (quality > best_quality) {
        best_translation = current_translation;
        best_quality = quality;
      }
    }
    return best_translation;
  }
}
