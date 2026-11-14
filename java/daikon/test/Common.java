package daikon.test;

import daikon.Daikon;
import daikon.FileIO;
import daikon.PptMap;
import daikon.PptTopLevel;
import daikon.ProglangType;
import daikon.VarComparabilityNone;
import daikon.VarInfo;
import daikon.VarInfoAux;
import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.regex.Pattern;
import org.checkerframework.checker.nullness.qual.Nullable;

/** A collection of useful helper methods that are common to many different individual tests. */
public class Common {
  private Common() {
    throw new UnsupportedOperationException("do not instantiate");
  }

  @SuppressWarnings("interning")
  public static VarInfo newIntVarInfo(String name) {
    return new VarInfo(
        name, ProglangType.INT, ProglangType.INT, VarComparabilityNone.it, VarInfoAux.getDefault());
  }

  @SuppressWarnings("interning")
  public static VarInfo makeHashcodeVarInfo(String name) {
    return new VarInfo(
        name,
        ProglangType.HASHCODE,
        ProglangType.HASHCODE,
        VarComparabilityNone.it,
        VarInfoAux.getDefault());
  }

  @SuppressWarnings("interning")
  public static VarInfo makeIntArrayVarInfo(String name) {
    return new VarInfo(
        name,
        ProglangType.INT_ARRAY,
        ProglangType.INT_ARRAY,
        VarComparabilityNone.it,
        VarInfoAux.getDefault());
  }

  @SuppressWarnings("interning")
  public static VarInfo makeHashcodeArrayVarInfo(String name) {
    return new VarInfo(
        name,
        ProglangType.HASHCODE_ARRAY,
        ProglangType.HASHCODE_ARRAY,
        VarComparabilityNone.it,
        VarInfoAux.getDefault());
  }

  public static PptTopLevel makePptTopLevel(String pptname, VarInfo[] vars) {

    // If any of the variables have enclosing variables, include those in
    // the ppt as well.
    List<VarInfo> vlist = new ArrayList<>();
    for (VarInfo vi : vars) {
      if (vi.enclosing_var != null) {
        vlist.add(vi.enclosing_var);
      }
    }
    if (!vlist.isEmpty()) {
      VarInfo[] full = new VarInfo[vars.length + vlist.size()];
      int index = 0;
      for (VarInfo vi : vars) {
        full[index++] = vi;
      }
      for (VarInfo vi : vlist) {
        full[index++] = vi;
      }
      vars = full;
    }

    PptTopLevel ppt = new PptTopLevel(pptname, vars);
    return ppt;
  }

  /**
   * Writes the given lines to the given decls file, and returns the program points that Daikon
   * reads from it. The program points are not filtered, even if another test has set the options
   * that filter them.
   *
   * @param declsFile a file, which this method overwrites
   * @param lines the lines of the decls file, without line terminators
   * @return the program points read from the decls file
   * @throws IOException if there is trouble writing or reading the decls file
   */
  public static PptMap readDecls(File declsFile, List<String> lines) throws IOException {
    Files.write(declsFile.toPath(), lines, StandardCharsets.UTF_8);
    @Nullable Pattern oldPptRegexp = Daikon.ppt_regexp;
    @Nullable Pattern oldPptOmitRegexp = Daikon.ppt_omit_regexp;
    @Nullable String oldPptMaxName = Daikon.ppt_max_name;
    boolean oldRmStackDups = FileIO.dkconfig_rm_stack_dups;
    Daikon.ppt_regexp = null;
    Daikon.ppt_omit_regexp = null;
    Daikon.ppt_max_name = null;
    FileIO.dkconfig_rm_stack_dups = false;
    try {
      return FileIO.read_declaration_files(Collections.singletonList(declsFile));
    } finally {
      Daikon.ppt_regexp = oldPptRegexp;
      Daikon.ppt_omit_regexp = oldPptOmitRegexp;
      Daikon.ppt_max_name = oldPptMaxName;
      FileIO.dkconfig_rm_stack_dups = oldRmStackDups;
    }
  }
}
