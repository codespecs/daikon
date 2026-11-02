package daikon.test;

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
   * Writes the given lines to a decls file, and returns the program points that Daikon reads from
   * it. Afterward, FileIO again accepts a decls file of either format version, so this does not
   * affect later tests.
   *
   * @param declsFile a file, which this method overwrites
   * @param lines the lines of the decls file, without line terminators
   * @return the program points read from the decls file
   * @throws IOException if there is trouble writing or reading the decls file
   */
  public static PptMap readDecls(File declsFile, String... lines) throws IOException {
    StringBuilder decls = new StringBuilder();
    for (String line : lines) {
      decls.append(line).append('\n');
    }
    Files.write(declsFile.toPath(), decls.toString().getBytes(StandardCharsets.UTF_8));
    FileIO.resetNewDeclFormat();
    try {
      return FileIO.read_declaration_files(Collections.singletonList(declsFile));
    } finally {
      FileIO.resetNewDeclFormat();
    }
  }
}
