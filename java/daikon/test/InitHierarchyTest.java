package daikon.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;

import daikon.Daikon;
import daikon.FileIO;
import daikon.PptMap;
import daikon.PptRelation;
import daikon.PptTopLevel;
import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.Collections;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.junit.After;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

/** Tests {@link PptRelation#init_hierarchy} on program points that are leaves of the hierarchy. */
public class InitHierarchyTest {

  /** A temporary directory for decls files. */
  @Rule public TemporaryFolder tmpFolder = new TemporaryFolder();

  /** Creates an InitHierarchyTest. */
  public InitHierarchyTest() {}

  /** Restores the global settings that a test changes. */
  @After
  public void restoreSettings() {
    Daikon.use_dataflow_hierarchy = true;
    PptRelation.dkconfig_enable_object_user = false;
  }

  /**
   * Returns a ppt declaration with one int variable, x.
   *
   * @param name the name of the program point
   * @param type the ppt-type of the program point, such as "point"
   * @return the declaration, in the format of a decls file
   */
  private static String pptDecl(String name, String type) {
    return pptDecl(name, type, null, null);
  }

  /**
   * Returns a ppt declaration with one int variable, x, which is related to variable x of the
   * parent.
   *
   * @param name the name of the program point
   * @param type the ppt-type of the program point, such as "point"
   * @param relType the type of the relation to the parent, such as "parent" or "user", or null for
   *     no parent
   * @param parentName the name of the parent program point, or null for no parent
   * @return the declaration, in the format of a decls file
   */
  private static String pptDecl(
      String name, String type, @Nullable String relType, @Nullable String parentName) {
    StringBuilder sb = new StringBuilder();
    sb.append("ppt ").append(name).append("\n");
    sb.append("ppt-type ").append(type).append("\n");
    if (parentName != null) {
      sb.append("parent ").append(relType).append(" ").append(parentName).append(" 1\n");
    }
    sb.append("variable x\n");
    sb.append("  var-kind variable\n");
    sb.append("  dec-type int\n");
    sb.append("  rep-type int\n");
    if (parentName != null) {
      sb.append("  parent ").append(parentName).append(" 1 x\n");
    }
    sb.append("\n");
    return sb.toString();
  }

  /**
   * Reads the given program point declarations, creates the combined exits, and builds the
   * hierarchy, as Daikon does.
   *
   * @param decls ppt declarations, in the format of a decls file
   * @return the program points
   */
  private PptMap initHierarchy(String decls) throws IOException {
    File declsFile = tmpFolder.newFile();
    String contents = "decl-version 2.0\nvar-comparability none\n\n" + decls;
    Files.write(declsFile.toPath(), contents.getBytes(StandardCharsets.UTF_8));
    PptMap ppts = FileIO.read_declaration_files(Collections.singletonList(declsFile));
    Daikon.create_combined_exits(ppts);
    PptRelation.init_hierarchy(ppts);
    return ppts;
  }

  /**
   * Returns the program point with the given name.
   *
   * @param ppts a map of program points
   * @param name the name of a program point in the map
   * @return the program point named name
   */
  private static PptTopLevel get(PptMap ppts, String name) {
    PptTopLevel result = ppts.get(name);
    if (result == null) {
      throw new AssertionError("no program point named " + name);
    }
    return result;
  }

  /** A point that is named like an enter point is not the parent of the combined exit. */
  @Test
  public void testPointNamedEnter() throws IOException {
    PptMap ppts =
        initHierarchy(
            pptDecl("Foo.bar():::ENTER", "point") + pptDecl("Foo.bar():::EXIT5", "subexit"));
    PptTopLevel enter = get(ppts, "Foo.bar():::ENTER");
    assertTrue(enter.is_dataflow_leaf());
    assertEquals(Collections.emptyList(), enter.children);
    PptTopLevel exit = get(ppts, "Foo.bar():::EXIT");
    assertEquals(Collections.emptyList(), exit.parents);
  }

  /** A point cannot be declared as a parent. */
  @Test
  public void testPointAsParent() {
    String decls =
        pptDecl("P:::POINT", "point") + pptDecl("Q:::POINT", "point", "parent", "P:::POINT");
    Daikon.UserError e = assertThrows(Daikon.UserError.class, () -> initHierarchy(decls));
    assertEquals(
        "ppt P:::POINT is declared as a parent of ppt Q:::POINT, but its type (point) makes it a"
            + " leaf of the dataflow hierarchy",
        String.valueOf(e.getMessage()));
  }

  /** A point can be declared as a parent when the hierarchy is not used. */
  @Test
  public void testPointAsParentWithoutHierarchy() throws IOException {
    Daikon.use_dataflow_hierarchy = false;
    initHierarchy(
        pptDecl("P:::POINT", "point") + pptDecl("Q:::POINT", "point", "parent", "P:::POINT"));
  }

  /** A user relation to a point is ignored by default, so it is permitted. */
  @Test
  public void testPointAsUserParent() throws IOException {
    PptMap ppts =
        initHierarchy(
            pptDecl("P:::POINT", "point") + pptDecl("Q:::POINT", "point", "user", "P:::POINT"));
    assertEquals(Collections.emptyList(), get(ppts, "P:::POINT").children);
  }

  /** A user relation to a point is forbidden when user relations are enabled. */
  @Test
  public void testPointAsEnabledUserParent() {
    PptRelation.dkconfig_enable_object_user = true;
    String decls =
        pptDecl("P:::POINT", "point") + pptDecl("Q:::POINT", "point", "user", "P:::POINT");
    assertThrows(Daikon.UserError.class, () -> initHierarchy(decls));
  }
}
