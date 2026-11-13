package daikon.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;

import daikon.Daikon;
import daikon.FileIO;
import daikon.PptMap;
import daikon.PptRelation;
import daikon.PptTopLevel;
import daikon.split.PptSplitter;
import daikon.split.Splitter;
import daikon.test.split.ConditionSplitter;
import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.Collections;
import java.util.List;
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
   * Reads the given program point declarations and creates the combined exits, as Daikon does.
   *
   * @param decls ppt declarations, in the format of a decls file
   * @return the program points
   */
  private PptMap readDecls(String decls) throws IOException {
    File declsFile = tmpFolder.newFile();
    String contents = "decl-version 2.0\nvar-comparability none\n\n" + decls;
    Files.write(declsFile.toPath(), contents.getBytes(StandardCharsets.UTF_8));
    PptMap ppts = FileIO.read_declaration_files(Collections.singletonList(declsFile));
    Daikon.create_combined_exits(ppts);
    return ppts;
  }

  /**
   * Reads the given program point declarations, creates the combined exits, and builds the
   * hierarchy, as Daikon does.
   *
   * @param decls ppt declarations, in the format of a decls file
   * @return the program points
   */
  private PptMap initHierarchy(String decls) throws IOException {
    PptMap ppts = readDecls(decls);
    PptRelation.init_hierarchy(ppts);
    return ppts;
  }

  /**
   * Returns the splitters of the given program point.
   *
   * @param ppt a program point that has splitters
   * @return the splitters of ppt
   */
  private static List<PptSplitter> splitters(PptTopLevel ppt) {
    List<PptSplitter> result = ppt.splitters;
    if (result == null) {
      throw new AssertionError("no splitters at " + ppt.name());
    }
    return result;
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

  /** A point that is named like an enter point is not the entry of a numbered exit. */
  @Test
  public void testPointNamedEnter() {
    String decls = pptDecl("Foo.bar():::ENTER", "point") + pptDecl("Foo.bar():::EXIT5", "subexit");
    Daikon.UserError e = assertThrows(Daikon.UserError.class, () -> initHierarchy(decls));
    assertTrue(
        String.valueOf(e.getMessage()),
        String.valueOf(e.getMessage())
            .startsWith("exit found with no corresponding entry: Foo.bar():::EXIT5"));
  }

  /**
   * A point that is named like a combined exit is not replaced by a combined exit, and it is not
   * the parent of the numbered exits.
   */
  @Test
  public void testPointNamedCombinedExit() throws IOException {
    PptMap ppts =
        initHierarchy(
            pptDecl("Foo.bar():::ENTER", "enter")
                + pptDecl("Foo.bar():::EXIT", "point")
                + pptDecl("Foo.bar():::EXIT5", "subexit"));
    PptTopLevel exit = get(ppts, "Foo.bar():::EXIT");
    assertTrue(exit.is_dataflow_leaf());
    assertEquals(Collections.emptyList(), exit.children);
    assertEquals(Collections.emptyList(), get(ppts, "Foo.bar():::EXIT5").parents);
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

  /** A point that is declared as a parent is reported as soon as the declarations are read. */
  @Test
  public void testPointAsParentBeforeHierarchy() throws IOException {
    PptMap ppts =
        readDecls(
            pptDecl("P:::POINT", "point") + pptDecl("Q:::POINT", "point", "parent", "P:::POINT"));
    Daikon.UserError e =
        assertThrows(Daikon.UserError.class, () -> PptRelation.check_declared_parents(ppts));
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

  /**
   * When a combined exit's first splitter is removed because its numbered exits lack it, the
   * conditional ppts of the remaining splitter have the combined exit as their parent.
   */
  @Test
  public void testFirstSplitterRemoved() throws IOException {
    PptMap ppts =
        readDecls(
            pptDecl("Foo.bar():::ENTER", "enter")
                + pptDecl("Foo.bar():::EXIT5", "subexit")
                + pptDecl("Foo.bar():::EXIT7", "subexit"));
    Splitter removed = new ConditionSplitter("x == 1");
    Splitter kept = new ConditionSplitter("x == 2");
    PptTopLevel exit = get(ppts, "Foo.bar():::EXIT");
    exit.addConditions(new Splitter[] {removed, kept});
    get(ppts, "Foo.bar():::EXIT5").addConditions(new Splitter[] {kept});
    get(ppts, "Foo.bar():::EXIT7").addConditions(new Splitter[] {kept});
    PptRelation.init_hierarchy(ppts);

    List<PptSplitter> exitSplitters = splitters(exit);
    assertEquals(1, exitSplitters.size());
    PptSplitter split = exitSplitters.get(0);
    assertSame(kept, split.splitter);
    for (PptTopLevel cond : split.ppts) {
      assertEquals(1, cond.parents.size());
      assertSame(exit, cond.parents.get(0).parent);
    }
  }

  /**
   * Without the hierarchy, a non-leaf with no children keeps its splitters, because it receives
   * samples directly.
   */
  @Test
  public void testChildlessSplitterWithoutHierarchy() throws IOException {
    Daikon.use_dataflow_hierarchy = false;
    PptMap ppts = readDecls(pptDecl("Foo.bar():::ENTER", "enter"));
    PptTopLevel enter = get(ppts, "Foo.bar():::ENTER");
    enter.addConditions(new Splitter[] {new ConditionSplitter("x == 1")});
    PptRelation.init_hierarchy(ppts);
    assertEquals(1, splitters(enter).size());
  }

  /** With the hierarchy, a non-leaf with no children loses its splitters. */
  @Test
  public void testChildlessSplitterWithHierarchy() throws IOException {
    PptMap ppts = readDecls(pptDecl("Foo.bar():::ENTER", "enter"));
    PptTopLevel enter = get(ppts, "Foo.bar():::ENTER");
    enter.addConditions(new Splitter[] {new ConditionSplitter("x == 1")});
    PptRelation.init_hierarchy(ppts);
    assertEquals(0, splitters(enter).size());
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
