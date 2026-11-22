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

  // JUnit creates an instance of this class for each test, so these fields hold the values of the
  // global settings from just before the test.

  /** The value of {@link Daikon#use_dataflow_hierarchy} before the test. */
  private final boolean savedUseDataflowHierarchy = Daikon.use_dataflow_hierarchy;

  /** The value of {@link PptRelation#dkconfig_enable_object_user} before the test. */
  private final boolean savedEnableObjectUser = PptRelation.dkconfig_enable_object_user;

  /** Creates an InitHierarchyTest. */
  public InitHierarchyTest() {}

  /** Restores the global settings that a test changes. */
  @After
  public void restoreSettings() {
    Daikon.use_dataflow_hierarchy = savedUseDataflowHierarchy;
    PptRelation.dkconfig_enable_object_user = savedEnableObjectUser;
  }

  /**
   * Returns the declaration of the given program point, with one int variable, x.
   *
   * @param name a name for the program point
   * @param type the ppt-type of the program point, such as "point"
   * @return the declaration, in the format of a decls file
   */
  private static String pptDecl(String name, String type) {
    return pptDecl(name, type, null, null);
  }

  /**
   * Returns the declaration of the given program point, with one int variable, x, which is related
   * to variable x of the parent.
   *
   * @param name a name for the program point
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
   * Returns the splitters of the given program point. Throws AssertionError if ppt has no list of
   * splitters.
   *
   * @param ppt a program point on which {@link PptTopLevel#addConditions} created a list of
   *     splitters, which might since have become empty
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

  /** A point that is named like an enter point is not the parent of the combined exit. */
  @Test
  public void testPointNamedEnter() throws IOException {
    PptMap ppts =
        initHierarchy(
            pptDecl("Foo.bar():::ENTER", "point") + pptDecl("Foo.bar():::EXIT5", "subexit"));
    PptTopLevel enter = get(ppts, "Foo.bar():::ENTER");
    assertTrue(enter.isDataflowLeaf());
    assertEquals(Collections.emptyList(), enter.children);
    PptTopLevel exit = get(ppts, "Foo.bar():::EXIT");
    assertEquals(Collections.emptyList(), exit.parents);
  }

  /** A point, which is a leaf, cannot be declared as the parent of another program point. */
  @Test
  public void testPointWithChild() throws IOException {
    PptMap ppts =
        readDecls(
            pptDecl("Foo:::POINT", "point")
                + pptDecl("Foo:::POINT2", "point", "parent", "Foo:::POINT"));
    assertThrows(Daikon.UserError.class, () -> PptRelation.init_hierarchy(ppts));
  }

  /**
   * When a combined exit's first splitter is removed because its numbered exits lack it, the
   * conditional ppts of the remaining splitter have the combined exit as their parent.
   */
  @Test
  public void testFirstSplitterRemoved() throws IOException {
    Daikon.use_dataflow_hierarchy = true;
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
    Splitter splitter = split.splitter;
    if (splitter == null) {
      throw new AssertionError("no splitter at " + exit.name());
    }
    assertSame(kept, splitter);
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
    Daikon.use_dataflow_hierarchy = true;
    PptMap ppts = readDecls(pptDecl("Foo.bar():::ENTER", "enter"));
    PptTopLevel enter = get(ppts, "Foo.bar():::ENTER");
    enter.addConditions(new Splitter[] {new ConditionSplitter("x == 1")});
    PptRelation.init_hierarchy(ppts);
    assertEquals(0, splitters(enter).size());
  }

  /**
   * With the hierarchy, a non-leaf whose only child is a childless non-leaf loses its splitters,
   * even though it is processed before its child and the child has a matching splitter.
   */
  @Test
  public void testChildlessSplitterCascades() throws IOException {
    Daikon.use_dataflow_hierarchy = true;
    PptMap ppts =
        readDecls(
            pptDecl("Foo:::CLASS", "class")
                + pptDecl("Foo:::OBJECT", "object", "parent", "Foo:::CLASS"));
    Splitter splitter = new ConditionSplitter("x == 1");
    PptTopLevel classPpt = get(ppts, "Foo:::CLASS");
    PptTopLevel objectPpt = get(ppts, "Foo:::OBJECT");
    classPpt.addConditions(new Splitter[] {splitter});
    objectPpt.addConditions(new Splitter[] {splitter});
    PptRelation.init_hierarchy(ppts);
    assertEquals(0, splitters(classPpt).size());
    assertEquals(0, splitters(objectPpt).size());
    assertTrue(objectPpt.equality_view != null);
  }
}
