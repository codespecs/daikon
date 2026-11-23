package daikon.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;

import daikon.Daikon;
import daikon.FileIO;
import daikon.PptMap;
import daikon.PptRelation;
import daikon.PptTopLevel;
import daikon.ValueTuple;
import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.Collections;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.checker.nullness.qual.RequiresNonNull;
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
    PptMap ppts = readDeclsOnly(decls);
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
        assertThrows(Daikon.UserError.class, () -> PptRelation.checkDeclaredParents(ppts));
    assertEquals(
        "ppt P:::POINT is declared as a parent of ppt Q:::POINT, but its type (point) makes it a"
            + " leaf of the dataflow hierarchy",
        String.valueOf(e.getMessage()));
  }

  /**
   * A point named like a combined exit can be declared as a parent if it is replaced by a combined
   * exit, because a numbered exit is declared.
   */
  @Test
  public void testReplaceableCombinedExitAsParent() throws IOException {
    File declsFile = tmpFolder.newFile();
    String contents =
        "decl-version 2.0\nvar-comparability none\n\n"
            + pptDecl("Foo.bar():::ENTER", "enter")
            + pptDecl("Foo.bar():::EXIT", "point")
            + pptDecl("Foo.bar():::EXIT5", "subexit")
            + pptDecl("Q:::POINT", "point", "parent", "Foo.bar():::EXIT");
    Files.write(declsFile.toPath(), contents.getBytes(StandardCharsets.UTF_8));
    PptMap ppts = FileIO.read_declaration_files(Collections.singletonList(declsFile));
    // As in Daikon, the declared parents are checked before the combined exits are created.
    PptRelation.checkDeclaredParents(ppts);
    Daikon.create_combined_exits(ppts);
    PptRelation.init_hierarchy(ppts);
    PptTopLevel exit = get(ppts, "Foo.bar():::EXIT");
    assertTrue(exit.is_combined_exit());
    assertEquals(exit, get(ppts, "Q:::POINT").parents.get(0).parent);
  }

  /** A dtrace record that is a sample of P:::POINT, in which x is 1. */
  private static final String SAMPLE_P = "P:::POINT\nx\n1\n1\n\n";

  /** A dtrace record that is a sample of Q:::POINT, in which x is 1. */
  private static final String SAMPLE_Q = "Q:::POINT\nx\n1\n1\n\n";

  /**
   * A FileIO.Processor that counts the samples, but does not otherwise process them. It can check
   * the declared parents before each sample, as Daikon does.
   */
  private static class CountingProcessor extends FileIO.Processor {
    /** True if this checks the declared parents before each sample, as Daikon does. */
    private final boolean checkParents;

    /** The number of samples that have been passed to {@link #process_sample}. */
    int samples = 0;

    /**
     * Creates a CountingProcessor.
     *
     * @param checkParents true if the processor checks the declared parents before each sample, as
     *     Daikon does
     */
    CountingProcessor(boolean checkParents) {
      this.checkParents = checkParents;
    }

    @Override
    @RequiresNonNull("daikon.FileIO.data_trace_state")
    public void process_sample(
        PptMap all_ppts, PptTopLevel ppt, ValueTuple vt, @Nullable Integer nonce) {
      if (checkParents) {
        FileIO.data_trace_state.parentChecker.check(FileIO.data_trace_state);
      }
      samples++;
    }
  }

  /**
   * Reads the given dtrace file contents, which can include ppt declarations.
   *
   * @param contents the records of a dtrace file, after its header
   * @param processor a processor that is passed each record
   * @return the program points
   */
  private PptMap readDtrace(String contents, FileIO.Processor processor) throws IOException {
    File dtraceFile = tmpFolder.newFile("test.dtrace");
    String allContents = "decl-version 2.0\nvar-comparability none\n\n" + contents;
    Files.write(dtraceFile.toPath(), allContents.getBytes(StandardCharsets.UTF_8));
    PptMap ppts = new PptMap();
    FileIO.read_data_trace_file(dtraceFile.toString(), ppts, processor, false, true);
    return ppts;
  }

  /**
   * Asserts that the given error reports that P:::POINT is declared as a parent of Q:::POINT in
   * file test.dtrace.
   *
   * @param e a UserError
   */
  private static void assertPointAsParentInDtrace(Daikon.UserError e) {
    String msg = String.valueOf(e.getMessage());
    assertTrue(msg, msg.contains("test.dtrace"));
    assertTrue(
        msg,
        msg.endsWith(
            "ppt P:::POINT is declared as a parent of ppt Q:::POINT, but its type (point) makes it"
                + " a leaf of the dataflow hierarchy"));
  }

  /**
   * A point that a dtrace file declares as a parent is reported before any sample is processed,
   * even if the child is declared before the parent.
   */
  @Test
  public void testPointAsParentInDtrace() {
    CountingProcessor processor = new CountingProcessor(true);
    String contents =
        pptDecl("Q:::POINT", "point", "parent", "P:::POINT")
            + pptDecl("P:::POINT", "point")
            + SAMPLE_P;
    Daikon.UserError e =
        assertThrows(Daikon.UserError.class, () -> readDtrace(contents, processor));
    assertPointAsParentInDtrace(e);
    assertEquals(0, processor.samples);
  }

  /**
   * A point that a dtrace file declares as a parent is reported before its first sample, even if a
   * sample of its child precedes its declaration.
   */
  @Test
  public void testPointAsParentDeclaredAfterSampleInDtrace() {
    CountingProcessor processor = new CountingProcessor(true);
    String contents =
        pptDecl("Q:::POINT", "point", "parent", "P:::POINT")
            + SAMPLE_Q
            + pptDecl("P:::POINT", "point")
            + SAMPLE_P;
    Daikon.UserError e =
        assertThrows(Daikon.UserError.class, () -> readDtrace(contents, processor));
    assertPointAsParentInDtrace(e);
    assertEquals(1, processor.samples);
  }

  /**
   * Reading a dtrace file does not itself check the declared parents, so a tool that does not use
   * the dataflow hierarchy, such as InvariantChecker, can read a point that is declared as a
   * parent.
   */
  @Test
  public void testPointAsParentInDtraceUnchecked() throws IOException {
    CountingProcessor processor = new CountingProcessor(false);
    readDtrace(
        pptDecl("Q:::POINT", "point", "parent", "P:::POINT")
            + pptDecl("P:::POINT", "point")
            + SAMPLE_P,
        processor);
    assertEquals(1, processor.samples);
  }

  /**
   * The parents of a numbered exit are not checked, because the hierarchy relates a numbered exit
   * only to its combined exit, whose parents are those of the first numbered exit.
   */
  @Test
  public void testPointAsParentOfSecondSubexit() throws IOException {
    PptMap ppts =
        readDecls(
            pptDecl("P:::POINT", "point")
                + pptDecl("Foo.bar():::ENTER", "enter")
                + pptDecl("Foo.bar():::EXIT11", "subexit")
                + pptDecl("Foo.bar():::EXIT22", "subexit", "parent", "P:::POINT"));
    PptRelation.checkDeclaredParents(ppts);
    PptRelation.init_hierarchy(ppts);
    assertEquals(Collections.emptyList(), get(ppts, "P:::POINT").children);
  }

  /**
   * Reads the given program point declarations, but does not create the combined exits.
   *
   * @param decls ppt declarations, in the format of a decls file
   * @return the program points
   */
  private PptMap readDeclsOnly(String decls) throws IOException {
    File declsFile = tmpFolder.newFile();
    String contents = "decl-version 2.0\nvar-comparability none\n\n" + decls;
    Files.write(declsFile.toPath(), contents.getBytes(StandardCharsets.UTF_8));
    return FileIO.read_declaration_files(Collections.singletonList(declsFile));
  }

  /**
   * A point that the sole numbered exit declares as a parent is reported before the combined exit,
   * which inherits the parent, is created.
   */
  @Test
  public void testPointAsParentOfSoleSubexit() throws IOException {
    PptMap ppts =
        readDeclsOnly(
            pptDecl("P:::POINT", "point")
                + pptDecl("Foo.bar():::ENTER", "enter")
                + pptDecl("Foo.bar():::EXIT11", "subexit", "parent", "P:::POINT"));
    Daikon.UserError e =
        assertThrows(Daikon.UserError.class, () -> PptRelation.checkDeclaredParents(ppts));
    assertEquals(
        "ppt P:::POINT is declared as a parent of ppt Foo.bar():::EXIT11, but its type (point)"
            + " makes it a leaf of the dataflow hierarchy",
        String.valueOf(e.getMessage()));
  }

  /**
   * A point that the first numbered exit declares as a parent is reported before the combined exit,
   * which inherits the parent, is created, even if the first numbered exit is declared last.
   */
  @Test
  public void testPointAsParentOfFirstSubexit() throws IOException {
    PptMap ppts =
        readDeclsOnly(
            pptDecl("Foo.bar():::EXIT22", "subexit")
                + pptDecl("Foo.bar():::ENTER", "enter")
                + pptDecl("Foo.bar():::EXIT11", "subexit", "parent", "P:::POINT")
                + pptDecl("P:::POINT", "point"));
    assertThrows(Daikon.UserError.class, () -> PptRelation.checkDeclaredParents(ppts));
  }

  /**
   * The parents of a numbered exit other than the first are not checked, even if it is declared
   * first.
   */
  @Test
  public void testPointAsParentOfSecondSubexitDeclaredFirst() throws IOException {
    PptMap ppts =
        readDeclsOnly(
            pptDecl("P:::POINT", "point")
                + pptDecl("Foo.bar():::EXIT22", "subexit", "parent", "P:::POINT")
                + pptDecl("Foo.bar():::ENTER", "enter")
                + pptDecl("Foo.bar():::EXIT11", "subexit"));
    PptRelation.checkDeclaredParents(ppts);
    Daikon.create_combined_exits(ppts);
    PptRelation.init_hierarchy(ppts);
    assertEquals(Collections.emptyList(), get(ppts, "P:::POINT").children);
  }

  /**
   * A dtrace file can declare a child before its parent, if the parent is not a leaf. The child's
   * samples can precede the parent's declaration.
   */
  @Test
  public void testParentDeclaredLaterInDtrace() throws IOException {
    CountingProcessor processor = new CountingProcessor(true);
    PptMap ppts =
        readDtrace(
            pptDecl("P:::POINT", "point", "parent", "C:::OBJECT")
                + SAMPLE_P
                + pptDecl("C:::OBJECT", "object")
                + SAMPLE_P,
            processor);
    assertEquals(2, processor.samples);
    PptRelation.init_hierarchy(ppts);
    assertEquals(1, get(ppts, "C:::OBJECT").children.size());
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
