package daikon.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import daikon.PptConditional;
import daikon.PptTopLevel;
import daikon.PptTopLevel.PptType;
import daikon.VarInfo;
import daikon.split.misc.ReturnTrueSplitter;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import org.junit.Test;

/** Tests the type of a {@link PptTopLevel}. */
public class PptTopLevelTypeTest {

  /** Creates a PptTopLevelTypeTest. */
  public PptTopLevelTypeTest() {}

  /**
   * Serializes and deserializes a program point, as writing and reading a .inv file does.
   *
   * @param ppt a program point
   * @return a copy of the program point, read from its serialized form
   */
  private static PptTopLevel serializeAndDeserialize(PptTopLevel ppt)
      throws IOException, ClassNotFoundException {
    ByteArrayOutputStream bytes = new ByteArrayOutputStream();
    try (ObjectOutputStream out = new ObjectOutputStream(bytes)) {
      out.writeObject(ppt);
    }
    try (ObjectInputStream in =
        new ObjectInputStream(new ByteArrayInputStream(bytes.toByteArray()))) {
      return (PptTopLevel) in.readObject();
    }
  }

  /** Reading a conditional program point keeps its parent's type. */
  @Test
  public void testTypeOfDeserializedConditionalPpt() throws IOException, ClassNotFoundException {
    PptTopLevel combinedExit = new PptTopLevel("Foo.bar():::EXIT", new VarInfo[0]);
    PptConditional conditional = new PptConditional(combinedExit, new ReturnTrueSplitter(), false);
    assertTrue(conditional.is_combined_exit());
    PptTopLevel read = serializeAndDeserialize(conditional);
    assertTrue(read.is_combined_exit());
    assertFalse(read.is_subexit());
  }

  /** A program point's name indicates the THROWS and GLOBAL types. */
  @Test
  public void testThrowsAndGlobalTypeFromName() {
    assertEquals(PptType.THROWS, new PptTopLevel("Foo.bar():::THROWS", new VarInfo[0]).type);
    assertEquals(PptType.THROWS, new PptTopLevel("Foo.bar():::THROWS12", new VarInfo[0]).type);
    assertEquals(PptType.GLOBAL, new PptTopLevel(":::GLOBAL", new VarInfo[0]).type);
  }

  /** A program point declared as THROWS or GLOBAL must have a name of that type. */
  @Test
  public void testThrowsAndGlobalTypeError() {
    assertNull(PptTopLevel.type_error("Foo.bar():::THROWS", PptType.THROWS));
    assertNull(PptTopLevel.type_error("Foo.bar():::THROWS12", PptType.THROWS));
    assertNull(PptTopLevel.type_error("Foo.bar()", PptType.THROWS));
    assertNull(PptTopLevel.type_error(":::GLOBAL", PptType.GLOBAL));
    assertNotNull(PptTopLevel.type_error("Foo.bar():::ENTER", PptType.THROWS));
    assertNotNull(PptTopLevel.type_error("Foo.bar():::THROWS", PptType.GLOBAL));
    assertNotNull(PptTopLevel.type_error("Foo.bar():::GLOBAL", PptType.THROWS));
  }
}
