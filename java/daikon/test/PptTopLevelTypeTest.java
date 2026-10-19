package daikon.test;

import static org.junit.Assert.assertFalse;
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

  /**
   * Sets the type of a program point to null, as an earlier version of Daikon could.
   *
   * @param ppt a program point
   */
  @SuppressWarnings("nullness:assignment") // an earlier version of Daikon could leave the type null
  private static void clearType(PptTopLevel ppt) {
    ppt.type = null;
  }

  /**
   * A .inv file written by an earlier version of Daikon may have no type for a program point, or
   * the type EXIT for a numbered exit. Reading it gives the program point the type that its name
   * indicates.
   */
  @Test
  public void testTypeOfDeserializedPpt() throws IOException, ClassNotFoundException {
    PptTopLevel numberedExit = new PptTopLevel("Foo.bar():::EXIT22", new VarInfo[0]);
    numberedExit.type = PptType.EXIT;
    PptTopLevel read = serializeAndDeserialize(numberedExit);
    assertTrue(read.is_subexit());
    assertFalse(read.is_combined_exit());

    PptTopLevel enter = new PptTopLevel("Foo.bar():::ENTER", new VarInfo[0]);
    clearType(enter);
    read = serializeAndDeserialize(enter);
    assertTrue(read.is_enter());
  }

  /** Reading a conditional program point keeps its parent's type. */
  @Test
  public void testTypeOfDeserializedConditionalPpt() throws IOException, ClassNotFoundException {
    PptTopLevel combinedExit = new PptTopLevel("Foo.bar():::EXIT", new VarInfo[0]);
    PptConditional conditional =
        new PptConditional(combinedExit, new ReturnTrueSplitter(), false);
    assertTrue(conditional.is_combined_exit());
    PptTopLevel read = serializeAndDeserialize(conditional);
    assertTrue(read.is_combined_exit());
    assertFalse(read.is_subexit());
  }
}
