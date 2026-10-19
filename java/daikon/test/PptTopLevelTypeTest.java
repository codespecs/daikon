package daikon.test;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import daikon.PptTopLevel;
import daikon.PptTopLevel.PptType;
import daikon.VarInfo;
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
    @SuppressWarnings("nullness") // an earlier version of Daikon could leave the type null
    PptType noType = null;
    enter.type = noType;
    read = serializeAndDeserialize(enter);
    assertTrue(read.is_enter());
  }
}
