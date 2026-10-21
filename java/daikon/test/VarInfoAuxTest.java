package daikon.test;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertThrows;

import daikon.VarInfoAux;
import java.io.IOException;
import org.junit.Test;

/** Tests for {@link VarInfoAux}. */
public class VarInfoAuxTest {

  /**
   * Parses an aux string and returns its valid values.
   *
   * @param auxString the string to parse, which must define {@link VarInfoAux#VALID_VALUES}
   * @return the valid values in the parsed aux string
   * @throws IOException if the aux string cannot be parsed
   */
  private static String[] getValidValues(String auxString) throws IOException {
    VarInfoAux aux = VarInfoAux.parse(auxString);
    if (!aux.hasValue(VarInfoAux.VALID_VALUES)) {
      throw new AssertionError("No valid values in: " + auxString);
    }
    return aux.getList(VarInfoAux.VALID_VALUES);
  }

  /** Tests that {@link VarInfoAux#getList} returns the elements of a bracketed list. */
  @Test
  public void testGetListBracketed() throws IOException {
    assertArrayEquals(new String[] {"0", "1"}, getValidValues("validvalues=[0 1]"));
    assertArrayEquals(new String[] {"true"}, getValidValues("validvalues=[true]"));
    assertArrayEquals(new String[] {}, getValidValues("validvalues=[]"));
  }

  /** Tests that {@link VarInfoAux#getList} rejects a value without brackets. */
  @Test
  public void testGetListUnbracketed() {
    assertThrows(IllegalStateException.class, () -> getValidValues("validvalues=0"));
    assertThrows(IllegalStateException.class, () -> getValidValues("validvalues=0 1"));
  }
}
