package daikon.test;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.LineNumberReader;

import daikon.asm.AsmFile;
import junit.framework.TestCase;


public class AsmFileTest extends TestCase {


  public static void testParser() throws IOException {

    InputStream stream = AsmFileTest.class.getResourceAsStream("galar.asm");
    InputStreamReader streamReader = new InputStreamReader(stream);
    AsmFile file = new AsmFile(new LineNumberReader(streamReader));

    // The example asm file contains 152 basic blocks.
    assert 152 == file.numBasicBlocks();

  }

}
