package MapQuick;

import MapQuick2.*;


import java.io.*;
import java6170.test.FilterTest;
import junit.framework.*;

public class TextUITest extends TestSuite
{    
    public TextUITest() 
    {
        this("Problem Set 6 TextUI Test");
    }

    public TextUITest(String name)
    {
        super(name);
        addBadPathTest();
        addBadComponentTests();
        addCorrectTest();
    }
    
    private static String expectedStart =
        "starting number? starting street? starting zipcode? " +
        "destination number? destination street? destination zipcode? ";

    private static String expectedEnd = "starting number? ";

    public static String tinyPath = "/g4/projects/invariants/tests/mapquick/tiny/";

    // Create specialized test cases.
    private void addBadPathTest()
    {
        addTest(new FilterTest("InvalidDatabaseDirectory", TextUI.class,
                               new StringBufferInputStream(""),
                               new StringReader("Database error\n"))
            {
                protected Object[] mainParams()
                    {
                        return new Object[]
                            { new String[] { "/no/such/path" } };
                    }
            });
    }

    private void addBadComponentTests()
    {
        addTextUITest("WrongStartZIPCode",
                      "42\nWauwinet Rd\n01111\n" +
                      "42\nWauwinet Rd\n02554\n",
                      "No such zipcode: 42 Wauwinet Rd 01111\n");
        addTextUITest("WrongStartStreet",
                      "42\nStupid Rd\n02554\n" +
                      "42\nWauwinet Rd\n02554\n",
                      "No such street: 42 Stupid Rd 02554\n");
        addTextUITest("WrongStartNumber",
                      "46\nWauwinet Rd\n02554\n" +
                      "42\nWauwinet Rd\n02554\n",
                      "No such number: 46 Wauwinet Rd 02554\n");
        addTextUITest("WrongEndZIPCode",
                      "42\nWauwinet Rd\n02554\n" +
                      "42\nWauwinet Rd\n01111\n",
                      "No such zipcode: 42 Wauwinet Rd 01111\n");
        addTextUITest("WrongEndStreet",
                      "42\nWauwinet Rd\n02554\n" +
                      "42\nStupid Rd\n02554\n",
                      "No such street: 42 Stupid Rd 02554\n");
        addTextUITest("WrongEndNumber",
                      "42\nWauwinet Rd\n02554\n" +
                      "46\nWauwinet Rd\n02554\n",
                      "No such number: 46 Wauwinet Rd 02554\n");
    }

    private void addCorrectTest()
    {
        addTextUITest("CorrectOutputFormat",
                      "42\nWauwinet Rd\n02554\n" +
                      "54\nWauwinet Rd\n02554\n",
                      "Start at 42 Wauwinet Rd 02554\n" +
                      "Turn left onto Wauwinet Rd and go 0.7 miles.\n" +
                      "54 Wauwinet Rd 02554 is on your left\n" +
                      "Trip length: 0.7 miles\n");
    }

    private void addTextUITest(String description, String in, String out)
    {
        addTest(makeTextUITest(description, in, out,
                               new String[] { tinyPath }));
    }

    /** Create a java6170.test.FilterTest to test ps6.TextUI.main().
     * The test will check for equality to the provided output given
     * the provided input.  The input will be followed by "0\n",
     * indicating that the main loop should exit.  The output should
     * be preceded by expectedStart and succeeded by expectedEnd.
     * (That is, the parameters should not include these strings,
     * since this method will add them in explicitly.)
     *
     * @param description: Description of the test case
     * @param input: Input to be provided to test
     * @param output: Expected output from test
     * @param params: Array of command-line parameters to pass to TextUI.main()
     * @returns: A new FilterTest as described above
     */
    private FilterTest makeTextUITest(String description,
                                      String input,
                                      String output,
                                      final String[] params)
    {
        return new FilterTest(description, TextUI.class,
                              new StringBufferInputStream(input + "0\n"),
                              new StringReader(expectedStart + output))
            {
                protected Object[] mainParams()
                    {
                        return new Object[] { params };
                    }

                private boolean sawExpectedEnd = false;
                private boolean sawNewline = false;
                
                protected void assertLineMatch(int lineNum,
                                               String expected,
                                               String actual)
                {
                    // Manually check for special end-of-file cases.
                    if (expected == null)
                    {
                        // If the actual input is an empty line and we
                        // haven't seen one, then retroactively expect it.
                        if (actual != null &&
                            actual.equals("") &&
                            !sawNewline)
                        {
                            sawNewline = true;
                            expected = "";
                        }
                        else if (!sawExpectedEnd)
                        {
                            sawExpectedEnd = true;
                            expected = expectedEnd;
                        }
                    }
                    super.assertLineMatch(lineNum, expected, actual);
                }
            };
    }
}

  
