package MapQuick;

import MapQuick2.*;


import junit.framework.*;
import java.io.*;
import java.util.Arrays;
import java6170.test.FilterTest;

public class PS3TestSuite extends TestSuite
{
    private static String _testDir = null;
    public static void setTestDir(String testDir)
    {
	_testDir = testDir;
    }
    public static String getTestDir()
    {
	if (_testDir == null)
	    throw new IllegalStateException
		("Test directory has not been set");
	return _testDir;
    }
    public static Test suite()
    {
	try {
	    return new PS3TestSuite();
	} catch (final Throwable t) {
	    // avoids annoying "could not invoke suite method" message
	    // by creating a Test that simply fails by complaining
	    return new TestCase("Loading PS3TestSuite") {
		    protected void runTest() throws Throwable
		    {
			throw t;
		    }
		};
	}
    }
    protected PS3TestSuite() throws FileNotFoundException, IOException
    {
	// get the input and output file pairs
	File testDir = new File(getTestDir());
	File[] inputs = getInputFiles(testDir);

	if ((inputs == null) || (inputs.length == 0))
	    throw new FileNotFoundException
		("No test input files found");

	// create tests for each pair
	for (int i = 0; i < inputs.length; i++) {
	    File output = getOutputFile(inputs[i]);
	    if (!output.exists()) continue; // ignore unmatched tests
	    this.addTest(makeFilterTest(getDescription(inputs[i]),
					new FileInputStream(inputs[i]),
					new FileReader(output)));
	}
    }
    private FilterTest makeFilterTest(String description,
				      InputStream input,
				      Reader output)
    {
        return new FilterTest(description,
			      PS3TestDriver.class,
			      input,
			      output) {
	      protected void assertLineMatch(int line_no,
					     String expected,
					     String actual)
	      {
		  if (!areEnoughEquals(expected, actual)) {
		    assertEquals("mismatch on line "+line_no+":",
				 expected, actual);
		  }
	      }
	    };
    }
    private File[] getInputFiles(final File dir)
    {
        File[] result = dir.listFiles(new FilenameFilter() {
		public boolean accept(File file, String name)
		{
		    return name.endsWith(".test");
		}
	    });
        Arrays.sort(result);
        return result;
    }
    private File getOutputFile(File input)
    {
	String filename = input.getName().substring
	    (0,input.getName().lastIndexOf('.'))
	    +".expected";
	return new File(input.getParent(), filename);
    }
    private String getDescription(File file) throws FileNotFoundException, IOException
    {
	String desc = (new BufferedReader(new FileReader(file)))
	    .readLine();
	if (desc == null || !desc.startsWith("#"))
	    return file.getName();  // just use the filename
	return file.getName()+" "+desc;	// prepend the filename
    }

  // this method ignores whitespace differences at the start and end
  // of the string, as well as any whitespace which neighbors a colon
  public static boolean areEnoughEquals(String exp, String act)
  {
    if ((exp == null) && (act == null))
      return true;

    if ((exp == null) || (act == null))
      return false;

    if (exp.equals(act))
      return true;

    exp = exp.trim();
    act = act.trim();
    if (exp.equals(act))
      return true;

    // split on first ':' then recurse
    int e = exp.indexOf(':');
    int a = act.indexOf(':');
    if ((e < 0) || (a < 0))
      return false;
    String exp_pre = exp.substring(0, e);
    String act_pre = act.substring(0, a);
    exp = exp.substring(e+1);
    act = act.substring(a+1);

    if (!exp_pre.trim().equals(act_pre.trim()))
      return false;

    return areEnoughEquals(exp, act);
  }

}
