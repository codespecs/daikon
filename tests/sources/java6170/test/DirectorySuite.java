package java6170.test;

import junit.framework.*;
import java.io.*;

public class DirectorySuite extends TestSuite
{
    // TODO:
    // accept FilenameFilter for input files
    // accept FilenameMapper for output files
    // use DiffTest instead of FilterTest
    public DirectorySuite(String testDirName, Class testDriver)
	throws FileNotFoundException, IOException
    {
	// get the input and output file pairs
	File testDir = new File(testDirName);
	File[] inputs = getInputFiles(testDir);

	if (inputs.length == 0)
	    throw new FileNotFoundException
		("No test input files found");

	// create tests for each pair
	for (int i = 0; i < inputs.length; i++) {
	    File output = getOutputFile(inputs[i]);
	    if (!output.exists()) continue; // ignore unmatched tests
	    this.addTest(makeFilterTest(inputs[i].getName(),
					testDriver,
					new FileInputStream(inputs[i]),
					new FileReader(output)));
	}
    }
    private FilterTest makeFilterTest(String description,
				      Class testDriver,
				      InputStream input,
				      Reader output)
    {
        return new FilterTest(description,
			      testDriver,
			      input,
			      output) {
		protected void assertLineMatch(int lineNum,
					       String expected,
					       String actual)
		{
		    super.assertEofMatch(lineNum, expected, actual);
		    // compare lines fuzzily
		    if (!areEnoughEquals(expected, actual)) {
			assertEquals("mismatch on line "+lineNum+":",
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
