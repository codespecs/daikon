/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.tools.benchmarks;

import java.util.*;
import ajax.jbc.*;
import ajax.jbc.util.*;
import java.io.*;
import ajax.analyzer.*;
import ajax.analyzer.semantics.*;
import ajax.analyzer.util.*;
import ajax.util.*;
import ajax.*;

public class ComparablePairsDescFileReader extends Benchmark {
    private boolean rewriteDescFile;
    private JBCClassLoader appLoader;
    private Vector descFileReaders = new Vector();

    public JBCClassLoader getAppLoader() {
	return appLoader;
    }

    protected ComparablePairsDescFileReader() {
    }

    public void configure(Analyzer analyzer) {
        analyzer.setSemantics(new CombiningSemantics());

        for (Enumeration e = descFileReaders.elements(); e.hasMoreElements();) {
	    ((ComparablePairsDescFile)e.nextElement()).configure(analyzer);
	}
    }

    public void printReport(Writer w) throws IOException {
        if (rewriteDescFile) {
            for (Enumeration e = descFileReaders.elements(); e.hasMoreElements();) {
	        ((ComparablePairsDescFile)e.nextElement()).printRewrittenFile();
	    }
        } else {
            for (Enumeration e = descFileReaders.elements(); e.hasMoreElements();) {
	        ((ComparablePairsDescFile)e.nextElement()).printBasicReport(w);
	    }
	}
    }

    protected void notifyAppClassLoader(JBCClassLoader appClassLoader) {
        appLoader = appClassLoader;
    }

    public void parseArgs(Args args) {
	/* If set, produces a rewritten decls file containing comparability information written into Daikon format.
	   Otherwise we produce a raw matrix giving compatibility data. */
        rewriteDescFile = args.extractBoolOption("-rewrite");

	/* Specifies the base directory for the decls files. The package that a decls file belongs to is computed
	   by stripping this base directory off the decls file name. */
        String declsDir = args.extractStringOption("-declsdir", null);

	/* One or more decls files come here */
	String fileName = args.extractNextArg("<descfile>");

	descFileReaders.addElement(new ComparablePairsDescFile(this, fileName, declsDir));

	while (true) {
	    fileName = args.extractNextOptionalArg();
	    if (fileName == null) {
		break;
	    } else {
		descFileReaders.addElement(new ComparablePairsDescFile(this, fileName, declsDir));
	    }
	}
    }

    public static void main(String[] args) {
        GeneralBenchmark.run(args, new ComparablePairsDescFileReader(), "[-rewrite] <descfile> [descfiles...]");
    }
}
