/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.tools.benchmarks;

import java.util.*;
import ajax.jbc.*;
import ajax.jbc.util.*;
import ajax.jbc.util.reflect.*;
import java.io.*;
import ajax.analyzer.*;
import ajax.analyzer.rta.*;
import ajax.analyzer.util.*;
import ajax.analyzer.semi.*;
import ajax.analyzer.hybrid.*;
import ajax.solver.*;
import ajax.util.*;
import ajax.Globals;
import java.lang.reflect.*;

class GeneralBenchmark {
    private static final int MB = 1024*1024;
    private Analyzer analyzer;
    private Benchmark benchmark;
    private SEMIAnalyzer SEMIInstance = null;
    private RTA RTAInstance = null;
    private String mainClass = null;
    private Analyzer RTAAnalyzer = null;

    GeneralBenchmark() {
    }

    String getMainClassName() {
        return mainClass;
    }

    private static String memString(long bytes) {
        return Long.toString((bytes + MB - 1)/MB) + "MB";
    }

    private void makeEngine(String analysisType, Args args, JBCWorld world) {
        RTAInstance = new RTA(world);
        RTAAnalyzer = new Analyzer(RTAInstance);

        if (analysisType.equals("RTA")) {
            analyzer = RTAAnalyzer;
        } else if (analysisType.equals("RTA-SEMI")) {
            SEMIInstance = new SEMIAnalyzer(RTAAnalyzer);
            analyzer = new Analyzer(SEMIInstance);
        } else if (analysisType.equals("RTA-SEMI/RTA")) {
            SEMIInstance = new SEMIAnalyzer(RTAAnalyzer);

            GenericAnalyzer[] sources = { SEMIInstance, new RTA(world) };

            analyzer = new Analyzer(new HybridAnalysis(sources));
        } else {
            throw args.printUsageErrorAndDie("Invalid analysis type: " + analysisType);
        }
    }

    public static String fractionToString(int numerator, int denominator) {
        return numerator + " out of " + denominator + " ("
            + numerator*100/denominator + "%)";
    }

    static void run(String[] argStrings, Benchmark b) {
        run(argStrings, b, null, "RTA-SEMI/RTA");
    }

    static void run(String[] argStrings, Benchmark b, String specialOptions) {
        run(argStrings, b, specialOptions, "RTA-SEMI/RTA");
    }

    static void run(String[] argStrings, Benchmark b, String specialOptions, String defaultAnalysis) {
        (new GeneralBenchmark()).go(argStrings, b,
            specialOptions == null ? "" : " " + specialOptions, defaultAnalysis);
    }

    void terminate(String reason) {
        System.err.println(reason);
        Globals.writeLog(this, "ERROR: Terminated --- " + reason);
	    benchmark.terminate();
    }

    private static PrintStream makePrintSinkStream() {
        // use reflection here to avoid a @deprecated warning
        // the deprecated constructor is the only way to do this!
        try {
            Class[] paramTypes = { Class.forName("java.io.OutputStream") };
            Object[] args = { new SinkOutputStream() };

            return (PrintStream)Class.forName("java.io.PrintStream")
                .getDeclaredConstructor(paramTypes).newInstance(args);
        } catch (ClassNotFoundException ex) {
            return null;
        } catch (IllegalAccessException ex) {
            return null;
        } catch (NoSuchMethodException ex) {
            return null;
        } catch (InstantiationException ex) {
            return null;
        } catch (InvocationTargetException ex) {
            return null;
        }
    }

    void go(String[] argStrings, Benchmark b, String specialOptions, String defaultAnalysis) {
	int result = 30;

        Globals.writeLog(this, "Started on " + (new Date()).toString() + "\n"
            + "Command line arguments: " + StringUtils.join(" ", argStrings));

        try {
	        benchmark = b;

            Args args = new Args(argStrings,
                "Usage: <main-class>" + specialOptions
                    + " [-cp <class-path>] [-ap <application-path>] [-dump] [-dumpdir <dir>] [-iterlimit <n>] [-iterfun <function>] [-lenient]"
                    + " [-analysis <analysis-name>] [-timelimit <seconds>] [-server] [-fatalerrors]\n"
                    + "Valid <analysis-name>s are 'RTA', 'SEMI', 'RTA-SEMI', 'RTA-SEMI/RTA'.\n"
                    + "Additional options for SEMI: [-semi-hindleymilner] [-semi-opdump] [-semi-ndvirtuals] [-semi-checkconsistency]\n"
                    + "                             [-semi-usesubchunks] [-semi-usesubobjects]\n"
                    + "Additional options for RTA: [-rta-nopreciseclasses] [-rta-noinstanceoftracking]\n");

            if (args.extractBoolOption("-help")
                || args.extractBoolOption("-h")) {
                throw args.printUsageAndDie();
            }

            boolean quiet = args.extractBoolOption("-quiet");
            Globals.quietUserError = quiet;
            boolean doDump = args.extractBoolOption("-dump");
            int iterationLimit = args.extractIntOption("-iterlimit", Integer.MAX_VALUE);
            int timeLimit = args.extractIntOption("-timelimit", Integer.MAX_VALUE);
            String iterationFunction = args.extractStringOption("-iterfun", null);
            String classPath = args.extractStringOption("-cp", null);
            String appPath = args.extractStringOption("-ap", null);
            String serverName = args.extractStringOption("-server", null);
            String dumpDir = args.extractStringOption("-dumpdir", null);
            boolean lenientVerification = args.extractBoolOption("-lenient");
            boolean SEMIDumpOperations = args.extractBoolOption("-semi-opdump");
            boolean SEMICheckConsistency = args.extractBoolOption("-semi-checkconsistency");
            boolean SEMIHindleyMilnerMode = args.extractBoolOption("-semi-hindleymilner");
            boolean SEMINondeterministicVirtuals = args.extractBoolOption("-semi-ndvirtuals");
            boolean SEMIUseSubchunks = args.extractBoolOption("-semi-usesubchunks");
            boolean SEMIUseSubobjects = args.extractBoolOption("-semi-usesubobjects");
            boolean RTADisablePreciseClassTypes = args.extractBoolOption("-rta-nopreciseclasses");
            boolean RTADisableInstanceOfTracking = args.extractBoolOption("-rta-noinstanceoftracking");
            boolean fatalErrors = args.extractBoolOption("-fatalerrors");
            boolean noStdErr = args.extractBoolOption("-nostderr");
            boolean noStdOut = args.extractBoolOption("-nostdout");
            String analysisName = args.extractStringOption("-analysis", defaultAnalysis);

            if (noStdErr) {
                System.setErr(makePrintSinkStream());
            }

            if (noStdOut) {
                System.setOut(makePrintSinkStream());
            }

            b.parseOptions(args);

            mainClass = args.extractNextArg("main class name");
            if (mainClass.indexOf('/') != -1) {
                String mainClassDot = mainClass.replace('/', '.');
                Globals.userError("Main class " + mainClass + " should be specified as " + mainClassDot);
                return;
            }

            b.parseArgs(args);

            args.checkDone();

            Globals.setFatalErrors(fatalErrors);

            if (dumpDir != null) {
	            if (!dumpDir.endsWith(File.separator)) {
		            dumpDir = dumpDir + File.separator;
	            }
                Globals.setLogFileName(dumpDir + "ajax.log");
                IdentityManager.setBindingsLogFileName(dumpDir + "bindings");
            }

            BasicAnalyzerStats stats = new BasicAnalyzerStats();
            GeneralBenchmarkConfig cfg = new GeneralBenchmarkConfig();

            if (classPath != null) {
                cfg.setClassPath(classPath);
            }
            if (appPath != null) {
                cfg.setAppClassPath(appPath);
            }
            cfg.setStats(stats);

            cfg.getWorld().setVerificationLenient(lenientVerification);
            cfg.init();

            makeEngine(analysisName, args, cfg.getWorld());

            if (SEMIInstance != null) {
                if (dumpDir != null) {
                    SEMIInstance.setDumpDirectory(dumpDir);
	            }
                SEMIInstance.setNondeterministicVirtuals(SEMINondeterministicVirtuals);
                SEMIInstance.setCheckConsistency(SEMICheckConsistency);
                SEMIInstance.setDumpOperations(SEMIDumpOperations);
                SEMIInstance.setCoalesceClusterLevelInstances(SEMIHindleyMilnerMode);
                SEMIInstance.setUseSubchunks(SEMIUseSubchunks);
                SEMIInstance.setUseSubobjects(SEMIUseSubobjects);
            }

            if (RTAInstance != null) {
                RTAInstance.setUsePreciseClassTypes(!RTADisablePreciseClassTypes);
                RTAInstance.setUseInstanceOfTracking(!RTADisableInstanceOfTracking);
            }

            analyzer.enableLogging();
            analyzer.setIterationLimit(iterationLimit, iterationFunction);
            analyzer.setStatisticsListener(stats);

            b.notifyAppClassLoader(cfg.getApplicationClassLoader());

            ReflectionHandler reflectionHandler = cfg.getReflectionHandler();

            if (reflectionHandler != null) {
                reflectionHandler.setReceiveLivenessNotifications();
                RTAAnalyzer.setReflectionHandler(reflectionHandler);
            }

            b.configure(analyzer);

            if (!b.usesDynamicQueryFamilies()) {
                analyzer.disableNewQueryFamilies();
            }

            try {
                cfg.addMainInvocation(mainClass);
            } catch (UnresolvedClassException ex) {
                Globals.userError("Main class not found: " + ex.getClassName());
                return;
            } catch (MissingMethodException ex) {
                Globals.userError("Main method not found in " + ex.getClassName());
                return;
            } catch (AmbiguousMethodException ex) {
                Globals.userError("Ambiguous main method in " + ex.getClassName());
                return;
            }

            boolean finishedNormally = false;

            (new GeneralBenchmarkTerminationThread(this)).start();

            GeneralBenchmarkTimerTerminationThread terminator
                = new GeneralBenchmarkTimerTerminationThread(this, timeLimit);

            terminator.start();

            try {
                byte[] savedSpace = new byte[1000000];

                finishedNormally = !b.work(analyzer);
            } catch (Error ex) {
                StringWriter w = new StringWriter();

                w.write("Internal error: ");
                ex.printStackTrace(new PrintWriter(w));
                Globals.writeLog(this, w.toString());
                System.err.println(w.toString());
            } catch (RuntimeException ex) {
                StringWriter w = new StringWriter();

                w.write("Internal error: ");
                ex.printStackTrace(new PrintWriter(w));
                Globals.writeLog(this, w.toString());
                System.err.println(w.toString());
            }

            try {
                StringWriter errWriter = new StringWriter();
                Runtime r = Runtime.getRuntime();

                errWriter.write("Iterations: " + analyzer.getIterations() + "\n");
                errWriter.write("Max memory used: " + memString(terminator.getMaxUsedMemory()) + "\n");

                System.gc();

                errWriter.write("Current memory used: " + memString(r.totalMemory() - r.freeMemory()) + "\n");
                errWriter.write("Total memory allocated: " + memString(r.totalMemory()) + "\n");

                stats.write(errWriter);

                Globals.writeLog(this, errWriter.toString());
                if (! quiet) {
                    System.err.println(errWriter.toString());
                }

                if (finishedNormally) {
                    StringWriter writer = new StringWriter();

                    b.printReport(writer);
                    System.out.println(writer.toString());
		    result = 0;
                }
            } catch (IOException ex) {
                Globals.userError("Error printing stats: " + ex.getMessage());
            }

            if (doDump && SEMIInstance != null) {
                SEMIInstance.dumpInfo();
            }
        } catch (Exception ex) {
            ex.printStackTrace();
        } catch (Error ex) {
            ex.printStackTrace();
        } finally {
            Globals.writeLog(this, "Completed on " + (new Date()).toString());
            Globals.flushLog();
            System.gc();
            System.exit(result);
        }
    }
}
