/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax;

import java.util.Hashtable;
import java.io.*;
import ajax.util.IdentityManager;

/**
<h3>Error handling</h3>

An error detected by a component can be classified into one of three categories:
<ul>
<li><strong>User errors</strong>. The error occurred because of some inappropriate
user action. For example, invalid bytecode was submitted.</li>
<li><strong>Nonlocal errors</strong>. The error occurred because some
other component misused the interface to this component.</li>
<li><strong>Local errors</strong>. The error occurred because of a
bug in this component.</li>
</ul>
Generally, a component is considered to be an entire package.

Local and nonlocal errors are not guaranteed to be checked unless
the Globals.debug flag is 'true'. User errors must always be checked.

Local and nonlocal errors are signalled by calling a method below. These methods
throw fatal exceptions when debugging, but the caller should attempt to recover
if debugging is turned off. If recovery is impossible, the Throwable returned
by the method can be thrown. These methods are very good places to set breakpoints
while debugging!

User errors are signalled using some scenario-specific mechanism. This may
involve throwing a checked exception, but it may not --- especially when the
error is detected far from the source, e.g. a bytecode typecheck failure.
Recovery must always be attempted. The method userError below may be used
to report an error to the user.

We also support a logging mechanism. Events that are not strictly "errors"
but may be unusual and helpful for diagnosing problems can be reported
using Globals.writeLog().
*/
public class Globals {
    private static Writer logWriter = null;
    private static String logFileName = null;
    private static boolean fatalErrors = false;

    private static Writer getLogWriter() {
        if (logWriter == null) {
            try {
                logFileName = "log";
                logWriter = new FileWriter(logFileName);
            } catch (IOException ex) {
                System.err.println("Could not open log file, writing log to standard error");
                logFileName = "<System.err>";
                logWriter = new OutputStreamWriter(System.err);
            }
        }

        return logWriter;
    }

    private static String getLogFileName() {
        if (logFileName == null) {
            getLogWriter();
        }
        return logFileName;
    }

    private static Globals exitHandler = new Globals();

    private Globals() {
        System.runFinalizersOnExit(true);
    }

    protected void finalize() {
        flushLog();
    }

    public static void flushLog() {
        try {
            getLogWriter().flush();
        } catch (IOException ex) {
        }
    }

    public static void setFatalErrors(boolean fatal) {
        fatalErrors = fatal;
    }

    public static void setLogFileName(String name) {
        try {
            logWriter = new FileWriter(name);
            logFileName = name;
        } catch (IOException ex) {
            System.err.println("Could not open new log file " + name);
        }
    }

/** This turns all debugging code on or off. */
    public static final boolean debug = true; // CONFIG
/** This turns determinism on or off.
    We can't make things truly deterministic, but we try. */
    public static final boolean debugDeterminism = false; // CONFIG

    private static RuntimeException dumpStack(RuntimeException t) {
        System.err.println("Sorry, an internal error occurred:");
        if (fatalErrors) {
            throw t;
        } else {
            t.printStackTrace();
            System.err.println("Attempting to continue, but damage may accrue.");
            return t;
        }
    }

    public static RuntimeException localError(String s) {
        writeLog(null, "ERROR: " + s);
        flushLog();
        return dumpStack(new RuntimeException(s));
    }

    public static RuntimeException nonlocalError(String s) {
	writeLog(null, "ERROR: " + s);
        flushLog();
        return dumpStack(new RuntimeException(s));
    }

    public static void userError(String s) {
	writeLog(null, "ERROR: " + s);
        flushLog();
        System.err.println(s);
    }

/**
Write an entry to the log. The source object (or a Class if this is a static
method) is provided to help with filtering. Usually the caller will just
provide "this". A String can also be provided with the class name, in a pinch.
*/
    public static void writeLog(Object source, String s) {
        /*
        String className;

        if (source instanceof Class) {
            className = ((Class)source).getName();
        } else if (source instanceof String) {
            className = (String)source;
        } else {
            className = source.getClass().getName();
        } */

        try {
            Writer log = getLogWriter();

            synchronized (log) {
                log.write(s);
                log.write('\n');
                flushLog();
            }
        } catch (IOException ex) {
            System.err.println("I/O error writing to log file "
                               + getLogFileName() + ": " + ex);
        }
    }

    public static String getHexID(Object o) {
        return "0x" + Integer.toHexString(IdentityManager.getIdentityHashCode(o));
    }
}
