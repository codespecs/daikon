/*
 *  Copyright 2002-2003 MIT Laboratory for Computer Science.
 *
 *  Author: Alan Donovan <adonovan@lcs.mit.edu>
 *
 *  JTrace.java -- jtrace host program (application thread)
 *
 *  $Id$
 *
 */

package daikon;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.*;
import utilMDE.ArraysMDE;

/**
 * JTrace is a wrapper program that runs both a target program and
 * Daikon's inference engine in parallel.  It also loads, starts, and
 * stops the libJTrace.so debugger library. <p>
 *
 * When the target program exits, this class writes a binary-format
 * ".inv" file that contains the detected invariants.  The file may be
 * viewed with the PrintInvariants tool, or one of the GUIs.
 **/
public final class JTrace
{
    // This class is never instantiated. All methods are static.
    private JTrace() { throw new Error("do not instantiate"); }

    public static void main(String[] args)
    {
        try {
            mainHelper(args);
        } catch (Daikon.TerminationMessage e) {
            System.err.println(e.getMessage());
            System.exit(1);
        }
        // Any exception other than Daikon.TerminationMessage gets propagated.
        // This simplifies debugging by showing the stack trace.
    }

    /**
     * This does the work of main, but it never calls System.exit, so it
     * is appropriate to be called progrmmatically.
     * Termination of the program with a message to the user is indicated by
     * throwing Daikon.TerminationMessage.
     * @see #main(String[])
     * @see daikon.Daikon.TerminationMessage
     **/
    public static void mainHelper(final String[] args)
    {
        // This usage message is displayed on behalf of the toplevel
        // driver script.
        if (args.length < 1) {
            println(V_ERROR,
                    "usage: jtrace [options] -- <classname> [args] ...");
            return;
        }

        println(V_ERROR, "JTrace: running on " +
                System.getProperty("java.vm.vendor") + " JVM " +
                System.getProperty("java.vm.version"));

        String target_name = args[0]; // first is program, rest is its args
        String[] target_args = ArraysMDE.subarray(args, 1, args.length - 1);

        Method main_method = loadTargetProgram(target_name);
        if (main_method == null)
            return;

        // Crank up the system (load the libJTrace.so debugger)
        println(V_INFO, "JTrace: loading shared library.");
        System.loadLibrary("JTrace");

        JTrace.verbosity = getVerbosity();

        // create and start the data-harvesting loop:
        JTraceInference inference = new JTraceInference();

        try {
            System.setSecurityManager(new Enforcer());
        } catch(Throwable t) {
            println(V_ERROR, "JTrace: can't set SecurityManager: " + t);
        }

        println(V_INFO, "JTrace: Application thread start.");

        // enable production of trace data from this thread:
        startTracing(Thread.currentThread());

        // Now start the target program such that calls to exit from
        // within it are caught as exceptions here:
        boolean ok = runTargetProgram(main_method, target_args);

        // target is dead!  Now finish off...

        // stop producing trace data on this thread.  Causes the
        // Inference thread to die.
        stopTracing(Thread.currentThread());

        if (ok) // if it didn't fail to start...
            inference.joinX(); // ...wait for it to stop

        System.setSecurityManager(null);

        if (inference.failure_message != null) {
            throw new Daikon.TerminationMessage("JTrace: inference thread failed with message: "
                               + inference.failure_message);
        }

        // Write out the invariant file
        File inv_file = new File("temp.inv"); // XXX read name from command line with -o, like Daikon
        try {
            FileIO.write_serialized_pptmap(inference.all_ppts, inv_file);
        } catch (IOException e) {
            throw new Daikon.TerminationMessage("Error while writing '" + inv_file + "': " + e);
        }

        // Display the invariants
        PrintInvariants.print_invariants(inference.all_ppts);

        println(V_INFO, "JTrace: Application thread stop.");
    }

    private static Method loadTargetProgram(String target)
    {
        // map dirnames to package names for convenience
        target = target.replace('/', '.');

        println(V_INFO, "JTrace: hosting target program `" + target + "'.");

        Class   cls = null;
        try {
            cls = Class.forName(target);
        } catch(ClassNotFoundException e) {
            println(V_ERROR, "JTrace: couldn't find class `" + target + "'.");
            return null;
        }
        catch(Throwable e) {
            println(V_ERROR, "JTrace: couldn't load class `" + target
                               + "': " + e.getMessage());
            return null;
        }

        Method method = null;
        try {
            method = cls.getMethod("main", new Class[] { String[].class });
        }
        catch(Throwable e) {
            println(V_ERROR, "JTrace: couldn't find method `main' in class `"
                               + target + "'.");
            return null;
        }

        int mods = method.getModifiers();
        if (!Modifier.isPublic(mods) ||
           !Modifier.isStatic(mods))
        {
            println(V_ERROR, "JTrace: target's method `main' has wrong " +
                    "modifiers.");
            return null;
        }

        if (!Void.TYPE.equals(method.getReturnType()))
        {
            println(V_ERROR, "JTrace: target's method `main' has wrong " +
                    "return type (" + method.getReturnType() + ")");
            return null;
        }

        if (!(method.getParameterTypes().length == 1))
        {
            println(V_ERROR, "JTrace: target's method `main' has wrong " +
                    "argument count (" + method.getParameterTypes().length + ")");
            return null;
        }

        if (!String[].class.equals(method.getParameterTypes()[0]))
        {
            println(V_ERROR, "JTrace: target's method `main' has wrong " +
                    "argument type (" + method.getParameterTypes()[0] + ")");
            return null;
        }

        return method;
    }

    public static boolean runTargetProgram(Method main_method, String[] args)
    {
        try {
            // this lets us access mains in default-access classes
            // inside a package:
            main_method.setAccessible(true);

            main_method.invoke(null, new Object[] { args });
        }
        catch(InvocationTargetException e)
        {
            if (e.getTargetException() instanceof SystemExitException)
            {
                println(V_INFO, "JTrace: System.exit() intercepted.");
                // XXX note -- shutdown hooks not yet called;
                // target threads could still be running.  What do we do?
            }
            else
            {
                println(V_INFO, "JTrace: target exited due to exception: "
                                   + e.getTargetException());
            }
        }
        catch(Throwable e)
        {
            println(V_ERROR, "JTrace: target's method `main' could not be " +
                               "invoked: "  + e);
            return false;
        }

        return true;
    }

    // startTracing() enables tracing for this thread and causes all
    // subsequently-forked threads to be traced as well (if the "all
    // threads" option is specified).
    private static native void          startTracing(Thread threadID);

    // stopTracing() causes the STOP marker to be introduced into the
    // control stream, which the inference thread will take as a cue
    // to shutdown.
    private static native void          stopTracing(Thread threadID);

    // get the verbosity level from the underlying C system
    private static native int           getVerbosity();

    // We subclass Error because it is much less likely that the target
    // program catches this anywhere, compared to other unchecked
    // exceptions such as RuntimeException.
    private static class SystemExitException extends SecurityException {
        static final long serialVersionUID = 20050923L;
    }

    private static class Enforcer extends SecurityManager
    {
        // XXX We need a better mechanism than this; otherwise a
        // catch-all exception handler (such as that around the Java
        // Swing event dispatcher) will suppress this, so it will
        // never get seen by us.  Push the EOF control token through
        // now?
        public void checkExit(int status) { // System.exit() called
            // XXX While not obviously wrong in any way, this
            // doesn't seem quite right.

            // Note that the exception will not be seen by the tracing
            // code because this class is uninstrumented.  Unless it
            // is caught and rethrown in app code.

            stopTracing(Thread.currentThread());
            throw new SystemExitException();
        }
        // permit all other calls XXX review this!
        public void checkPermission(java.security.Permission p, Object o) {}
        public void checkPermission(java.security.Permission p) {}
    }

    // exported to JTraceInference:

    static final int V_ERROR    = 0;
    static final int V_INFO     = 1;
    static final int V_DEBUG    = 2;

    static void println(int verb, String msg)
    {
        if (verb > verbosity) return;
        System.err.println(msg);
        System.err.flush();
    }
    static void print(int verb, String msg)
    {
        if (verb > verbosity) return;
        System.err.print(msg);
        System.err.flush();
    }

    /**
     * The verbosity is actually determined by libJTrace, the shared
     * library.  Changing it here will have no effect.
     **/
    private static int  verbosity = 0;
}

/*
 * Local Variables:
 * c-basic-offset:      4
 * End:
 */
