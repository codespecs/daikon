/*
 *  (C) 2002 MIT Laboratory for Computer Science.
 *
 *  Author: Alan Donovan <adonovan@lcs.mit.edu>
 *
 *  JTrace.java -- jtrace host program (application thread)
 *
 *  $Id$
 *
 */

package daikon;
 
import java.lang.reflect.*;

import daikon.JTraceInference;

// This class is never instantiated. All methods are static.
abstract class JTrace
{
    public static void		main(String[] args)
    {
	// This usage message is displayed on behalf of the toplevel
	// driver script.
        if(args.length < 1) {
            println(V_ERROR, 
		    "usage: jtrace [options] -- <classname> [args] ...");
	    return;
        }

	Method main_method = loadTargetProgram(args[0]);
	if(main_method == null)
	    return;

	// Crank up the system (load the libJTrace.so debugger)
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
	String[] newargs = new String[args.length - 1]; // (cdr args)
	System.arraycopy(args, 1, newargs, 0, newargs.length);
	boolean ok = runTargetProgram(main_method, newargs);

	// target is dead!  Now finish off...

	// stop producing trace data on this thread.  Causes the
	// Inference thread to die.
	stopTracing(Thread.currentThread());

	if(ok) // if it didn't fail to start...
	    inference.joinX(); // ...wait for it to stop

	System.setSecurityManager(null);

	println(V_INFO, "JTrace: Application thread stop.");
    }

    private static Method loadTargetProgram(String target)
    {
	// map dirnames to package names for convenience
	target = target.replace('/', '.');

	println(V_INFO, "JTrace: hosting target program `" + target + "'.");

	Class	cls = null;
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
	if(!Modifier.isPublic(mods) ||
	   !Modifier.isStatic(mods))
	{
	    println(V_ERROR, "JTrace: target's method `main' has wrong " +
			       "modifiers.");
	    return null; 
	}

	if(!method.toString().equals("public static void " + target +
				     ".main(java.lang.String[])"))
	{
	    println(V_ERROR, "JTrace: target's method `main' has wrong sig: "
			       + method);
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
	    try {
		throw e.getTargetException();
	    }
	    catch(SystemExitException e2)
	    {
		println(V_INFO, "JTrace: System.exit() intercepted.");
		// XXX note -- shutdown hooks not yet called;
		// target threads could still be running.  What do we do?
	    }
	    catch(Throwable e2)
	    {
		println(V_INFO, "JTrace: target exited due to exception: " 
				   + e2);
	    }
	}
	catch(Throwable e)
	{	    
	    println(V_ERROR, "JTrace: target's method `main' could not be "+
			       "invoked: "  + e);
	    return false; 
	}

	return true;
    }

    // startTracing() enables tracing for this thread and causes all
    // subsequently-forked threads to be traced as well (if the "all
    // threads" option is specified).
    private native static void		startTracing(Thread threadID);	

    // stopTracing() causes the STOP marker to be introduced into the
    // control stream, which the inference thread will take as a cue
    // to shutdown.
    private native static void		stopTracing(Thread threadID);	

    // get the verbosity level from the underlying C system
    private native static int		getVerbosity();

    // We subclass Error because it is much less likely that the target
    // program catches this anywhere, compared to other unchecked
    // exceptions such as RuntimeException.
    private static class SystemExitException extends Error {}

    private static class Enforcer extends SecurityManager
    {
	// XXX note: might be better if we just block forever in here,
	// rather than throw an exception: may get caught by a lazy
	// exception handler declaration!
	public void checkExit(int status) // System.exit() called
	    { throw new SystemExitException(); }
	// permit all other calls XXX review this!
	public void checkPermission(java.security.Permission p, Object o) {}
	public void checkPermission(java.security.Permission p) {}
    }
   
    // exported to JTraceInference:

    static final int V_ERROR	= 0;
    static final int V_INFO	= 1;
    static final int V_DEBUG	= 2;

    static void println(int verb, String msg)
    {
	if(verb > verbosity) return;
	System.err.println(msg);
    }
    static void print(int verb, String msg)
    {
	if(verb > verbosity) return;
	System.err.print(msg);
    }

    private static int	verbosity = 0;
}

/*
 * Local Variables:
 * c-basic-offset:	4
 * End:
 */
