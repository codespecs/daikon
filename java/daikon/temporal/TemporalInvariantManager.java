package daikon.temporal;

import java.util.*;

import daikon.*;

/**
 * This class serves as the glue between the body of Daikon and the temporal
 * invariant detector. It accepts program points (and the associated value
 * tuples) from Daikon, generates sample events and basic events (explained
 * in Event.java) from them, and dispatches them to the global scope so that
 * candidate invariants may be generated and the event can be processed.
 * It also is responsible for reporting results (currently to stdout).
 *
 * It should be noted that this particular class will need some significant
 * revision once the proper interface for "interesting event" definition is
 * settled upon. In particular, the event-generation methods should obviously
 * not be domain-specific.
 *
 * SKETCH OF TEMPORAL INVARIANT DETECTION
 *
 * The temporal invariant manager produces events which are fed into a
 * tree of scopes and invariants. Each scope optionally generates new
 * candidate invariants based on the events it sees, then processes
 * the event fed to it (sending it to its child invariants for possible
 * falsification and to its child scopes for processing). Once all dtrace
 * files (each corresponding to a particular execution) are processed, the
 * temporal invariant detector reports what invariants it has detected, if
 * any.
 *
 * It should be noted that the EventReceptor class captures common
 * behavior of all denizens of the scope/invariant tree, Scope
 * captures scope behavior, and TemporalInvariant captures invariant
 * behavior.
 **/

public class TemporalInvariantManager
{
    public Scope.ScopeGlobal global_scope; // Only one global scope should exist per manager.
    public static boolean active = false;
    public static TemporalInvariantManager Manager = null; // Only one manager should be instantiated at a
    // time

    public TemporalInvariantManager()
    {
	global_scope = new Scope.ScopeGlobal();
	Manager = this;
    }


    // This method takes a sample from the tracefile (stored as a ppt and valuetuple)
    // and produces an event which captures all of its properties.
    // FIXME: Revise this!
    public Event generateSampleEvent(Ppt ppt, ValueTuple vt)
    {
	// Fill this in to produce a sample event from a program point and value tuple
        return new ErdosSampleEvent(ppt, vt);
    }

    // This method takes a sampleEvent (that is, an event capturing all properties of a
    // given sample) and produces a list of the primitive events it consists of (all the
    // things which happened since the last sample).
    public Vector generateBasicEventsFromSample(Event e)
    {
        // Fill this in to produce basic events from a sample
   	if (e instanceof ErdosSampleEvent)
	{
        	return ((ErdosSampleEvent)e).generateBasicEvents();
	} else {
		return new Vector();
	}
    }

    // Signal the start of an execution.
    public void beginExecution()
    {
	global_scope.enter();
    }

    // Signal the end of an execution.
    public void endExecution()
    {
	global_scope.exit();
    }

    // Process a sample event (convert it to basic events and send it to all invariants in the
    // hierarchy).
    public void processEvent(Event e)
    {
	if (!active)
	    return;

	System.out.println("Starting ppt processing.");

	Vector newEvents = generateBasicEventsFromSample(e);

	System.out.println("Processing program point. " + String.valueOf(newEvents.size()) + " basic events seen.");

	global_scope.processEvent(e, newEvents);

	System.out.println("We have: " + global_scope.getAllChildInvariants().size() + " invariants.");

    }

    // Output the scope/invariant tree.
    public void reportTemporalInvariants()
    {
	if (!active)
	    return;

	for (Iterator i = global_scope.getAllChildInvariants().iterator();
	    i.hasNext(); )
	{
		String out = ((TemporalInvariant)i.next()).outputString();

		System.out.println(out);
	}
    }
}
