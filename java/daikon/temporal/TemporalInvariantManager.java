package daikon.temporal;

import java.util.*;

import daikon.*;

public class TemporalInvariantManager
{
    public ScopeGlobal global_scope;
    public static boolean active = false;

    public TemporalInvariantManager()
    {
	global_scope = new ScopeGlobal();
    }

    //FIXME: Revise this!
    public Event generateSampleEvent(Ppt ppt, ValueTuple vt)
    {
	//Fill this in to produce a sample event from a program point and value tuple

	return null;
    }

    public Vector generateBasicEventsFromSample(Event e)
    {
	//Fill this in to produce basic events from a sample

	return null;
    }

    public void beginExecution()
    {
	global_scope.enter();
    }

    public void endExecution()
    {
	global_scope.exit();
    }

    public void processEvent(Event e)
    {
	if (!active)
	    return;

	Vector newEvents = generateBasicEventsFromSample(e);

	global_scope.processEvent(e, newEvents);
    }

    public void reportTemporalInvariants()
    {
	if (!active)
	    return;

	//FIXME: Improve!
	global_scope.printState();
    }
}
