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
        return new ErdosSampleEvent(ppt, vt);
    }
    
    public static Vector generateBasicEventsFromSample(Event e)
    {   
        //Fill this in to produce basic events from a sample
   	if (e instanceof ErdosSampleEvent)
	{ 
        	return ((ErdosSampleEvent)e).generateBasicEvents();
	} else {
		return new Vector();
	}
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

	System.out.println("Starting ppt processing.");

	Vector newEvents = generateBasicEventsFromSample(e);

	System.out.println("Processing program point. " + String.valueOf(newEvents.size()) + " basic events seen.");

	global_scope.processEvent(e, newEvents);

	System.out.println("We have: " + global_scope.getAllChildInvariants().size() + " invariants.");

	//reportTemporalInvariants();
    }

    public void reportTemporalInvariants()
    {
	if (!active)
	    return;

	for(Iterator i = global_scope.getAllChildInvariants().iterator();
	    i.hasNext(); )
	{
		String out = ((TemporalInvariant)i.next()).outputString();

		System.out.println(out);
	}
    }
}
