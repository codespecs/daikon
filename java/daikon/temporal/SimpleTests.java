package daikon.temporal;

import junit.framework.*;
import java.util.*;

class SampleEvent extends Event
{
    String name;

    SampleEvent(String n)
    {
	name = n;
    }

    public boolean forwardMatches(Event e)
    {
	if (e instanceof LetterEvent)
	    {
		LetterEvent l = (LetterEvent)e;

		return (name.indexOf(l.name) != -1);
	    }
	else
	    {
		SampleEvent s = (SampleEvent)e;

		return name.equals(s.name);
	    }
    }

    public boolean forwardSharesTypeWith(Event e)
    {
	return (e instanceof SampleEvent) || (e instanceof LetterEvent);
    }

    public String toString()
    {
	return name;
    }
}

class TestManager extends TemporalInvariantManager
{
    TestManager()
    {
	super();

	TemporalInvariantManager.active = true;
    }

    public Vector generateBasicEventsFromSample(Event e)
	{
	    Vector out = new Vector();

	    if (e instanceof SampleEvent)
		{
		    SampleEvent s = (SampleEvent)e;

		    for (int i = 0; i < s.name.length(); i++)
			{
			    String letterString = String.valueOf(s.name.charAt(i));

			    out.add(LetterEvent.getEvent(letterString));
			}
		}

	    return out;
	}
}

// FIXME: Needs some major restructuring for efficiency/clarity.
public class SimpleTests
{
    public static void main(String[] args)
    {
	TestManager m = new TestManager();

	SampleEvent abc = new SampleEvent("ABC");
	SampleEvent bcd = new SampleEvent("BCD");
	SampleEvent cde = new SampleEvent("CDE");

	m.beginExecution();

	m.processEvent(abc);

	m.processEvent(bcd);

	m.processEvent(cde);

	m.endExecution();
    }
}
