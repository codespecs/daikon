package daikon.temporal;

import java.util.*;

/**
 * This class is basically a hashtable and some glue. It's used to answer
 * the question "have I seen event x in scope y"
 **/

class EventRecord
{
    // FIXME: Make this more efficient somehow?
    // Possibly make this implement Collection? Or do we care?
    Hashtable mEvents;

    EventRecord()
    {
	mEvents = new Hashtable();
    }

    void add(Event e)
    {
	mEvents.put(e, new Boolean(true));
    }

    void addAll(Collection c)
    {
	for (Iterator i = c.iterator(); i.hasNext(); )
	{
		add((Event)i.next());
	}
    }

    void add(EventRecord r)
    {
	mEvents.putAll(r.mEvents);
    }

    boolean hasEventMatching(Event e)
    {
	return mEvents.containsKey(e);
    }

    boolean seenAnyEvents()
    {
	return (mEvents.size() > 0);
    }

    boolean noEventsConflictWith(Event e)
    {
	for (Iterator i = mEvents.keySet().iterator(); i.hasNext(); )
	    {
		Event cur = (Event)i.next();

		if (e.sharesTypeWith(cur) && !e.matches(cur))
		    {
			return false;
		    }
	    }

	return true;
    }

    EventRecord duplicate()
    {
	EventRecord out = new EventRecord();

	out.mEvents = (Hashtable)mEvents.clone(); // FIXME: Is this right? How deep does clone copy, again?

	return out;
    }

}
