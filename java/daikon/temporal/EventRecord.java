package daikon.temporal;

import java.util.*;

/**
 * This class is basically a hashtable and some glue. It's used to answer
 * the question "have I seen event x in scope y"
 **/

class EventRecord
{
    //FIXME: Make this more efficient somehow?
    //Possibly make this implement Collection? Or do we care?
    //HashSet mEvents;
    boolean modified;
    Vector kids;

    Vector mEvents;

    EventRecord()
    {
//	mEvents = new HashSet(0);
	mEvents = new Vector();
	modified = false;

	kids = new Vector();
    }

    void disownKids()
    {
	for(Iterator i = kids.iterator(); i.hasNext(); )
	    {
		EventRecord r = (EventRecord)i.next();
		
		if (!r.modified)
		    {
			r.modified = true;
			
			r.mEvents = (Vector)mEvents.clone();
			
			r.disownKids();
		    }
		else
		    {
			//FIXME: Remove kids reference here later
		    }
	    }
	
    }
    
    void add(Event e)
    {
	if (!modified)
	    {
		modified = true;
	    }

	disownKids();
			  
	mEvents.add(e);
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
	mEvents.addAll(r.mEvents);
    }

    boolean hasEventMatching(Event e)
    {
	return mEvents.contains(e);
    }

    boolean seenAnyEvents()
    {
	return (mEvents.size() > 0);
    }

    boolean noEventsConflictWith(Event e)
    {
	for(Iterator i = mEvents.iterator(); i.hasNext(); )
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
	
	out.mEvents = mEvents;

	kids.add(out);

	return out;
    }

}
