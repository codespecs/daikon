package daikon.temporal;

import java.util.*;

// Similar to ScopeState in Scope.java -- look there.
class InvariantState
{
    boolean isFalsified;
    int numConfirmingSequences;
}

/**
 * This class captures behavior common to all invariants. It's very
 * straightforward, as are all invariants (three types supported:
 * always, eventually, and never, though never invariants aren't
 * instantiated by the dynamic algorithm yet). The class
 * TemporalInvariant itself captures behavior common to all kinds of
 * invariants; each inner class provides implementation of a specific
 * kind of invariant based on this. This basically involves overriding the
 * appropriate methods from the EventReceptor abstract class.
 **/

// FIXME: numConfirmingSequences numbers may not be correct
// for dynamically instantiated invariants
public abstract class TemporalInvariant extends EventReceptor
{
    public boolean isFalsified; //Has the invariant been falsified
    public int numConfirmingSequences; //The number of sequences of events which satisfied the invariant

    TemporalInvariant()
    {
	super();

	isFalsified = false;
	numConfirmingSequences = 0;
    }

    TemporalInvariant(Scope s)
    {
	mParent = s;

	isFalsified = false;
	numConfirmingSequences = 0;

	s.addChild(this);
    }

    void falsify()
    {
	isFalsified = true;
    }

    //Produce an output-worthy string rep of the invariant
    //which captures its position in the scope hierarchy in one line
    public String outputString()
    {
	Scope parent = mParent;
	Vector strings = new Vector();

	while(parent != null)
	{
		strings.add(parent.getNameString());
		parent = parent.mParent;
	}

	Collections.reverse(strings);

	StringBuffer out = new StringBuffer();

	for (Iterator i = strings.iterator(); i.hasNext(); )
	{
		out.append((String)i.next());
	}

	out.append(toString());

	return out.toString();
    }


    //Produce a string rep of just the invariant itself
    public String toString()
    {
	StringBuffer res = new StringBuffer();

	res.append(getClass().toString());

	res.append(" -- " + getDescriptionString() + " -- ");

	if (!isFalsified)
	    {
		res.append(String.valueOf(numConfirmingSequences));
	    }
	else
	    {
		res.append("FALSIFIED");
	    }

	return res.toString();
    }

    //The next two methods generate and restore TemporalInvariant state
    Object generateStateSnapshot()
    {
	InvariantState is = new InvariantState();

	is.isFalsified = isFalsified;
	is.numConfirmingSequences = numConfirmingSequences;

	return is;
    }

    void restoreState(Object snapshot)
    {
	InvariantState is = (InvariantState)snapshot;

	isFalsified = is.isFalsified;
	numConfirmingSequences = is.numConfirmingSequences;

	// FIXME: This is kludgy (should be done consistent with inheritance pattern).
	// Also may be incorrect. Here's hoping it isn't!
	if (this instanceof EventuallyInvariant)
	    {
		((EventuallyInvariant)this).happenedOnceInScope = false;
	    }
    }

    //If the invariant has not yet been falsified when the parent scope exits,
    //a "confirming sequence" was just observed
    void parentScopeExiting()
    {
	if (!isFalsified)
	    {
		numConfirmingSequences += 1;
	    }
    }

    abstract String getDescriptionString();


    /* ***************************************************************************
     * Inner classes
     */

    //This invariant (not instantiated yet) captures the notion that the given
    //event does not occur in scope
    public static class NeverInvariant extends TemporalInvariant
    {
        Event mEvent;

        void init(Event n)
        {
            mEvent = n;
        }

        public NeverInvariant(Event neverWhat)
        {
            super();

            init(neverWhat);
        }

        public NeverInvariant(Scope parent, Event neverWhat)
        {
            super(parent);

            init(neverWhat);
        }

        public void processEvent(Event e)
        {
            if (isFalsified) // Implement this check outside to avoid overhead?
                return;

            if (mEvent.matches(e))
                {
                    falsify();
                }
        }

        // FIXME: Factor out common produceDuplicate code into a parent helper
        // method (eg doCommonProduceDuplicateWork)
        EventReceptor produceDuplicate()
        {
            NeverInvariant inv = new NeverInvariant(mEvent);

            inv.numConfirmingSequences = numConfirmingSequences;
            inv.isFalsified = isFalsified;

            setTiedTo(inv);

            return inv;
        }

        String getDescriptionString()
        {
            return mEvent.toString();
        }
    }

    //This class captures the invariant "event foo is present in every sample in scope"
    public static class AlwaysInvariant extends TemporalInvariant
    {
        Event mEvent;

        void init(Event e)
        {
            mEvent = e;
        }

        public AlwaysInvariant(Event aW)
        {
            super();

            init(aW);
        }

        public AlwaysInvariant(Scope parent, Event aW)
        {
            super(parent);

            init(aW);
        }

        public void processEvent(Event e)
        {
            if (isFalsified)
                return;

            if (mEvent.sharesTypeWith(e) && !mEvent.matches(e))
                {
                    falsify();
                }
        }

        EventReceptor produceDuplicate()
        {
            AlwaysInvariant inv = new AlwaysInvariant(mEvent);

            inv.numConfirmingSequences = numConfirmingSequences;
            inv.isFalsified = isFalsified;

            setTiedTo(inv);

            return inv;
        }

        String getDescriptionString()
        {
            return mEvent.toString();
        }
    }

    //This invariant captures the invariant "event foo happens at least once in scope"
    public static class EventuallyInvariant extends TemporalInvariant
    {
        Event mEvent;
        public boolean happenedOnceInScope; //Has the event happened once since our parent
	//scope was last entered?

        void init(Event e)
        {
            mEvent = e;
            happenedOnceInScope = true;
        }

        public EventuallyInvariant(Event eW)
        {
            super();

            init(eW);
        }

        public EventuallyInvariant(Scope parent, Event eventuallyWhat)
        {
            super(parent);

            init(eventuallyWhat);
        }

        public void processEvent(Event e)
        {
            if (isFalsified)
                return;

            if (mEvent.matches(e))
                {
                    happenedOnceInScope = true;
                }
        }

        void parentScopeEntering()
        {
            happenedOnceInScope = false;
        }

        void parentScopeExiting()
        {
            if (!happenedOnceInScope)
                {
                    falsify();
                }
            else
                {
                    numConfirmingSequences += 1;
                }

            happenedOnceInScope = false;
        }

        EventReceptor produceDuplicate()
        {
            EventuallyInvariant inv = new EventuallyInvariant(mEvent);

            inv.numConfirmingSequences = numConfirmingSequences;
            inv.isFalsified = isFalsified;

            setTiedTo(inv);

            return inv;
        }

        String getDescriptionString()
        {
            return mEvent.toString();
        }
    }


}
