package daikon.temporal;

/**
 * This class represents a primitive event for detection purposes. Note that
 * it is uncoupled to daikon, as the detector could be applied to other
 * domains.
 *
 * FIXME: This documentation should probably be merged with/added to the
 * stuff in TemporalInvariantManager.
 *
 * In practice, two types of events are created: "sample" events and "Basic"
 * events. Sample events are records of everything that has happened at
 * a program point, while basic events are the individual subevents that the
 * sample is composed of. This is done to allow proper semantics for handling
 * multiple events at once.
 *
 * Each event A must provide two basic bits of functionality: it must
 * be able to determine if another event B shares its type (two events A and B
 * share type if invariants involving event A would be interested in the
 * occurrence (or absence) of event B) and it must be able to determine
 * if B matches A (if they are compatible events -- that is, either they are
 * the same basic event or one is a sample event which includes as its part
 * the other event). FIXME: Make this clearer.
 *
 **/

public abstract class Event
{
    public boolean sharesTypeWith(Event e)
    {
	return (forwardSharesTypeWith(e) || e.forwardSharesTypeWith(this));
    }

    public boolean matches(Event e)
    {
	return sharesTypeWith(e) && (forwardMatches(e) || e.forwardMatches(this));
    }

    public abstract boolean forwardSharesTypeWith(Event e);
    public abstract boolean forwardMatches(Event e);

    public String toString()
    {
	return this.getClass().toString();
    }
}
