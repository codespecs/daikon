package daikon.temporal;

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
