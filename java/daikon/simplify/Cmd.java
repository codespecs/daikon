package daikon.simplify;

public interface Cmd
{
  /**
   * Runs the command in the given session.
   **/
  public void apply(Session s);
}
