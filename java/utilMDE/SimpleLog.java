package utilMDE;

public final class SimpleLog {

  public String indent_str = "";
  public boolean enabled;

  public SimpleLog (boolean enabled) {
    this.enabled = enabled;
  }

  public SimpleLog() {
    this (true);
  }

  public final boolean enabled() {
    return enabled;
  }

  public final void indent() {
    indent_str += "  ";
  }

  public final void exdent() {
    indent_str = indent_str.substring (0, indent_str.length()-2);
  }

  public final void log (String format, Object... args) {

    if (enabled) {
      System.out.print (indent_str);
      System.out.printf (format, args);
    }
  }
}
