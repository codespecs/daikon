package daikon;


public class VarNames {
  // Do I want to intern the results of these functions before returning
  // them?  Possibly...

  // Do not instantiate this class.
  private VarNames() {
  }

  static String indexVar(String base, int dim) {
    return (base + "-index" + dim);
  }

  static String elementVar(String base, int dim) {
    StringBuffer sb = new StringBuffer(base);
    for (int i=0; i<dim; i++)
      sb.append("-element");
    return sb.toString();
  }
}
