package example;

public final class Failure3
{
  public Failure3()
  {
    System.setSecurityManager(new SecurityManager(){
      @Override public final void checkPackageAccess(final String pkg)
      {
        throw new SecurityException("access to package " + pkg + " not allowed"
          );
      }
    });
  }

  public static void main(String[] args) throws Exception
  {
    new Failure3();
  }
}
