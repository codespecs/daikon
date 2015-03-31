package com.example;

public class InnerStatic
{
  private int value ;

  public InnerStatic(int val)
  {
    this.value = val;
  }

  public static class MyInnerStatic
  {
    public MyInnerStatic()
    {
      super();
    }

    public MyInnerStatic(Object value)
    {
      super();
    }
  }

  public static void main(String[] args)
  {
    InnerStatic container = new InnerStatic(10);
    MyInnerStatic innerClass = new MyInnerStatic();
  }
}
