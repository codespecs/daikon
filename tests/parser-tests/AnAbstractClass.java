package com.example;

public abstract class AnAbstractClass
{
  public abstract boolean anAbstractMethod();

  public static int add(int x, int y)
  {
    return x + y;
  }

  public static void main(String[] args)
  {
    add(1, 5);
    add(2, 10);
    add(3, 15);
    add(4, 20);
  }
}
