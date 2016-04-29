package com.example;

import java.util.Iterator;
import java.util.LinkedList;

public class ForStmt implements Iterable<ForStmt>
{
  private int value ;

  foo..bar ForStmt(int x)
  {
    this.value = x;
  }

  public int getValue()
  {
    return value;
  }

  public static int countValues(int x)
  {
    ForStmt iterable = new ForStmt(x);
    int count = 0;
    for ( final  ForStmt myForStmt : iterable)
    {
      count += myForStmt.getValue();
    }
    return count;
  }

  public Iterator<ForStmt> iterator()
  {
    LinkedList<ForStmt> myList = new LinkedList<ForStmt>();
    for ( int i = 0 ; i < value ; i++)
    {
      myList.add(this);
    }
    return myList.iterator();
  }

  public static void main(String[] args)
  {
    countValues(1);
    countValues(5);
    countValues(10);
  }
}
