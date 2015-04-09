class Point
{
  int x , y;

  Point(int x, int y)
  {
    this.x = x;
    this.y = y;
  }
}

class ColoredPoint extends Point
{
  static final int WHITE = 0 , BLACK = 1;

  int color ;

  ColoredPoint(int x, int y)
  {
    this(x, y, WHITE);
  }

  ColoredPoint(int x, int y, int color)
  {
    super(x, y);
    this.color = color;
  }
}

class Outer
{
  class Inner
  {
  }
}

class ChildOfInner extends Outer.Inner
{
  ChildOfInner()
  {
    (new Outer()).super();
  }
}
