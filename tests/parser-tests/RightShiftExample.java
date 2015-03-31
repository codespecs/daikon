public class RightShiftExample
{
  public static int signed_right_shift(int x, int y)
  {
    return x >> y;
  }

  public static void main(String[] args)
  {
    int int0 = signed_right_shift(4, 2);
    int int1 = signed_right_shift(7, 1);
    int int2 = signed_right_shift(12, 1);
    int int3 = signed_right_shift(24, 1);
  }
}
