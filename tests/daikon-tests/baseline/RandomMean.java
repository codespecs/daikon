// default package

public class RandomMean
{
  public static void main() {
    compute();
  }

  public static void compute() {
    DataAvg accum = new DataAvg();
    int seed = 2001;
    for (int i=0; i<1000; i++) {
      seed = seed * 17 * 107;
      int num = seed & 0x08FF;
      num -= 0x0400;
      accum.insert(num);
    }
    int mean = accum.average();
    System.out.println("1000 trials over range +/- 1024 average = " + mean);
  }

}
