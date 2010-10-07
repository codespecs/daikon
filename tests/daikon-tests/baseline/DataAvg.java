// default package

public class DataAvg
{

  private int sum;
  private int count;

  public DataAvg()
  {
    sum = 0;
    count = 0;
  }

  public void insert(int element)
  {
    sum += element;
    count++;
  }


  public int average()
  {
    return sum / count;
  }

}
