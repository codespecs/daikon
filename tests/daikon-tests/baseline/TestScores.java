// default package

public class TestScores
{

  public static void main(String[] args) {
    six170();
    six821();
  }

  private static int classAve(int[] scores) {
    DataAvg avg = new DataAvg();
    for (int i=0; i<scores.length; i++) {
      avg.insert(scores[i]);
    }
    int result = avg.average();
    return result;
  }

  public static void six170() {
    int[][] six170 = {
      {76, 88, 53, 12, 100, 96, 98, 72, 26, 82, 82, 76, 50},
      {98, 72, 26, 82, 82, 76, 50, 76, 88, 53, 12, 100},
      {12, 100, 98, 72, 26, 82, 82, 76, 50, 76},
      {82, 76, 50, 76, 12, 100, 98, 72, 26}
    };

    for (int i=0; i<six170.length; i++) {
      int ave = classAve(six170[i]);
      System.out.println("Class ave for 6170:" + i + " = " + ave);
    }
  }

  public static void six821() {
    int[][] six821 = {
      {76, 99, 80, 97, 100, 20, 92, 94},
      {20, 92, 94, 76, 99, 80, 97},
      {80, 97, 20, 92, 94, 76},
      {20, 92, 99, 80, 97}
    };

    for (int i=0; i<six821.length; i++) {
      int ave = classAve(six821[i]);
      System.out.println("Class ave for 6821:" + i + " = " + ave);
    }
  }

  public static void six893() {
    int[][] six893 = {
      {100, 100, 100, 100, 100},
      {100, 100, 100, 100, 100},
      {100, 100, 100, 100, 100},
    };

    for (int i=0; i<six893.length; i++) {
      int ave = classAve(six893[i]);
      System.out.println("Class ave for 6893:A" + i + " = " + ave);
    }

    six893 = new int[][] {
      {60, 70, 80, 90, 100},
      {70, 80, 90, 100, 60},
      {90, 100, 60, 70, 80}
    };

    for (int i=0; i<six893.length; i++) {
      int ave = classAve(six893[i]);
      System.out.println("Class ave for 6893:B" + i + " = " + ave);
    }
  }

}
