#include <vector>

class Point;

class Cluster {

  Point *means;

  int dimensions;
  float error;
  int clusterNum;
  int numpoints;
  std::vector<Point*> points;

 public:
  Cluster(int dim, Point *randpoint, int clustNum);
  ~Cluster();

  float distanceSquared(Point *p);

  void refresh(void);

  void addPoint(Point *p);

  void calculateMeans(void);


  void setClusterNum(int num);

  int getClusterNum(void);

  void print (void);

  float calculateError (void);
};
