#include <string>

class Point {

  int dimensions;
  int clusterNum;
  int clusterChanged;
  float* data;
  int id;

 public:

  Point(int dim);
  ~Point();

  int getClusterNum(void) {
    return clusterNum;
  }

  float* getData (void) {
    return data;
  }

  int getId (void) {
    return id;
  }

  void init(float ID, float *x);

  void setClusterNum(int number);

  void setId(int id);

  Point* copy(void);

  float distanceSquared(Point *p);

  int getDimensions(void) {
    return dimensions;
  }

  void print (void);
};
