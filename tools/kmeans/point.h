#include <string>

class Point {
  
  int dimensions;
  int clusterNum;
  int clusterChanged;
  float* data;
  int name;
  
 public:
  
  Point(int dim);
  ~Point();
  
  int getClusterNum(void) {
    return clusterNum;
  }

  float* getData (void) {
    return data;
  }

  int getName (void) {
    return name;
  }
  
  void init(float *x);
  
  void setClusterNum(int number);
  
  void setName(int name);
  
  Point* copy(void);
  
  float distanceSquared(Point *p);

  int getDimensions(void) {
    return dimensions;
  }
  
  void print (void);
};
