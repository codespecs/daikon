/**
 * A single point of the k-means algorithm. Represented by an id/name and 
 * a value for each dimension of the point
 */

#include "point.h"
#include <string>
#include <math.h>
#include <stdlib.h>
#include <stdio.h>

Point::Point(int dim) {
  dimensions = dim;
  data = new float[dimensions];
  clusterNum = 0;
  clusterChanged = 1;
}

Point::~Point () {
}

/**
 * constructor
 */
void Point::init (float *x) {
  //the first value in the array 'x' is the point name/id
  name = (int) x[0];
  for (int i = 1; i < dimensions + 1; i++ ) {
    data[i-1] = x[i];
  }
}

/**
 * set the cluster number of this point to 'number'
 */
void Point::setClusterNum(int number) {
  if(number != clusterNum) {
    clusterNum = number;
    clusterChanged = 1;
  } else {
    clusterChanged = 0;
  }
}

/*
 * set the name of this point to 'nam'
 */
void Point::setName(int nam) {
  name = nam;
}

/**
 * returns a copy of this Point
 */
Point* Point::copy() {
  Point *q = new Point(dimensions);
  q->setName(name);
  for(int j = 0; j < dimensions; j++) {
    q->data[j] = data[j];
  }
  
  q->setClusterNum(clusterNum);
  return q;
}

/**
 * calculate the square of the euclidean distance
 * between this point and 'p'
 */
float Point::distanceSquared(Point *p) {
  if (p->getDimensions() != dimensions) {
    printf("Error: points don't have the same dimension \n");
    exit(0);
  }
  
  float sumsq = 0;
  float *otherdata = p->getData();
  for (int i = 0; i < dimensions; i++) {
    sumsq += pow((data[i] - otherdata[i]), 2);
  }

  return sumsq;
}

void Point::print(void) {
  printf ("name: %i, cluster: %i\n", name, clusterNum);
  for(int i = 0; i < dimensions; i++) {
    printf("%f\n", data[i]);
  }
}
