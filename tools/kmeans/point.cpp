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
void Point::init (float ID, float *x) {
  //the first value in the array 'x' is the point name/id
  id = (int) ID;
  for (int i = 0; i < dimensions; i++ ) {
    data[i] = x[i];
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
 * set the id of this point to 'i'
 */
void Point::setId(int i) {
  id = i;
}

/**
 * returns a copy of this Point
 */
Point* Point::copy() {
  Point *q = new Point(dimensions);
  q->setId(id);
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
  printf ("id: %i, cluster: %i\n", id, clusterNum);
  for(int i = 0; i < dimensions; i++) {
    printf("%f\n", data[i]);
  }
}
