/**
 * A cluster of points in the Kmeans algorithm. Represented by its
 * mean and the number of points in it.  
 */

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include "point.h"
#include "cluster.h"

//create a new cluster
Cluster::Cluster (int dim, Point *randpoint, int num) {
  dimensions = dim; 
  means = randpoint;
  clusterNum = num;
}

/**
 * requires: p is not null
 * effects:  returns the square of the Euclidean distance between the cluster mean and 
 *           the point p.  
 */
float Cluster::distanceSquared(Point *p) {
  
  if (p->getDimensions() != dimensions) {
    printf("Error: point don't have the same dimension as cluster\n");
    exit(0);
    }
  
  float sumsq = 0;
  error = 0;
  float *data = means->getData();
  float *otherdata = p->getData();
  for (int i = 0; i < dimensions; i++) {
    sumsq += pow((data[i] - otherdata[i]), 2);
  }
  return sumsq;
}

/**
 * effects: adds the point p to this cluster.
 */ 
void Cluster::addPoint (Point *p) {

  numpoints ++;
  points.push_back(p);
  
}

/**
 * effects: removes all points from this cluster
 */
void Cluster::refresh(void) {
  numpoints = 0;
  points.clear();
}

/**
 * calculates the new mean of this cluster
 */
void Cluster::calculateMeans(void) {
  
  //refresh the mean
  float *meandata = means->getData();
  for (int i = 0; i < dimensions; i++) {
    meandata[i] = 0;
  }
  
  for (int i = 0; i < numpoints; i++) {
    float *tempdata = points[i]->getData();
    for(int j = 0; j < dimensions; j++) {
      meandata[j] += tempdata[j];
    }
  }
  for (int i = 0; i < dimensions; i++) {
    meandata[i]  = meandata[i]/numpoints;
  }
}

/**
 * Return the id of this cluster
 */
int Cluster::getClusterNum (void) {
  return clusterNum;
}

void Cluster::setClusterNum (int num) {
  clusterNum = num;
}

/**
 * calculate the sum of the square errors of this cluster, which is
 * the sum of the square of the distances of each point from the mean 
 */
float Cluster::calculateError (void) {
  error = 0;
  for (int i = 0; i < numpoints; i++) {
    error +=  means->distanceSquared(points[i]);
  }
  return error;
}

void Cluster::print(void) {
  float *data = means->getData();
  for (int i = 0; i < numpoints; i++) {
    printf("%i\n", points[i]->getId());
  }
}

