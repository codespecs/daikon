/** 
 * A simple kmeans algorithm implementation. The only input is a filename 's', 
 * where the data points are stored. An example input file is shown below. Note
 * that there are no blank lines (can be easily changed to make more flexible).
 * 
 * number of dimensions
 * point 1 id
 * point 1 dimension 1
 * point 1 dimension 2
 *       .
 *       .
 *       .
 * point 1 dimension N
 * point 2 id
 * point 2 dimension 1
 * point 2 dimention 2
 *       .
 *       .
 *       .
 * 
 */

#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <iostream>
#include "point.h"
#include "kmeans.h"
#include "cluster.h"
#include <time.h>

// Doubles/floats must be initialized in source (not header) file
const float KMeans::convergenceThreshold = 0.00001;

KMeans::KMeans(char* s ) : s(s) { 
  DataFp = NULL;
  srand(seed);
  numIters = 0;
  numPoints = 0;
  
  // Reads the first line of the input file, which denotes the
  // number of dimensions of each point
  if ((DataFp = fopen( s, "r")) == NULL) {
    perror("Unable to open data file ");
    std::cout << s << std::endl;
    exit(1);
  }
  
  fscanf(DataFp, "%i", &dimensions);
  //the first thing in data is the point's unique id or name
  data = new float[dimensions];
  means = new float[dimensions];
  stdev = new float[dimensions];
 
  for(int i = 0; i < dimensions; i++) {
    means[i] = 0;
    stdev[i] = 0;
  }
  
  readPoints();
}

/**
 * Initializes the clusters by choosing random points from the vector 
 * of points for the initial means.
 */
void KMeans::initClusters (void) {
  //choose the random points
  clusters.clear();
  
  int *randNums = new int[numClusters];
  
  for(int i = 0; i < numClusters; i++) {
    int random  = rand()%numPoints;
    for (int j = 0; j < i; j++) {
      if (random == randNums[j]) {
	i--;
	continue;
      }
      randNums[i] = random;
    }
    Point* randpoint = points[random];
    Cluster *c = new Cluster(dimensions, randpoint->copy(), i);
    clusters.push_back(c);
  }
}

/**
 * Reads the points from a text file and initializes Point Objects in the vector
 * 'points'
 */
void KMeans::readPoints (void) {
  
   Point *p;
   
   while ((p = getPoint()) != NULL) {
     numPoints++;
     points.push_back(p);
   }
   if (numPoints == 0) {
     printf("No points in %s\n", s);
     exit(0);
   }
   standardizePoints();
}

/**
 * does the actual reading of the points from the data file
 */
Point* KMeans::getPoint(void) {
  float tmp;
  float ID;

  //the point identifier
  if ((fscanf(DataFp, "%f", &tmp) != 1)) {
    return NULL;
  } else {
    ID = tmp;
  }

  for (int i = 0; i < dimensions; i++) {
    if ((fscanf(DataFp, "%f", &tmp) != 1)) {
      return NULL;
    } else {
      means[i] += tmp;
      data[i] = tmp;
    }
  }

  Point *p = new Point(dimensions);
  p->init(ID, data);
  return p;
}

/**
 * returns the number of points read
 */
int KMeans::getNumPoints(void) {
  return numPoints;
}

/**
 * returns the closest cluster to the Point 'p'.
 * requires that 'p' is not null
 */
int KMeans::closestCluster(Point *p) {
  float lowestDist = -1;
  int closestCluster = 0;

  for (int i = 0; i < numClusters; i++) {
    Cluster *c = clusters[i];
    float dist = c->distanceSquared(p);
    if (lowestDist == -1) {
      lowestDist = dist;
    } else if (dist < lowestDist) {
      lowestDist = dist;
      closestCluster = i;
    }       
  }
  return closestCluster;
}
  


KMeans::~KMeans() {
  for (int i = 0; i < numPoints; i++) {
    delete(points[i]);
  }
}

/**
 * returns the vector containing all the points
 */
std::vector<Point*> KMeans::getPoints(void) {
  return points;
}

/**
 * Perform one iteration of the kmeans algorithm. 
 */
void KMeans::iterate (void) {
  numChanges = 0; //the number of cluster membership changes in this iteration
  
  for(int i = 0; i < numClusters; i++) {
    clusters[i]->refresh();
  }
  
  for (int i = 0; i < numPoints; i++) {
    Point *curpoint = points[i];
    int clusterNum = closestCluster(curpoint);
    if(curpoint->getClusterNum() != clusterNum) {
      numChanges++;
      curpoint->setClusterNum(clusterNum);
    }
    clusters[clusterNum]->addPoint(curpoint);
  }
  
  for(int i = 0; i < numClusters; i++) {
    clusters[i]->calculateMeans();
  }
}

/**
 * returns the number of changes of cluster membership in the most current
 * iteration
 */
int KMeans::getNumChanges(void) {
  return numChanges;
}

/**
 * prints all the points out, with their cluster information
 */
void KMeans::outputPoints (void) {
  for (int i = 0; i < numPoints; i++) {
    points[i]->print();
  }
}

/**
 * perform a k-clustering of the points.
 */
void KMeans::doCluster(int k) {
  
  numClusters = k;
  float minError = -1;
  int *clusterInfo = new int[numPoints];
  
  if (numPoints <= numClusters)
    numClusters = numPoints;

  //perform numKMeans operations of KMeans and return the optimal one
  for(int j = 0; j < numKMeans; j++) {
    
    initClusters();
    
    //each kmeans iteration should be performed maxIters times or until
    //it converges. Convergence results when there is no change in cluster
    //membership or if the change in error between the current and the 
    //previous iteration was less than convergenceThreshold
    
    float prevError = -1; //the error sum of squares in the previous iteration
    for (numIters = 0; numIters < maxIters; numIters++) {
	iterate();
	
	if (prevError == -1) 
	  prevError = sumErrors();
	else if ( getNumChanges() == 0 || 
		  prevError - sumErrors() < convergenceThreshold) {
	  break;
	}
    } 
    
    //store the cluster info of the best clustering so far
    float sum = sumErrors();
    
    //printf("number of changes is %i\n", getNumChanges());
    //printf("number of iterations is %i\n", numIters);
    
    if (minError == -1 || sum < minError) {
      minError = sum;
      for(int i = 0; i < numPoints; i++) {
	clusterInfo[i] = points[i]->getClusterNum();
      }
    }
  }
  
  //now reconstruct the best clustering
  for (int i = 0; i < numClusters; i++) {
    clusters[i]->refresh();
  }
  
  for( int i = 0; i < numPoints; i++) {
    clusters[clusterInfo[i]]->addPoint(points[i]);
  }
  
  for( int i = 0; i < numClusters; i++) {
    clusters[i]->print();
    printf ("\n");
  }
}
  
/**
 * calculate the total sum of square errors for this clustering
 */
float KMeans::sumErrors(void) {
  float sumError = 0;
  for (int i = 0;  i < numClusters; i++) 
    sumError += clusters[i]->calculateError(); 
  return sumError;
}

/* returns the absolute value of a number */
float KMeans::absolute(float x) {
  if (x < 0 )
    return -x;
  else 
    return x;
}

/**
 * perform a standardization of the points 
 */
void KMeans::standardizePoints() {
  
  //calculate the means
  for (int i = 0; i < dimensions; i++) {
    means[i] = means[i]/numPoints;
  }
  
  //calculate the standard deviation (actually the mean absolute
  //deviation which is sum(Xi - Mi)/n
  float *tmpdata;
  for(int i = 0; i < numPoints; i++) {
    tmpdata = points[i]->getData();
    for (int j = 0; j < dimensions; j++) {
      stdev[j] += absolute(tmpdata[j] - means[j]); //mean absolute deviation
      /* stdev[j] += pow((tmpdata[j] - means[j]), 2); //standard deviation */
    }
  }

  //calculate the deviation
  for (int i = 0; i < dimensions; i++) {
    stdev[i] = stdev[i]/numPoints; //mean absolute deviation
    /* stdev[i] = pow(stdev[i], 0.5)/(numPoints - 1); //standard deviation */
  }

  //now standardize the data
  for(int i = 0; i < numPoints; i++) {
    tmpdata = points[i]->getData();
    for(int j = 0; j < dimensions; j++) {
      if (stdev[j] == 0) {
	//no variation in this dimension. zero out.
	tmpdata[j] = 0;
      } else {
	tmpdata[j]  = (tmpdata[j] - means[j]) / stdev[j];
      }
    }
  }

  
}
/* End of file kmeans.cpp */
