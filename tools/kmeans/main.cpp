/* main.cpp */
#include <stdlib.h>
#include <math.h>
#include <stdio.h>
#include "kmeans.h"
#include "point.h"

int main(int argc, char *argv[]) {
 
  char* s = argv[1];
  int numClusters = atoi(argv[2]);
    
  KMeans *km = new KMeans ( s );
  km->doCluster(numClusters);
}


/* End of file main1.cpp */
