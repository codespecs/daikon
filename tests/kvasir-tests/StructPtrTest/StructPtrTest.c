#include <stdio.h>

struct tile {
  int a;
  char* b;
  int c;
};

struct map {
  char* mapName;
  struct tile * tiles;
  int numTiles;
};

int fooTiles(struct tile* t)
{
  return 100;
}

int barMaps(struct map m)
{
  return 300;
}

int bazMapPtr(struct map* m)
{
  return 500;
}

int main()
{
  struct tile localTileArray[5] = {
    {1, "first", 10},
    {2, "second", 20},
    {3, "third", 30},
    {4, "fourth", 40},
    {5, "fifth", 50}};

  struct map localMap = {"tiles map", localTileArray, 5};

  fooTiles(localTileArray);
  fooTiles(localTileArray + 1);
  fooTiles(localTileArray + 2);
  fooTiles(localTileArray + 3);
  fooTiles(localTileArray + 4);

  barMaps(localMap);

  bazMapPtr(&localMap);

  return 0;
}
