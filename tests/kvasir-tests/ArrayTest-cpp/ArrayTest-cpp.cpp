#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Tests the following things:
// arrays, arrays within structs, global arrays, multi-dimensional arrays, typedef

struct _buffers {
  int age;
  char middleName[15];
  double weight; // This DOESN'T DISPLAY properly for populationPtr->weight, but change it to float and it'll all work fine
  char* mallocMePlease;
  char lastName[50];
  unsigned long SS;
  char firstName[10];
};

typedef struct _buffers buffer;

typedef struct {
  int social_security[9];
  int MIT_id[9];
  int birthday[3];
} array_struct;

buffer population[5] = {{51, "world1", 6.1, 0, "baby1", 71, "hello1"},
			{52, "world2", 6.2, 0, "baby2", 72, "hello2"},
			{53, "world3", 6.3, 0, "baby3", 73, "hello3"},
			{54, "world4", 6.4, 0, "baby4", 74, "hello4"},
			{55, "world5", 6.5, 0, "baby5", 75, "hello5"}};

int globalInt;
static int* globalIntPtr;

buffer* populationPtr;

int basicIntArray[10] = {10, 9, 8, 7, 6, 5, 4, 3, 2, 1};

int multiDimensional[4][5][12];

static array_struct abracadabra = {{1, 1, 2, 3, 5, 8, 13, 21, 35},
				   {5, 4, 6, 3, 7, 2, 8, 1, 9},
				   {1983, 10, 30}};

// Time to get tricky
typedef struct {
  int foo_head;
  struct {
    int bar_head;
    struct {
      int baz_head;
      int y;
      int baz_tail;
    } C;
    int bar_tail;
    struct {
      int neverSeeMe;
    } D[3];
  } B[3];
  int foo_tail;
} A;

A eightA[8] = { {1, {{20, {300, 4000, 50000}, 600000, {{1}, {1}, {1}}}, {-4, {-6, -8, -10}, -12, {{1}, {1}, {1}}}, {6, {9, 12, 15}, 18, {{1}, {1}, {1}}}}, 7},
		{11,{{22, {33, 44, 55}, 66}, {44, {66, 88, 100}, 1200}, {66, {99, 1200, 1500}, 1800}}, 77},
		{111,{{222, {333, 444, 555}, 666}, {44, {66, 888, 1000}, 12000}, {666, {999, 12000, 15000}, 18000}}, 777},
		{1111,{{2222, {3333, 4444, 5555}, 6666}, {44, {66, 8888, 10000}, 120000}, {6666, {9999, 120000, 150000}, 180000}}, 7777},
		{11111,{{22222, {33333, 44444, 55555}, 66666}, {44, {66, 88888, 100000}, 1200000}, {66666, {99999, 1200000, 1500000}, 1800000}}, 77777},
		{111111,{{222222, {333333, 444444, 555555}, 666666}, {44, {66, 888888, 1000000}, 1200000}, {666666, {999999, 1200000, 1500000}, 1800000}}, 777777},
		{1111111,{{2222222, {3333333, 4444444, 5555555}, 6666666}, {44, {66, 8888888, 10000000}, 12000000}, {6666666, {9999999, 12000000, 15000000}, 18000000}}, 7777777},
		{11111111,{{22222222, {33333333, 44444444, 55555555}, 66666666}, {44, {66, 88888888, 100000000}, 120000000}, {66666666, {99999999, 120000000, 150000000}, 180000000}}, 77777777} };

static int returnIntSum(int a, int* b)
{
  static int static_local_array[1000];
  return a + *b;
}

A* structTester(A aStruct, buffer bufferStruct, buffer* bufferStructPtr, int myInt, int* myIntPtr)
{
  return 0;
}

int fooOverload(int over)
{
  static char charBuffer[1000];
  return 100;
}

int fooOverload(short load, char* ed)
{
  static char charBuffer[1000];
  return 1000;
}

int main()
{
  int localArray15[15];
  char localString33[33];
  int localInt = 100;
  buffer* cur_buffer = 0;
  printf("buffer population[999]: %p\n", population);
  printf("buffer multiDimensional[4][5][12]: %p\n", multiDimensional);

  cur_buffer = population;
  strcpy(cur_buffer->firstName, "123456789");
  cur_buffer[3].age = 69;
  strcpy(cur_buffer->middleName, "Ricky");
  cur_buffer->weight = 155;
  strcpy(cur_buffer->lastName, "supercalifragilisticexpialadocious");
  cur_buffer->SS = 12121212;

  cur_buffer[0].mallocMePlease = (char*)strdup("i just got malloc'ed - 00000!\n\n\t");
  cur_buffer[2].mallocMePlease = (char*)strdup("i just got malloc'ed - 22222!\n\n\t");
  cur_buffer[4].mallocMePlease = (char*)strdup("i just got malloc'ed - 44444!\n\n\t");

  printf("%s %d %s %f %s %lu\n",
	 population[0].firstName,
	 population[0].age,
	 population[0].middleName,
	 population[0].weight,
	 population[0].lastName,
	 population[0].SS);

  globalInt = 6170;
  globalIntPtr = (int*)malloc(sizeof(*globalIntPtr));
  *globalIntPtr = 6111;

  populationPtr = (buffer*)malloc(sizeof(*populationPtr));
  strcpy(populationPtr->firstName, "123456789");
  populationPtr->age = 69;
  strcpy(populationPtr->middleName, "Ricky");
  populationPtr->weight = 155.5;
  strcpy(populationPtr->lastName, "supercalifragilisticexpialadocious");
  populationPtr->SS = 12121212;

  printf("%s %d %s %f %s %lu\n",
	 populationPtr[0].firstName,
	 populationPtr[0].age,
	 populationPtr[0].middleName,
	 populationPtr[0].weight,
	 populationPtr[0].lastName,
	 populationPtr[0].SS);

  printf("%p %p %p %p %p %p\n",
	 &(populationPtr[0].firstName),
	 &(populationPtr[0].age),
	 &(populationPtr[0].middleName),
	 &(populationPtr[0].weight),
	 &(populationPtr[0].lastName),
	 &(populationPtr[0].SS));

  // Forces gcc 4.0 to generate a debug info entry for abracadabra:
  printf("abracadabra.social_security[5]: %d\n", abracadabra.social_security[5]);

  returnIntSum(250, &localInt);

  return 0;
}
