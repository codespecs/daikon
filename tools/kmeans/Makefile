OPTS = -O2 -ggdb
CC = g++

OBJS = kmeans.o point.o cluster.o main.o

all: kmeans

clean:
	rm -f $(OBJS) kmeans

point.o: point.cpp point.h
	$(CC) -c point.cpp $(OPTS)

kmeans.o: kmeans.cpp kmeans.h
	$(CC) -c kmeans.cpp $(OPTS)

cluster.o: cluster.cpp cluster.h
	$(CC) -c cluster.cpp $(OPTS)

main.o: main.cpp
	$(CC) -c main.cpp $(OPTS)

kmeans: $(OBJS)
	$(CC) -o kmeans $(OBJS) $(OPTS)
