OPTS = -O2 -ggdb
CC = g++

OBJS = kmeans.o point.o cluster.o

all: kmeans

clean:
	rm $(OBJS)
	rm main.o
	rm kmeans

point.o: point.cpp point.h
	$(CC) -c point.cpp $(OPTS)

kmeans.o: kmeans.cpp kmeans.h
	$(CC) -c kmeans.cpp $(OPTS)

cluster.o: cluster.cpp cluster.h
	$(CC) -c cluster.cpp $(OPTS)

main.o: main.cpp
	$(CC) -c main.cpp $(OPTS)

kmeans: $(OBJS) main.o
	$(CC) -o kmeans main.o $(OBJS) $(OPTS)
