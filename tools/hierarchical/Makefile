OPT = -s -Wall
OBJS = difftbl cluster clgroup

all: difftbl cluster clgroup
	cp difftbl cluster clgroup ${INV}/scripts/

difftbl: difftbl.c
	gcc $(OPT) -o difftbl difftbl.c -lm

cluster: cluster.c
	gcc $(OPT) -o cluster cluster.c

clgroup: clgroup.c
	gcc $(OPT) -o clgroup clgroup.c

clean: 
	rm ${OBJS}
