
.SUFFIXES:

OPT = -s -Wall

all: difftbl cluster den denxview clgroup

difftbl: difftbl.c
	gcc $(OPT) -o difftbl difftbl.c -lm

cluster: cluster.c
	gcc $(OPT) -o cluster cluster.c

den: den.c
	gcc $(OPT) -o den den.c -lm

denxview: denxview.c
	gcc $(OPT) -o denxview denxview.c -lm -lX11

clgroup: clgroup.c
	gcc $(OPT) -o clgroup clgroup.c
