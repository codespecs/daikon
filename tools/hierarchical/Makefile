.SUFFIXES:

OPT = -s -Wall
XOPT = -L/usr/X11/lib

# all: difftbl cluster den denxview clgroup
all: difftbl cluster den clgroup

clean:
	rm -f difftbl cluster den clgroup

difftbl: difftbl.c
	gcc $(OPT) -o difftbl difftbl.c -lm

cluster: cluster.c
	gcc $(OPT) -o cluster cluster.c -lm

den: den.c
	gcc $(OPT) -o den den.c -lm

denxview: denxview.c
	gcc $(OPT) $(XOPT) -o denxview denxview.c -lm -lX11

clgroup: clgroup.c
	gcc $(OPT) -o clgroup clgroup.c
