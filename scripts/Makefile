all: difftbl cluster clgroup kmeans trace-untruncate-fast

clean:
	rm -f difftbl cluster clgroup kmeans trace-untruncate-fast

TAGS: tags

tags:
	etags `find . | grep -v '~\|/CVS\|^\.$$\|^\./[kx]means$$'`

HIERDIR = ../tools/hierarchical
KMDIR = ../tools/kmeans

difftbl: $(HIERDIR)/difftbl
	cp -p $^ $@

cluster: $(HIERDIR)/cluster
	cp -p $^ $@

clgroup: $(HIERDIR)/clgroup
	cp -p $^ $@

kmeans: $(KMDIR)/kmeans
	cp -p $^ $@

trace-untruncate-fast: trace-untruncate-fast.c
	gcc -Wall -g $< -o $@

## Delegation to other Makefiles (gross)

$(HIERDIR)/difftbl: $(HIERDIR)/difftbl.c
	$(MAKE) -C $(HIERDIR) difftbl

$(HIERDIR)/cluster: $(HIERDIR)/cluster.c
	$(MAKE) -C $(HIERDIR) cluster

$(HIERDIR)/clgroup: $(HIERDIR)/clgroup.c
	$(MAKE) -C $(HIERDIR) clgroup

$(KMDIR)/cluster: cluster.cpp $(KMDIR)/kmeans.cpp $(KMDIR)/main.cpp $(KMDIR)/point.h $(KMDIR)/cluster.h $(KMDIR)/kmeans.h $(KMDIR)/point.cpp
	$(MAKE) -C $(KMDIR) cluster
