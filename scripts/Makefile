all: difftbl cluster clgroup kmeans trace-untruncate-fast

HIERDIR = ../tools/hierarchical
KMDIR = ../tools/kmeans

difftbl: $(HIERDIR)/difftbl.c
	cd $(HIERDIR) && make
	cp -p $(HIERDIR)/$@ $@

cluster: $(HIERDIR)/cluster.c
	cd $(HIERDIR) && make
	cp -p $(HIERDIR)/$@ $@

clgroup: $(HIERDIR)/clgroup.c
	cd $(HIERDIR) && make
	cp -p $(HIERDIR)/$@ $@

kmeans: $(KMDIR)/cluster.cpp $(KMDIR)/kmeans.cpp $(KMDIR)/main.cpp $(KMDIR)/point.h $(KMDIR)/cluster.h $(KMDIR)/kmeans.h $(KMDIR)/point.cpp
	cd $(KMDIR) && make
	cp -p $(KMDIR)/$@ $@

trace-untruncate-fast: trace-untruncate-fast.c
	gcc -Wall -g $< -o $@
