INV_FILES = p173-14.3.invs p176.invs p177-1.invs p177-14.8.invs p177-14.9.invs p177-2.invs p178-1b.invs p180-15.1.1.invs p184-3.invs p187.invs p191-2.invs
DTRACE_FILES = p173-14.3.dtrace p176.dtrace p177-1.dtrace p177-14.8.dtrace p177-14.9.dtrace p177-2.dtrace p178-1b.dtrace p180-15.1.1.dtrace p184-3.dtrace p187.dtrace p191-2.dtrace

%.invs:	%.dtrace
	java daikon.Daikon gries-instrumented.decls $< > $@

all:	clean $(INV_FILES) all.invs

all.invs: $(DTRACE_FILES)
	java daikon.Daikon gries-instrumented.decls $(DTRACE_FILES) > $@

clean:
	rm -rf $(INV_FILES)

%.diff:	%.invs %.invs-goal


diff:	$(INV_FILES)
	for i in $^; do diff -u $$i-goal $$i; done

new-goal:
	for i in $(INV_FILES); do mv -f $$i-goal $$i-goal.bak; cp -p $$i $$i-goal; done
