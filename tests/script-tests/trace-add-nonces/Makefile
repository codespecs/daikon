TRACE_ADD_NONCES := trace-add-nonces.pl
TESTS := test1 test2 test3 test4
OUTPUT := $(addsuffix .nonce.dtrace,${TESTS})
DIFF := $(addsuffix .diff,${OUTPUT})

default: summary

.PRECIOUS: ${OUTPUT}
output: ${OUTPUT}
${OUTPUT}: %.nonce.dtrace: %.dtrace
	${TRACE_ADD_NONCES} < $< > $@

.PRECIOUS: ${DIFF}
diff: ${DIFF}
${DIFF}: %.diff: %.goal %
	diff -u $^ > $@ || true

.PHONY: summary
summary: ${DIFF}
	@for file in $(subst .diff,,${DIFF}); do \
	  if [ -s $$file.diff ]; then \
	    echo "FAILED  $$file"; \
	  else \
	    echo "OK      $$file"; \
	  fi; \
	done

.PHONY: clean
clean:
	rm -f ${OUTPUT} ${DIFF}
