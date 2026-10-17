
diffs:
	${MAKE} -C drawlet diffs

update-goals:
	${MAKE} -C drawlet update-goals

.PHONY: all test diffs update-goals clean
all test: diffs
clean:
	${MAKE} -C drawlet clean
