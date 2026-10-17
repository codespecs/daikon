
diffs:
	${MAKE} -C drawlet diffs
	${MAKE} -C issue203 diffs

update-goals:
	${MAKE} -C drawlet update-goals
	${MAKE} -C issue203 update-goals

.PHONY: all test diffs update-goals clean
all test: diffs
clean:
	${MAKE} -C drawlet clean
	${MAKE} -C issue203 clean
