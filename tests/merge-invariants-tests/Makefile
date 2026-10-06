
diffs:
	${MAKE} -C drawlet diffs
	${MAKE} -C issue203 diffs

update-goals:
	${MAKE} -C drawlet update-goals
	${MAKE} -C issue203 update-goals

.PHONY: all test clean
all:
	${MAKE} -C drawlet all
test:
	${MAKE} -C drawlet test
clean:
	${MAKE} -C drawlet clean
	${MAKE} -C issue203 clean
