
diffs:
	${MAKE} -C decls-file diffs
	${MAKE} -C drawlet diffs

update-goals:
	${MAKE} -C decls-file update-goals
	${MAKE} -C drawlet update-goals

.PHONY: all test diffs update-goals clean
all test: diffs
clean:
	${MAKE} -C decls-file clean
	${MAKE} -C drawlet clean
