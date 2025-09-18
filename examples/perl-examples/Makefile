JAVA = java -Xmx3600m -XX:MaxPermSize=768M
# DAIKON = ${JAVA} daikon.Daikon --no_text_output --show_progress --suppress_redundant
DAIKON = ${JAVA} daikon.Daikon --no_text_output --show_progress

.PHONY: all
all: birthday.inv standalone.inv

.PHONY: clean
clean:
	rm -rf birthday.inv standalone.inv daikon-untyped daikon-instrumented daikon-output

birthday.inv: daikon-output/Birthday.decls daikon-output/test_bday-combined.dtrace
	${DAIKON} $^ -o $@

standalone.inv: daikon-output/standalone-main.decls daikon-output/standalone-combined.dtrace
	${DAIKON} $^ -o $@


daikon-output/test_bday-combined.dtrace: daikon-instrumented/Birthday.pm test_bday.pl
	dtrace-perl test_bday.pl 30

daikon-output/standalone-combined.dtrace: daikon-instrumented/standalone.pl
	dtrace-perl $<

daikon-output/Birthday.decls daikon-instrumented/Birthday.pm: daikon-instrumented/Birthday.types Birthday.pm
	dfepl -T Birthday.pm

daikon-output/standalone-main.decls daikon-instrumented/standalone.pl: daikon-instrumented/standalone-main.types standalone.pl
	dfepl -T standalone.pl

daikon-instrumented/Birthday.types: daikon-untyped/Birthday.pm test_bday.pl
	dtype-perl test_bday.pl 5

daikon-instrumented/standalone-main.types: daikon-untyped/standalone.pl
	dtype-perl $<

daikon-untyped/Birthday.pm: Birthday.pm Birthday.accessors
	dfepl -A Birthday.pm

daikon-untyped/standalone.pl: standalone.pl
	dfepl $<
