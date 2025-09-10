all default: config.yml
# TODO: all default: config.yml config-daily.yml

%: %.m4 defs.m4 ../.azure/jobs.m4
	m4 $< > $@

clean:
	rm -f config.yml
