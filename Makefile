all: deps app

deps:
	rebar get-deps

app:
	rebar compile

tests:
	rebar eunit

clean:
	rebar clean

distclean: clean
	rebar delete-deps
	
release:
	rebar generate
	
shell:
	erl -pa ebin -pa deps/*/ebin -config aws_events.config
	
run:
	erl -pa ebin -pa deps/*/ebin -config aws_events.config -s aws_events

.PHONY: all deps app tests clean distclean release shell run