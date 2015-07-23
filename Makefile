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
	
shell:
	erl -pa ebin -pa deps/*/ebin -config aws_events.config -s aws_events

.PHONY: all deps app tests clean distclean