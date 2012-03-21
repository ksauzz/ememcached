all:
	@./rebar update-deps
	@./rebar get-deps
	@./rebar compile
	@./rebar xref
	@./rebar eunit

compile:
	@./rebar compile skip_deps=true

xref:
	@./rebar xref skip_deps=true

eunit:
	@./rebar eunit skip_deps=true

clean:
	@./rebar clean skip_deps=true
	@rm -r doc/*

distclean:
	@./rebar delete-deps
	@./rebar clean

qc:
	@./rebar qc skip_deps=true

dialyzer:
	@dialyzer --src src/*.erl

docs:
	@./rebar doc
