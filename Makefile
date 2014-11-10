.PHONY: all deps compile rel run clean

REBAR=$(shell which rebar || echo ./rebar)


all: $(REBAR) deps compile

deps:
		$(REBAR) get-deps

compile:
		$(REBAR) compile

run:
		erl -pa ./ebin ./deps/*/ebin +Bd -sname xfp -config sys.config -boot start_sasl -s sync -run xfp

clean:
		$(REBAR) clean
		rm -rf ./rebar
		rm -rf ./ebin
		rm -rf ./erl_crash.dump

# Get rebar if it doesn't exist

REBAR_URL=https://cloud.github.com/downloads/basho/rebar/rebar

./rebar:
	erl -noinput -noshell -s inets -s ssl \
		-eval '{ok, _} = httpc:request(get, {"${REBAR_URL}", []}, [], [{stream, "${REBAR}"}])' \
		-s init stop
	chmod +x ${REBAR}
