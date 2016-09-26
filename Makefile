all:	up.rebarlx clean up rebar relx run_console_mod

up.rebarlx:
	chmod +x ./rebar
	chmod +x ./relx

up:
	./rebar get-deps

rebar:
	./rebar compile

relx:
	./relx

run_console_mod:
	_rel/echo_tcp_server/bin/echo_tcp_server console

run_demon_mod:
	_rel/echo_tcp_server/bin/echo_tcp_server start

clean:
	./rebar clean
	clear
