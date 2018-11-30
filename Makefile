all:
	rebar3 compile
debug:
	rebar3 as test shell
tar:
	rebar3 as prod release
	rebar3 as prod tar
run:
	rebar3 tar
	/Users/wenewzhang/Documents/workspace/trader/zentrading/_build/default/rel/zentrading/bin/zentrading console

go:
	/Users/wenewzhang/Documents/workspace/trader/zentrading/_build/default/rel/zentrading/bin/zentrading console
update:
	scp /Users/wenewzhang/Documents/workspace/qp/qp_zp/_build/prod/rel/qp_zp-0.0.3.tar.gz root@139.199.167.133:/root/hnzp/.
