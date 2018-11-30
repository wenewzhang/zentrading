-define(IC_URL,"http://webcffex.hermes.hexun.com/cffex/quotelist?code=").
-define(ZZ500_URL,"http://hq.sinajs.cn/?sn=").

% -define(DEBUG_MSG,1).
-ifdef(DEBUG_MSG).
-define(LOG(X,Y), qp_log:info("{~p,~p} ~p: ~p~n", [?MODULE,?LINE,X,Y])).
-else.
-define(LOG(X,Y), true).
-endif.
-define(LIST_APPEND(L,X,Y),lists:append(L,[{X,Y}])).

-record(event, {type, props, reference = undefined, timestamp}).


-record(future, {
                code,
                name,
                price,
                old_p,
                %%开仓量
                op,
                %%平仓量
                cp,
                %%增仓量
                diff,
                %%持仓量
                kp,
                days

}).

-record(index, {
                code,
                name,
                price,
                old_p

}).

-define(MAX_IC1_POINT_TO_NOTICE,18).
-define(MAX_IC2_POINT_TO_NOTICE,25).
-define(MAX_IC3_POINT_TO_NOTICE,8).
-define(MIN_IC_POINT_TO_NOTICE,0).
-define(IH_POINT_TO_NOTICE,17).
-define(IF_POINT_TO_NOTICE,17).
-define(IC_MONITOR_INTERVAL,6).
-define(IH_MONITOR_INTERVAL,15).
-define(IF_MONITOR_INTERVAL,10).
