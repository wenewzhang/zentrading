-module(zt_misc).
-export([get_env/3,get_env/2,get_env/1]).

get_env(Key) ->
    case application:get_env(zentrading, Key) of
      {ok, Value} -> Value;
      undefined   -> undefined
    end.

get_env(Key, Def) ->
    case application:get_env(zentrading, Key) of
        {ok, Val} -> Val;
        undefined -> Def
    end.

get_env(Application, Key, Def) ->
    case application:get_env(Application, Key) of
        {ok, Val} -> Val;
        undefined -> Def
    end.
