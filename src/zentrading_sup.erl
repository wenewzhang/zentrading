%%%-------------------------------------------------------------------
%% @doc zentrading top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(zentrading_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    FutSup = ?CHILD(arbitrage_sup, supervisor),
    DBSup = ?CHILD(db_sup, supervisor),
    {ok, { {one_for_one, 50, 10}, [FutSup,DBSup]} }.

%%====================================================================
%% Internal functions
%%====================================================================
