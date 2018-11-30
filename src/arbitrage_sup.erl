%% Copyright (c) 2013, Pedram Nimreezi <deadzen@deadzen.com>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

%% @doc Table manager supervisor for all goldrush ets process tables.
%%
%% Manager supervisor responsible for the {@link gr_manager:start_link/3.
%% <em>Manager</em>} processes, which serve as heir of the
%% {@link gr_counter:start_link/0. <em>Counter</em>} and
%% {@link gr_param:start_link/0. <em>Param</em>} ets table processes.
-module(arbitrage_sup).
-behaviour(supervisor).

-type startlink_err() :: {'already_started', pid()} | 'shutdown' | term().
-type startlink_ret() :: {'ok', pid()} | 'ignore' | {'error', startlink_err()}.

-define(CHILD(I, Type,Name), {Name, {I, start_link, [Name]}, permanent, 5000, Type, [I]}).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================
%% @hidden
-spec start_link() -> startlink_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
%% @hidden
-spec init([]) -> {ok, { {one_for_one, 50, 10}, [supervisor:child_spec()]} }.
init(_Args) ->
    % io:format("future_ic_sup~n",[]),
    AM     = ?CHILD(arbitrage_ic_monitor,worker,arbitrage_ic_monitor),
    FutIC1 = ?CHILD(future_ic,worker,future_current_month),
    FutIC2 = ?CHILD(future_ic,worker,future_next_month),
    FutIC3 = ?CHILD(future_ic,worker,future_next_season),
    ZZ500  = ?CHILD(sinajs,worker,stockmarket),

    AMIH     = ?CHILD(arbitrage_ih_monitor,worker,arbitrage_ih_monitor),
    FutIH1 = ?CHILD(future_ic,worker,future_current_month_ih),
    FutIH2 = ?CHILD(future_ic,worker,future_next_month_ih),
    FutIH3 = ?CHILD(future_ic,worker,future_next_season_ih),
    SZ50   = ?CHILD(sinajs,worker,stockmarket_ih),

    AMIF     = ?CHILD(arbitrage_if_monitor,worker,arbitrage_if_monitor),
    FutIF1 = ?CHILD(future_ic,worker,future_current_month_if),
    FutIF2 = ?CHILD(future_ic,worker,future_next_month_if),
    FutIF3 = ?CHILD(future_ic,worker,future_next_season_if),
    SZ300   = ?CHILD(sinajs,worker,stockmarket_if),

    StartArg = [FutIC1,FutIC2,FutIC3,ZZ500,AM,FutIH1,FutIH2,FutIH3,SZ50,AMIH,FutIF1,FutIF2,FutIF3,SZ300,AMIF],
    % StartArg = [FutIC1,FutIC2,FutIC3,ZZ500,AM],
    {ok, { {one_for_all, 50, 10}, StartArg} }.
