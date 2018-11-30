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


%% @doc Process table manager for goldrush.
%%
%% Manager responsible for the processes, which serve as heir of the
%% {@link gr_counter:start_link/0. <em>Counter</em>} and
%% {@link gr_param:start_link/0. <em>Param</em>} ets table processes.
%% This process creates the table and initial data then assigns itself
%% to inherit the ets table if any process responsible for it is killed.
%% It then waits to give it back while that process is recreated by its
%% supervisor.
-module(future_ic).
-behaviour(gen_server).
-include("zt.hrl").
%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).


%%%===================================================================
%%% API
%%%===================================================================

%% Setup the initial data for the ets table
% -spec setup(atom() | pid(), term()) -> ok.
% setup(Name, Data) ->
%     gen_server:cast(Name, {setup, Data}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(Name, Managee, Data) -> {ok, Pid} | ignore |
%%                                          {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Name) ->
    % io:format("future_ic here!~p~n",[Name]),
    gen_server:start_link({local, Name}, ?MODULE, [Name], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([_Name]) ->
    process_flag(trap_exit, true),
    % io:format("coding here!~p~n",[Name]),
    % {State} = check_the_last_data(Name,#future{}),
    {ok, #future{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = {error, unhandled_message},
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
% handle_cast({setup, Data}, State = #state{managee=Managee}) ->
%     ManageePid = whereis(Managee),
%     link(ManageePid),
%     TableId = ets:new(?MODULE, [set, private]),
%     ets:insert(TableId, Data),
%     ets:setopts(TableId, {heir, self(), Data}),
%     ets:give_away(TableId, ManageePid, Data),
%     {noreply, State#state{table_id=TableId}};

handle_cast({get_the_last_data,IC,Pid}, Future) when is_list(IC) ->
    {FutureN} = check_the_last_data(IC,Future),
    gen_server:cast(Pid,{return_last_data,FutureN}),
    {noreply, FutureN};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%% @doc Wait for a registered process to be associated to a process identifier.
%% @spec wait_for_pid(Managee) -> ManageePid
% -spec wait_for_pid(atom()) -> pid().
% wait_for_pid(Managee) when is_pid(Managee) ->
%     Managee;
% wait_for_pid(Managee) when is_atom(Managee), Managee =/= undefined ->
%     case whereis(Managee) of
%         undefined ->
%             timer:sleep(1),
%             wait_for_pid(Managee);
%         ManageePid -> ManageePid
%     end.



%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

check_the_last_data(Code,State) ->
  Url = build_access_url(Code),
  % qp_log:info("check url:~p~n",[Url]),
  case httpc:request(get,{Url,[]},[{timeout,5000},{connect_timeout,5000}],[]) of
  {ok,{_,_,HttpData}} ->
    % {FirstJD} = JsonData,
    % {JsonData} = jiffy:decode(HttpData),
    ReData = re:split(HttpData,",",[{return,list}]),
    Price  = lists:nth(3,ReData),
    ?LOG(httpc_request_user_success,[Price]),
    {State#future{code = Code,price = list_to_integer(Price)}};
  {error, Reasonx} ->
    qp_log:error("check_the_last_data error:~p URL:~p~n",[Reasonx,Url]),
    {State#future{code = Code,price = 0}}
end.
%%%===================================================================
%%% Internal functions
%%%===================================================================
build_access_url(Name) when Name =:= future_current_month ->
  ?IC_URL ++ "CFFEXIC1705" ++ ",&column=Code,Name,Price,OpenPosition,ClosePosition,PositionDiff,OpenInterest";
build_access_url(Name) when Name =:= future_next_month ->
  ?IC_URL ++ "CFFEXIC1706" ++ ",&column=Code,Name,Price,OpenPosition,ClosePosition,PositionDiff,OpenInterest";
build_access_url(Name)  ->
  ?IC_URL ++ Name ++ ",&column=Code,Name,Price,OpenPosition,ClosePosition,PositionDiff,OpenInterest".
