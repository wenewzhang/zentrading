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
-module(arbitrage_ic_monitor).
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

-record(state, {
                fic1,
                fic2,
                fic3,
                index,
                tm,
                count

}).

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
    gen_server:start_link({local, Name}, ?MODULE, [], []).

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
init([]) ->
    process_flag(trap_exit, true),
    % io:format("coding here!~p~n",[Name]),
    Fic1   = #future{code = "CFFEXIC1705",name = "IC1705",price = 0,old_p = 0,days = 1},
    Fic2   = #future{code = "CFFEXIC1706",name = "IC1706",price = 0,old_p = 0,days = 2},
    Fic3   = #future{code = "CFFEXIC1709",name = "IC1709",price = 0,old_p = 0,days = 5},
    Index  = #index{code = "sh000905",price = 0,old_p = 0},
    State0 = #state{fic1 = Fic1,fic2 = Fic2,fic3 = Fic3,index = Index,count = 0},
    State1 = qp_event:init_stats_timer(State0, #state.tm,?IC_MONITOR_INTERVAL*1000),
    State  = qp_event:ensure_stats_timer(State1, #state.tm, monitor_timer),
    % {State} = check_the_last_data(Name,),
    {ok, State}.

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


handle_cast({return_last_data,#future{code = Code,price = Price}},
                            #state{fic1 = #future{price = OldPrice} = Fic1,count = Count} = State) when Code =:= "CFFEXIC1705" ->
    io:format("股指期货:~p 现：~p 原：~p ~n",[Code,Price,OldPrice]),
    if Price > 0 ->
      {noreply, one_round_finished(State#state{fic1 = Fic1#future{old_p = OldPrice,price = Price},count = Count + 1})};
    true ->
      {noreply, one_round_finished(State#state{count = Count + 1})}
    end;

handle_cast({return_last_data,#future{code = Code,price = Price}},
                            #state{fic2 = #future{price = OldPrice} = Fic2,count = Count} = State) when Code =:= "CFFEXIC1706" ->
    io:format("股指期货:~p 现：~p 原：~p ~n",[Code,Price,OldPrice]),
    if Price > 0 ->
      {noreply, one_round_finished(State#state{fic2 = Fic2#future{old_p = OldPrice,price = Price},count = Count + 1})};
    true ->
      {noreply, one_round_finished(State#state{count = Count + 1})}
    end;

handle_cast({return_last_data,#future{code = Code,price = Price}},
                            #state{fic3 = #future{price = OldPrice} = Fic3,count = Count} = State) when Code =:= "CFFEXIC1709" ->
    io:format("股指期货:~p 现：~p 原：~p ~n",[Code,Price,OldPrice]),
    if Price > 0 ->
      {noreply, one_round_finished(State#state{fic3 = Fic3#future{old_p = OldPrice,price = Price},count = Count + 1})};
    true ->
      {noreply, one_round_finished(State#state{count = Count + 1})}
    end;
handle_cast({return_last_index,#index{code = Code,price = Price}},
                            #state{index = #index{price = OldPrice} = Index,count = Count} = State) ->
    io:format("~n中证500指数:~p 现：~p 原：~p ~n",[Code,Price,OldPrice]),
    if Price > 0 ->
      {noreply, one_round_finished(State#state{index = Index#index{price = Price,old_p = OldPrice},count = Count + 1})};
    true ->
      {noreply, one_round_finished(State#state{count = Count + 1})}
    end;
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
handle_info(monitor_timer, #state{fic1  = #future{code = Code1},
                                  fic2  = #future{code = Code2},
                                  fic3 = #future{code = Code3},
                                  index = #index{code = Index}} = State) ->
    % qp_log:info("monitor_timer run"),
    gen_server:cast(future_current_month,{get_the_last_data,Code1,self()}),
    gen_server:cast(future_current_month,{get_the_last_data,Code2,self()}),
    gen_server:cast(future_next_season,{get_the_last_data,Code3,self()}),
    gen_server:cast(stockmarket,{get_the_last_data,Index,self()}),
    {noreply, State};

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

one_round_finished(#state{count = Count,fic1  = #future{code = C1,price = P1,old_p = OP1,days = D1} ,
                                        fic2  = #future{code = C2,price = P2,old_p = OP2,days = D2} ,
                                        fic3  = #future{code = C3,price = P3,old_p = OP3,days = D3} ,
                                        index = #index{code  = ICode, price = PI,old_p = OPI} } = State) when Count =:= 4 ->
  % qp_log:info("one rount finished:~p ~p ~p ~n",[Fic1,Fic2,Index]),
  if (P1 =:= OP1 andalso P2 =:= OP2 andalso PI =:= OPI) orelse PI =:= 0 orelse P1 =:= 0  orelse P2 =:= 0 orelse OPI =:= 0 ->
  % if (P1 =:= OP1 andalso P2 =:= OP2 andalso P3 =:= OP3 andalso PI =:= OPI) ->
      io:format("数据未有变动 ~n",[]),
      State2 = State#state{count = 0};
    true ->
      % qp_log:info("to process the same:~p ~p ~p  ~n",[P1,P2,PI]),
      PIN = round(PI * 1000),
      Dif1 = (PIN - P1) div D1,
      Dif2 = (PIN - P2) div D2,
      Dif3 = (PIN - P3) div D3,
      io:format("期指1:~p 贴水:~p ~n",[C1,(PIN - P1) div 1000]),
      io:format("期指2:~p 贴水:~p ~n",[C2,(PIN - P2) div 1000]),
      io:format("期指3:~p 贴水:~p ~n",[C3,(PIN - P3) div 1000]),
      Point1 = (Dif1 - Dif2) div 1000,
      if Point1 >= 0 andalso  P1 > 0 andalso P2 > 0 ->
        io:format("可套利点数:~p 做多:~p 做空:~p ~n",[Point1,C1,C2]);
      Point1 < 0 andalso  P1 > 0 andalso P2 > 0 ->
        io:format("可套利点数:~p 做多:~p 做空:~p ~n",[Point1,C2,C1]);
      true ->
        io:format("取数据出错！")
      end,
      if abs(Point1) > ?MAX_IC1_POINT_TO_NOTICE andalso  P1 > 0 andalso P2 > 0  ->
        OPT1 = true,
        os:cmd("afplay ./zentrading-0.1.0/src/yi.mp3");
      true ->
        OPT1 = false
      end,

      Point2 = (Dif1 - Dif3) div 1000,
      if Point2 >= 0 andalso  P1 > 0 andalso P3 > 0  ->
        io:format("可套利点数:~p 做多:~p 做空:~p ~n",[Point2,C1,C3]);
      Point2 < 0 andalso  P1 > 0 andalso P3 > 0 ->
        io:format("可套利点数:~p 做多:~p 做空:~p ~n",[Point2,C3,C1]);
      true ->
          io:format("取数据出错！")
      end,
      if abs(Point2) > ?MAX_IC2_POINT_TO_NOTICE andalso  P1 > 0 andalso P3 > 0  ->
        OPT2 = true,
        os:cmd("afplay ./zentrading-0.1.0/src/yi.mp3");
      true ->
        OPT2 = false
      end,
      Point3 = (Dif2 - Dif3) div 1000,
      if Point3 >= 0 andalso  P2 > 0 andalso P3 > 0  ->
        io:format("可套利点数:~p 做多:~p 做空:~p ~n",[Point3,C2,C3]);
      Point3 < 0 andalso  P2 > 0 andalso P3 > 0   ->
        io:format("可套利点数:~p 做多:~p 做空:~p ~n",[Point3,C3,C2]);
      true ->
        io:format("取数据出错！")
      end,
      if abs(Point3) > ?MAX_IC3_POINT_TO_NOTICE andalso  P2 > 0 andalso P3 > 0 ->
        OPT3 = true,
        os:cmd("afplay ./zentrading-0.1.0/src/yi.mp3");
      true ->
        OPT3 = false
      end,
      % Dif1 div D1
      if OPT1 orelse OPT2 orelse OPT3 ->
        gen_server:cast(db_mysql,{insert_ic,ICode,round(PI*1000),P1,P2,P3,1,Point1,Point2,Point3});
      true ->
        gen_server:cast(db_mysql,{insert_ic,ICode,round(PI*1000),P1,P2,P3,0,Point1,Point2,Point3})
      end,
      State2 = State#state{count = 0}
  end,
  StateN = qp_event:init_stats_timer(State2, #state.tm,?IC_MONITOR_INTERVAL*1000),
  qp_event:ensure_stats_timer(StateN, #state.tm, monitor_timer);
one_round_finished(State) ->
  State.
