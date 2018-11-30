%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License
%% at http://www.mozilla.org/MPL/
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and
%% limitations under the License.
%%
%% The Original Code is RabbitMQ.
%%
%% The Initial Developer of the Original Code is GoPivotal, Inc.
%% Copyright (c) 2007-2016 Pivotal Software, Inc.  All rights reserved.
%%

-module(qp_log).

-export([debug/1, debug/2, debug/3,
         info/1, info/2, info/3,
         notice/1, notice/2, notice/3,
         warning/1, warning/2, warning/3,
         error/1, error/2, error/3,
         critical/1, critical/2, critical/3,
         alert/1, alert/2, alert/3,
         emergency/1, emergency/2, emergency/3,
         none/1, none/2, none/3]).

%%----------------------------------------------------------------------------

-type category() :: atom().

-spec debug(string()) -> 'ok'.
-spec debug(string(), [any()]) -> 'ok'.
-spec debug(pid() | [tuple()], string(), [any()]) -> 'ok'.
-spec info(string()) -> 'ok'.
-spec info(string(), [any()]) -> 'ok'.
-spec info(pid() | [tuple()], string(), [any()]) -> 'ok'.
-spec notice(string()) -> 'ok'.
-spec notice(string(), [any()]) -> 'ok'.
-spec notice(pid() | [tuple()], string(), [any()]) -> 'ok'.
-spec warning(string()) -> 'ok'.
-spec warning(string(), [any()]) -> 'ok'.
-spec warning(pid() | [tuple()], string(), [any()]) -> 'ok'.
-spec error(string()) -> 'ok'.
-spec error(string(), [any()]) -> 'ok'.
-spec error(pid() | [tuple()], string(), [any()]) -> 'ok'.
-spec critical(string()) -> 'ok'.
-spec critical(string(), [any()]) -> 'ok'.
-spec critical(pid() | [tuple()], string(), [any()]) -> 'ok'.
-spec alert(string()) -> 'ok'.
-spec alert(string(), [any()]) -> 'ok'.
-spec alert(pid() | [tuple()], string(), [any()]) -> 'ok'.
-spec emergency(string()) -> 'ok'.
-spec emergency(string(), [any()]) -> 'ok'.
-spec emergency(pid() | [tuple()], string(), [any()]) -> 'ok'.
-spec none(string()) -> 'ok'.
-spec none(string(), [any()]) -> 'ok'.
-spec none(pid() | [tuple()], string(), [any()]) -> 'ok'.

%%----------------------------------------------------------------------------

debug(Format) -> debug(Format, []).
debug(Format, Args) -> debug(self(), Format, Args).
debug(Metadata, Format, Args) ->
    lager:log(debug, Metadata, Format, Args).

info(Format) -> info(Format, []).
info(Format, Args) -> info(self(), Format, Args).
info(Metadata, Format, Args) ->
    lager:log(info, Metadata, Format, Args).

notice(Format) -> notice(Format, []).
notice(Format, Args) -> notice(self(), Format, Args).
notice(Metadata, Format, Args) ->
    lager:log(notice, Metadata, Format, Args).

warning(Format) -> warning(Format, []).
warning(Format, Args) -> warning(self(), Format, Args).
warning(Metadata, Format, Args) ->
    lager:log( warning, Metadata, Format, Args).

error(Format) -> ?MODULE:error(Format, []).
error(Format, Args) -> ?MODULE:error(self(), Format, Args).
error(Metadata, Format, Args) ->
    lager:log(error, Metadata, Format, Args).

critical(Format) -> critical(Format, []).
critical(Format, Args) -> critical(self(), Format, Args).
critical(Metadata, Format, Args) ->
    lager:log(critical, Metadata, Format, Args).

alert(Format) -> alert(Format, []).
alert(Format, Args) -> alert(self(), Format, Args).
alert(Metadata, Format, Args) ->
    lager:log(alert, Metadata, Format, Args).

emergency(Format) -> emergency(Format, []).
emergency(Format, Args) -> emergency(self(), Format, Args).
emergency(Metadata, Format, Args) ->
    lager:log(emergency, Metadata, Format, Args).

none(Format) -> none(Format, []).
none(Format, Args) -> none(self(), Format, Args).
none(Metadata, Format, Args) ->
    lager:log(none, Metadata, Format, Args).
