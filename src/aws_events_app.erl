-module(aws_events_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    lager:start(),
    application:ensure_all_started(cowboy),
    ok = sqs_events:init(),
    ssl:start(),
    erlcloud:start(),
    websocket:start(),
    aws_events_sup:start_link().

stop(_State) ->
    aws_events_sup:stop(),
    websocket:stop().
