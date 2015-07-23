-module (aws_events).

-export([start/0, stop/0]).

start() ->
  application:start(aws_events).

stop() ->
  application:stop(aws_events).
