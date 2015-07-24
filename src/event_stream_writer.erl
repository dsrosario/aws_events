-module(event_stream_writer).
-behaviour(gen_event).

%gen_event handler behaviour
-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3, terminate/2]).

-include("sqs_message.hrl").
-include("sqs_event.hrl").

init({WebSocketPid, EventType}) ->
  {ok, StreamFormat} = application:get_env(aws_events, event_stream_format_description),
  {ok, {WebSocketPid, EventType, StreamFormat}}.

handle_event({_SQSReader,
              EventMessage = #sqs_event_message{event = #sqs_event{type = EventType}}},
              {WebSocketPid, EventType, StreamFormat}) ->
  Event = EventMessage#sqs_event_message.event,
  Format = proplists:get_value(EventType, StreamFormat),
  websocket:post(WebSocketPid, EventType, event_print:stream(Format, Event)),
  {ok, {WebSocketPid, EventType, StreamFormat}};
handle_event(_, ConfigData) ->
  {ok, ConfigData}.

handle_call(_, ConfigData) ->
  {ok, ok, ConfigData}.

handle_info(_, ConfigData) ->
  {ok, ConfigData}.

code_change(_OldVsn, ConfigData, _Extra) ->
  {ok, ConfigData}.

terminate(_Reason, _Config) ->
  ok.
