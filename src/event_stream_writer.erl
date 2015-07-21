-module(event_stream_writer).
-behaviour(gen_event).

-compile(export_all).

-include("sqs_message.hrl").
-include("sqs_event.hrl").

init([]) ->
  {ok, StreamFormat} = application:get_env(aws_events, event_stream_format_description),
  {ok, StreamFormat}.

handle_event({_SQSReader,
              EventMessage = #sqs_event_message{event = #sqs_event{}}},
              StreamFormat) ->
  Format = proplists:get_value(EventMessage#sqs_event_message.event#sqs_event.type, StreamFormat),
  io:format("~p~n", [event_print:stream(Format, EventMessage#sqs_event_message.event)]),
  {ok, StreamFormat};
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
