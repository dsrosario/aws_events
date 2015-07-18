-module(event_writer).
-behaviour(gen_event).

-compile(export_all).

-include("sqs_message.hrl").
-include("sqs_event.hrl").

init(EventType) ->
  {ok, EventType}.

handle_event({_SQSReader, EventMessage = #sqs_event_message{event = #sqs_event{type = EventType}}}, EventType) ->
  io:format("Event ~p received and handle~n", [EventMessage]),
  {ok, EventType};
handle_event(_, EventType) ->
  {ok, EventType}.

handle_call(_, EventType) ->
  {ok, ok, EventType}.

handle_info(_, EventType) ->
  {ok, EventType}.

code_change(_OldVsn, EventType, _Extra) ->
  {ok, EventType}.

terminate(_Reason, _State) ->
  ok.

type_2_filename(session) -> "session";
type_2_filename(purchase) -> "purchase".
