-module(bad_event).
-behaviour(gen_event).

-include("sqs_message.hrl").
%gen_event handler behaviour
-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3, terminate/2]).

init([]) ->
  {ok, []}.

handle_event({_SQSReader, Bad = #sqs_invalid_message{}}, State) ->
  lager:error(
"Invalid message received!
  Queue=~p
  Body=~p
  Handle=~p
  Id=~p~n",
[Bad#sqs_invalid_message.queue,
 Bad#sqs_invalid_message.raw_message,
 Bad#sqs_invalid_message.receipt_handle,
 Bad#sqs_invalid_message.message_id]),
  {ok, State};
handle_event({_SQSReader, #sqs_event_message{}}, State) ->
  {ok, State};
handle_event(Unknown, State) ->
  error_logger:error_msg("Unknown message received!
                          ~p~n",
                          [Unknown]),
  {ok, State}.

handle_call(_, State) ->
  {ok, ok, State}.

handle_info(_, State) ->
  {ok, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.
