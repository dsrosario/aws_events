-module(bad_event).
-behaviour(gen_event).

-compile(export_all).

-include("sqs_message.hrl").

init([]) ->
  {ok, []}.

handle_event({_SQSReader, Bad = #sqs_invalid_message{}}, State) ->
  error_logger:error_msg("Invalid message received!
                          Body=~p
                          Handle=~p
                          Id=~p~n",
                          [Bad#sqs_invalid_message.raw_message,
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