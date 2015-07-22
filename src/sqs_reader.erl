-module(sqs_reader).
-behaviour(gen_fsm).
-compile(export_all).

-include("sqs_message.hrl").

-define(TIMEOUT, 1000).

%EXTERNAL API
start_link(AWSAccessKeyId, AWSSecretKey, AWSHost, Queue) ->
  gen_fsm:start_link(?MODULE, [AWSAccessKeyId, AWSSecretKey, AWSHost, Queue], []).

delete_message(Pid, Handle) ->
  gen_fsm:send_event(Pid, {delete, Handle}).

%GEN FSM functions
init([AWSAccessKeyId, AWSSecretKey, AWSHost, Queue]) ->
  ok = erlcloud_sqs:configure(AWSAccessKeyId, AWSSecretKey, AWSHost),
  lager:info("SQS reader -> queue:~p",[Queue]),
  {ok, read_queue, Queue, 10}.

handle_event(_, _, Queue) ->
  {next_state, read_queue, Queue, ?TIMEOUT}.

handle_sync_event(_, _, _, Queue) ->
  {next_state, read_queue, Queue, ?TIMEOUT}.

handle_info(_, read_queue, Queue) ->
  {next_state, read_queue, Queue, ?TIMEOUT}.

read_queue({delete, Handle}, Queue) ->
  erlcloud_sqs:delete_message(Queue, Handle),
  {next_state, read_queue, Queue, ?TIMEOUT};
read_queue(timeout, Queue) ->
  receive_and_dispatch_message(Queue),

  {next_state, read_queue, Queue, ?TIMEOUT}.
code_change(_OldVsn, read_queue, Queue, _Extra) ->
  {ok, read_queue, Queue, ?TIMEOUT}.

terminate(_Reason, read_queue, _Queue) ->
    ok.


%%INTERNAL API
receive_and_dispatch_message(Queue) ->
  dispatch_and_receive_message(receive_message(Queue), Queue).
dispatch_and_receive_message(no_message, _) -> ok;
dispatch_and_receive_message({_, Message}, Queue) ->
  event_dispatcher:dispatch(Message),
  NewMessage = receive_message(Queue),
  dispatch_and_receive_message(NewMessage, Queue).

receive_message(Queue) ->
  RawMessage = erlcloud_sqs:receive_message(Queue),
  case RawMessage of
    [{messages, [Message]}] -> decode_message(Message, Queue);
    [{messages, []}] -> no_message
  end.

decode_message(Message, Queue) ->
  Body = proplists:get_value(body, Message),
  ReceiptHandle = proplists:get_value(receipt_handle, Message),
  MessageId = proplists:get_value(message_id, Message),
  try sqs_events:decode(list_to_binary(Body)) of
    Event -> {ok, #sqs_event_message{event = Event,
                      receipt_handle = ReceiptHandle,
                      message_id = MessageId,
                      queue = Queue}}
  catch
    error:_ -> {error, #sqs_invalid_message{raw_message = Body,
                      receipt_handle = ReceiptHandle,
                      message_id = MessageId,
                      queue = Queue}}
  end.
