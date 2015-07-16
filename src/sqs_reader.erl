-module(sqs_reader).
-compile(export_all).

-include("sqs_message.hrl").

init(AWSAccessKeyId, AWSSecretKey, AWSHost) ->
  sqs_events:init(),
  ssl:start(),
  erlcloud:start(),
  erlcloud_sqs:configure(AWSAccessKeyId, AWSSecretKey, AWSHost).

list_queues() ->
  erlcloud_sqs:list_queues().

receive_message(Queue) ->
  RawMessage = erlcloud_sqs:receive_message(Queue),
  case RawMessage of
    [{messages, [Message]}] -> decode_message(Message);
    [{messages, []}] -> no_message
  end.

decode_message(Message) ->
  Body = proplists:get_value(body, Message),
  ReceiptHandle = proplists:get_value(receipt_handle, Message),
  MessageId = proplists:get_value(message_id, Message),
  try sqs_events:decode(list_to_binary(Body)) of
    Event -> {ok, #sqs_event_message{event = Event,
                      receipt_handle = ReceiptHandle,
                      message_id = MessageId}}
  catch
    error:_ -> {error, #sqs_invalid_message{raw_message = Body,
                      receipt_handle = ReceiptHandle,
                      message_id = MessageId}}
  end.
