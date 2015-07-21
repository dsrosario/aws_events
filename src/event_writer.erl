-module(event_writer).
-behaviour(gen_event).

-compile(export_all).

-include("sqs_message.hrl").
-include("sqs_event.hrl").

-record(writer_config, {file_handle, fields, max_file_size, file_name}).

init(EventType) ->
  {ok, Format} = application:get_env(aws_events, event_file_format_description),
  Fields = proplists:get_value(EventType, Format),
  {ok, FileSize} = application:get_env(aws_events, max_event_file_size),
  MaxSize = proplists:get_value(EventType, FileSize),
  FileName = "log/" ++ type_2_filename(EventType),
  {ok, FileHandle} = file:open(FileName, [write, append]),
  {ok, {EventType, #writer_config{file_handle=FileHandle, fields=Fields, max_file_size=MaxSize, file_name=FileName}}}.

handle_event({_SQSReader,
              EventMessage = #sqs_event_message{event = #sqs_event{type = EventType}}},
              {EventType, Config = #writer_config{}}) ->
  file:write(Config#writer_config.file_handle, event_print:print(Config#writer_config.fields, EventMessage#sqs_event_message.event) ++ "\n"),
  {ok, {EventType, Config}};
handle_event(_, ConfigData) ->
  {ok, ConfigData}.

handle_call(_, ConfigData) ->
  {ok, ok, ConfigData}.

handle_info(_, ConfigData) ->
  {ok, ConfigData}.

code_change(_OldVsn, ConfigData, _Extra) ->
  {ok, ConfigData}.

terminate(_Reason, Config = #writer_config{}) ->
  file:close(Config#writer_config.file_handle),
  ok.

type_2_filename(session) -> "session.csv";
type_2_filename(purchase) -> "purchase.csv".
