-module(event_file_writer).
-behaviour(gen_event).

-include("sqs_message.hrl").
-include("sqs_event.hrl").

%gen_event handler behaviour
-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3, terminate/2]).

-record(writer_config, {file_handle, fields, max_file_size, file_name}).

-define(LOG_DIG, "log/").

init(EventType) ->
  {ok, Format} = application:get_env(aws_events, event_file_format_description),
  Fields = proplists:get_value(EventType, Format),
  {ok, FileSize} = application:get_env(aws_events, max_event_file_size),
  MaxSize = proplists:get_value(EventType, FileSize),
  FileName = ?LOG_DIG ++ type_2_filename(EventType),
  {ok, FileHandle} = file:open(FileName, [write, append]),
  {ok, {EventType, #writer_config{file_handle=FileHandle, fields=Fields, max_file_size=MaxSize, file_name=FileName}}}.

handle_event({_SQSReader,
              EventMessage = #sqs_event_message{event = #sqs_event{type = EventType}}},
              {EventType, Config = #writer_config{}}) ->
  file:write(Config#writer_config.file_handle, event_print:print(Config#writer_config.fields, EventMessage#sqs_event_message.event) ++ "\n"),
  FileSize = filelib:file_size(Config#writer_config.file_name),
  {ok, {EventType, check_file_size(FileSize, Config)}};
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

%internal functions
type_2_filename(Atom) when is_atom(Atom) -> atom_to_list(Atom) ++ ".csv".

check_file_size(FileSize, Config = #writer_config{}) when FileSize >= Config#writer_config.max_file_size ->
  lager:info("~p reach max file size of ~p", [Config#writer_config.file_name, Config#writer_config.max_file_size]),
  file:close(Config#writer_config.file_handle),
  DirName = filename:dirname(Config#writer_config.file_name),
  Extension = filename:extension(Config#writer_config.file_name),
  BaseName = filename:basename(Config#writer_config.file_name, Extension),
  {{Year, Month, Day},{Hour, Minute, Second}} = calendar:local_time(),
  NewFileName = string:join([DirName, "/",
                             BaseName,
                             "_",
                             integer_to_list(Year),integer_to_list(Month),integer_to_list(Day),
                             "_",
                             integer_to_list(Hour),integer_to_list(Minute),integer_to_list(Second),
                             Extension], ""),
  lager:info("Renaming ~p to ~p", [Config#writer_config.file_name, NewFileName]),
  file:rename(Config#writer_config.file_name, NewFileName),
  s3_uploader:upload(NewFileName),
  {ok, FileHandle} = file:open(Config#writer_config.file_name, [write]),
  #writer_config{file_handle=FileHandle,
                fields=Config#writer_config.fields,
                 max_file_size=Config#writer_config.max_file_size,
                 file_name=Config#writer_config.file_name};
check_file_size(_, Config) ->
  Config.
