-module (event_dispatcher).
-behaviour(gen_event).
-include("sqs_event.hrl").
-include("sqs_message.hrl").
-compile(export_all).

%EXTERNAL API
start_link() ->
  {ok, Pid} = gen_event:start_link(),
  register(event_dispatcher, Pid),
  gen_event:add_handler(Pid, event_writer, session),
  gen_event:add_handler(Pid, event_writer, purchase),
  gen_event:add_handler(Pid, bad_event, []),
  gen_event:add_handler(Pid, ?MODULE, []),
  {ok, Pid}.

dispatch(SQSEvent = #sqs_event_message{}) ->
  gen_event:notify(whereis(event_dispatcher), {self(), SQSEvent});
dispatch(SQSBadMessage = #sqs_invalid_message{}) ->
  gen_event:notify(whereis(event_dispatcher), {self(), SQSBadMessage}).

stop() ->
  gen_event:stop(whereis(event_dispatcher)).

%EVENT LOOP BEHAVIOUR
init([]) ->
  {ok, []}.

handle_event({Pid, Message = #sqs_event_message{}}, State) ->
  sqs_reader:delete_message(Pid,Message#sqs_event_message.receipt_handle),
  {ok, State};
handle_event({Pid, Message = #sqs_invalid_message{}}, State) ->
    sqs_reader:delete_message(Pid,Message#sqs_invalid_message.receipt_handle),
    {ok, State}.

handle_call(_, State) ->
  {ok, ok, State}.

handle_info(_, State) ->
  {ok, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.
