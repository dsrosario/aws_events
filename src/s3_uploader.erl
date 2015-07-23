-module (s3_uploader).
-behaviour(gen_event).
-compile(export_all).

%EXTERNAL API
start_link() ->
  {ok, Pid} = gen_event:start_link(),
  register(s3_uploader, Pid),
  gen_event:add_handler(Pid, ?MODULE, []),
  {ok, Pid}.

upload(File) ->
  gen_event:notify(whereis(s3_uploader), File).

stop() ->
  gen_event:stop(whereis(event_dispatcher)).

%EVENT LOOP BEHAVIOUR
init([]) ->
  {ok, []}.

handle_event(File, State) ->
  lager:info("s3_uploader: uploading ~p",[File]),
  {ok, State}.

handle_call(_, State) ->
  {ok, ok, State}.

handle_info(_, State) ->
  {ok, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.
