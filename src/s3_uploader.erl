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
  {ok, AWSAccessId} = application:get_env(aws_events, aws_access),
  {ok, AWSSecret} = application:get_env(aws_events, aws_secret),
  erlcloud_s3:configure(AWSAccessId, AWSSecret),
  {ok, Bucket} = application:get_env(aws_events, aws_s3_bucket),
  %%if bucket doesn't exist, this process fail here!
  lager:info("Getting info from S3 bucket ~p", [Bucket]),
  Location = erlcloud_s3:get_bucket_attribute(Bucket, location),
  lager:info("S3 bucket ~p from ~p", [Bucket, Location]),
  {ok, Bucket}.

handle_event(File, Bucket) ->
  lager:info("s3_uploader: Reading file ~p",[File]),
  try file:read_file(File) of
    {ok, Data} ->
      lager:info("s3_uploader: uploading ~p",[File]),
      erlcloud_s3:put_object(Bucket, filename:basename(File), Data),
      lager:info("s3_uploader: uploaded ~p",[File]);
    Error ->
      lager:error("Can't upload ~p~nError~p",[File, Error])
  catch
    Error ->
      lager:error("Can't upload ~p~nError~p",[File, Error])
  end,
  {ok, Bucket}.

handle_call(_, Bucket) ->
  {ok, ok, Bucket}.

handle_info(_, Bucket) ->
  {ok, Bucket}.

code_change(_OldVsn, Bucket, _Extra) ->
  {ok, Bucket}.

terminate(_Reason, _State) ->
  ok.
