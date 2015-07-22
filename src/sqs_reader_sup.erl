-module(sqs_reader_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, stop/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop() ->
    ok.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

get_queues(AWSAccessId, AWSSecret, AWSHost, all) ->
  get_queues(AWSAccessId, AWSSecret, AWSHost,[]);
get_queues(AWSAccessId, AWSSecret, AWSHost, []) ->
  ok = erlcloud_sqs:configure(AWSAccessId, AWSSecret, AWSHost),
  erlcloud_sqs:list_queues();
%%Queues from configuration
get_queues(_, _, _, [Queues]) -> [Queues].

init([]) ->
  {ok, AWSAccessId} = application:get_env(aws_events, aws_access),
  {ok, AWSSecret} = application:get_env(aws_events, aws_secret),
  {ok, AWSHost} = application:get_env(aws_events, aws_sqs_host),
  {ok, QueueList} = application:get_env(aws_events, aws_sqs_queues),
  AWSQueues = get_queues(AWSAccessId, AWSSecret, AWSHost, QueueList),
  lager:info("Queue list: ~p", AWSQueues),
  Procs = [
    {
       {sqs_reader, make_ref()},
       {sqs_reader, start_link, [AWSAccessId, AWSSecret, AWSHost, Queue]},
       permanent,
       5000,
       worker,
       dynamic
    }
   || Queue <- AWSQueues],

  {ok,
    { {one_for_one, 5, 10},
      Procs
    }
  }.
