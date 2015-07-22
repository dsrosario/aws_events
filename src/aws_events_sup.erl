-module(aws_events_sup).

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

init([]) ->
    {ok, { {one_for_all, 5, 10},
          [
            {event_dispatcher,
             {event_dispatcher, start_link, []},
             permanent,
             5000,
             worker,
             dynamic
             }
             ,
             {
              sqs_reader_sup,
              {sqs_reader_sup, start_link, []},
              permanent,
              5000,
              supervisor,
              dynamic
             }
          ]}
    }.
