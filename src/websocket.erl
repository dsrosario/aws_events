-module(websocket).
-behaviour(cowboy_websocket_handler).

-include("sqs_event.hrl").

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).


%% API.
-export([start/0]).
-export([stop/0]).
-export([post/3]).

%% API.
start() ->
	WebsocketEndpoints = [{"/" ++ atom_to_list(EventType), ?MODULE, EventType} || EventType <- ?VALID_EVENT_TYPES],

	Dispatch = cowboy_router:compile([
		{'_', [
			{"/", cowboy_static, {file, "priv/index.html"}},
			{"/static/[...]", cowboy_static, {dir, "priv/static"}} ] ++
			WebsocketEndpoints
		}
	]),
	cowboy:start_http(http, 100, [{port, 8080}],
		[{env, [{dispatch, Dispatch}]}]).

stop() ->
	cowboy:stop_listener(whereis(websocket)),
	ok.

post(Pid, EventType, Message) ->
  lager:info("posting ~p for websocket client ~p register for ~p", [Message, Pid, EventType]),
	Pid ! {EventType, Message}.

init({tcp, http}, Req, EventType) ->
	{upgrade, protocol, cowboy_websocket, Req, EventType}.

websocket_init(_TransportName, Req, EventType) ->
	lager:info("websocket client ~p register for ~p", [self(), EventType]),
	event_dispatcher:register(event_stream_writer, {self(), EventType}),
	{ok, Req, EventType}.

websocket_handle(_Data, Req, EventType) ->
	{ok, Req, EventType}.

websocket_info({EventType, Msg}, Req, EventType) when is_list(Msg) ->
	lager:info("websocket client ~p received ~p", [self(), Msg]),
	{reply, {text, list_to_binary(Msg)}, Req, EventType};

websocket_info(_Info, Req, EventType) ->
	{ok, Req, EventType}.

websocket_terminate(_Reason, _Req, EventType) ->
  lager:info("websocket client ~p unregister for ~p", [self(), EventType]),
	event_dispatcher:unregister(event_stream_writer, {self(), EventType}),
	ok.
