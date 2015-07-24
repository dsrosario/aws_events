-module(sqs_events).
-export([init/0, init/1, decode/1]).
-include("egeoip.hrl").
-include("sqs_event.hrl").

%%% API

%% Init module
%% SchemaFilePath is the path of the json schema (draft 3)
init() ->
	SchemaFile = priv_path("json_events.schema"),
	init(SchemaFile).

init(SchemaFilePath) ->
	{ok, Data} = file:read_file(SchemaFilePath),
	true = jsx:is_json(Data),
	Schema = jsx:decode(Data),
	egeoip:start(),
	jesse:add_schema(aws_events, Schema).

%% Decode Json data to record
decode(Json) ->
	{ok, DecodedJson} = validate(Json),
	Type = proplists:get_value(<<"event_type">>, DecodedJson),
	Source = proplists:get_value(<<"source">>, DecodedJson),
	Time = proplists:get_value(<<"time">>, DecodedJson),
	UserId = proplists:get_value(<<"user_id">>, DecodedJson),
	IpAddress = proplists:get_value(<<"ip_address">>, DecodedJson),
	{ok, GeoIp} = egeoip:lookup(binary:bin_to_list(IpAddress)),
	DataList = proplists:get_value(<<"event_data">>, DecodedJson),
	#sqs_event{type=translate_event_type(Type),
		source=translate_source(Source),
		time=Time,
		user_id=UserId,
		longitude=GeoIp#geoip.longitude,
		latitude=GeoIp#geoip.latitude,
		ip_address=IpAddress,
		data=[translate_data_elem(D) || D <- DataList]}.


%% Internal helpers
validate(Json) ->
	Decoded = jsx:decode(Json),
	jesse:validate(aws_events, Decoded).


translate_event_type(<<"session">>) -> session;
translate_event_type(<<"purchase">>) -> purchase.

translate_source(<<"web">>) -> web;
translate_source(<<"mobile">>) -> mobile.

translate_data_elem(PropList) ->
	Name = proplists:get_value(<<"name">>, PropList),
	Value = proplists:get_value(<<"value">>, PropList),
	{Name, Value}.

priv_path(Components) ->
    AppDir = case code:which(?MODULE) of
                 cover_compiled -> "..";
                 F -> filename:dirname(filename:dirname(F))
             end,
    filename:join([AppDir, "priv",  Components]).
