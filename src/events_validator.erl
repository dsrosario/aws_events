-module(events_validator).
-export([init/1, validate/1]).


%% Init module
%% SchemaFilePath is the path of the json schema (draft 3)
init(SchemaFilePath) ->
	{ok, Data} = file:read_file(SchemaFilePath),
	true = jsx:is_json(Data),
	Schema = jsx:decode(Data),
	jesse:add_schema(aws_events, Schema).


%% Validate Raw Json data
%% Returns {ok, json} if valid.
validate(Json) ->
	Decoded = jsx:decode(Json),
	jesse:validate(aws_events, Decoded).
	