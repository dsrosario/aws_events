-module(event_print).
-compile(export_all).

-include("sqs_event.hrl").

record_to_proplist(Event = #sqs_event{}) ->
  [sqs_event | Values] = tuple_to_list(Event),
  lists:zip(record_info(fields, sqs_event), Values).

convert_to_list(undefined) -> [];
convert_to_list(Value) when is_list(Value) -> Value;
convert_to_list(Value) when is_float(Value) -> float_to_list(Value, [{decimals, 3}, compact]);
convert_to_list(Value) when is_integer(Value) -> integer_to_list(Value);
convert_to_list(Value) when is_atom(Value) -> atom_to_list(Value);
convert_to_list(Value) when is_binary(Value) -> binary_to_list(Value).

get_element(Field, EventPropList) when is_atom(Field)->
  convert_to_list(proplists:get_value(Field, EventPropList));
get_element(Field, EventPropList) when is_binary(Field)->
  Data = proplists:get_value(data, EventPropList),
  convert_to_list(proplists:get_value(Field, Data)).

print(Config, Event = #sqs_event{}) ->
  EventPropList = record_to_proplist(Event),
  string:join([get_element(Field, EventPropList) || Field <- Config], ",").
