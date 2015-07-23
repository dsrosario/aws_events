-record(sqs_event,
	{type, source, time, user_id, ip_address, longitude, latitude, data}).

-define(VALID_EVENT_TYPES, [session, purchase]).
