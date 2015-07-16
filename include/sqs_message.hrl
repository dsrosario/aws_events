-record(sqs_event_message,
	{event, receipt_handle, message_id}).

-record(sqs_invalid_message,
	{raw_message, receipt_handle, message_id}).
