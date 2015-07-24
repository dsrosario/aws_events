# aws_events
To build:
cmd: make

To run:
Edit aws_events.config and edit:
	* aws_access
	* aws_secret
	* aws_sqs_host: the host with sqs queues.
	* aws_sqs_queues: all to use all queues or a list with queues url.
	* aws_s3_host: the host to connect to S3 service.
	* aws_s3_bucket: the place to post .csv files.

cmd: make shell

 