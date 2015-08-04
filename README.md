Objective of the server is to read different types of events from Amazon SQS, collect the events
into separate files, and upload the files to Amazon S3 bucket once file has achieved certain size.
The server should also expose a websockets interface for real-time event monitoring.

To build:
1. make

To run:
1. Edit aws_events.config and edit:
	* aws_access
	* aws_secret
	* aws_sqs_host: the host with sqs queues.
	* aws_sqs_queues: all to use all queues or a list with queues url.
	* aws_s3_host: the host to connect to S3 service.
	* aws_s3_bucket: the place to post .csv files.

2. make run
