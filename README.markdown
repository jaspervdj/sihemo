sihemo
======

`sihemo` is a SImple HEartbeat MOnitoring application. It provides a web
interface and a REST API.

Web interface
-------------

The web interface exposes a read-only view for live monitoring.

REST API
--------

### Get the currently running services and their states

    GET /services

Example:

    curl localhost:8000/services

### Get information about a single service

    GET /services/:group/:name

Example:

    curl localhost:8000/services/galactica/shields

### Sending a heartbeat

    POST /services/:group/:name

Send a heartbeat for the specified service.

Parameters:

- `alive`, an integer indicating how long we assume the component to be alive.
  If it is not given, the default 30 is used.

Example:

    curl -XPOST -d alive=30 localhost:8000/services/galactica/shields/heartbeat

### Shut down a service

    DELETE /services/:group/:name

Safely shut down the specified service. This will cause the system not to panic
when no further heartbeats arrive.

Example:

    curl -XDELETE localhost:8000/services/galactica/shields

Haskell API
-----------

(to document)

Ruby client
-----------

A simple ruby client is available in `ruby/sihemo.rb`. Usage example:

    s = Sihemo.new('localhost', 8000)
    s.heartbeat('galactica', 'shields', 5)
    s.shutdown('galactica', 'shields')
