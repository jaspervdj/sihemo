sihemo
======

`sihemo` is a SImple HEartbeat MOnitoring application. It provides a web
interface and a REST API.

Web interface
-------------

The web interface exposes a read-only view for live monitoring.

REST API
--------

### Gettint the currently running services and their states

    GET /services.json

Example:

    curl localhost:8000/services.json

### Sending a heartbeat

    POST /services/:group/:name/heartbeat

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