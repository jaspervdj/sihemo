sihemo
======

`sihemo` is a SImple HEartbeat MOnitoring application. It provides a web
interface and a REST API.

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

    POST /services/:group/:name/shutdown

Safely shut down the specified service. This will cause the system not to panic
when no further heartbeats arrive.

Example:

    curl -XPOST localhost:8000/services/galactica/shields/shutdown
