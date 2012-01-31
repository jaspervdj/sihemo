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

- `alive`, an integer indicating how long we assume the component to be alive

Example:

    curl -XPOST -d alive=30 localhost:8000/services/galactica/shields/heartbeat
