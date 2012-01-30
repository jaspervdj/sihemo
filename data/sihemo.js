function createWebSocket(path) {
    var host = window.location.hostname;
    if(host == '') host = 'localhost';
    var uri = 'ws://' + host + ':8000' + path;

    var Socket = "MozWebSocket" in window ? MozWebSocket : WebSocket;
    return new Socket(uri);
}

function Group(name) {
    this.name     = name;
    this.services = {};

    this.div = $(document.createElement('div'));
    this.div.append($(document.createElement('h2')).text(name));
    $('#groups').append(this.div);

    this.getService = function(json) {
        var service;
        if(this.services[json.id]) {
            service = this.services[json.id];
        } else {
            service = new Service(this, json.id, json.name);
            this.services[json.id] = service;
        }

        return service;
    }
}

function Service(group, id, name) {
    this.group = group;
    this.id    = id;
    this.name  = name;
    this.state = 'down';

    this.div = $(document.createElement('div'));
    this.div.append($(document.createElement('div')).text(name));
    this.div.append($(document.createElement('div'))
        .addClass('state')
        .text(this.state));

    $('#groups').append(this.div);

    this.update = function(state) {
        this.state = state;
        this.div.children('.state').text(state);
    };
}

function GroupManager() {
    this.groups = {};

    this.getService = function(json) {
        var group;
        if(this.groups[json.group]) {
            group = this.groups[json.group];
        } else {
            group = new Group(json.group);
            this.groups[json.group] = group;
        }

        return group.getService(json);
    }
}

$(document).ready(function() {
    var groupManager = new GroupManager();

    $.get('services.json', function(json) {
        for(var i in json) {
            var service = groupManager.getService(json[i].service);
            service.update(json[i].state);
        }
    });
});
