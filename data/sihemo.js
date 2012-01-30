function createWebSocket(path) {
    var host = window.location.hostname;
    if(host == '') host = 'localhost';
    var uri = 'ws://' + host + ':8000' + path;

    var Socket = "MozWebSocket" in window ? MozWebSocket : WebSocket;
    return new Socket(uri);
}


function Model() {
    this.listeners = [];
}

Model.prototype.addChangeListener = function(listener) {
    this.listeners.push(listener);
};

Model.prototype.triggerChange = function() {
    for(var i in this.listeners) {
        this.listeners[i].onChange(this);
    }
};


Group.prototype = new Model();
Group.prototype.constructor = Group;

function Group(name) {
    Model.call(this);
    this.name     = name;
    this.services = {};
    this.allUp    = true;
}

Group.prototype.getService = function(json) {
    var service;
    if(this.services[json.id]) {
        service = this.services[json.id];
    } else {
        service = new Service(json.id, json.name);
        service.addChangeListener(this);
        this.services[json.id] = service;
        this.triggerChange();
    }

    return service;
};

Group.prototype.onChange = function() {
    var allUp = true;

    for(var i in this.services) {
        allUp = allUp && this.services[i].state == 'up';
    }

    if(allUp != this.allUp) {
        this.allUp = allUp;
        this.triggerChange();
    }
};


function GroupView(group) {
    this.group    = group;
    this.div      = $(document.createElement('div'));
    this.services = {};

    var servicesDiv = this.servicesDiv = $(document.createElement('div'))
            .addClass('services')
            .hide();

    this.header = $(document.createElement('h2'))
            .text(group.name)
            .click(function () {servicesDiv.toggle(100)});

    this.div.append(this.header);
    this.div.append(this.servicesDiv);

    this.group.addChangeListener(this);
    this.onChange(group);
}

GroupView.prototype.addMissingServiceViews = function() {
    for(var i in this.group.services) {
        if(!this.services[i]) {
            var serviceView = new ServiceView(this.group.services[i]);
            this.servicesDiv.append(serviceView.div);
            this.services[i] = serviceView;
        }
    }
}

GroupView.prototype.onChange = function(group) {
    this.addMissingServiceViews();
    this.div.children('h2')
            .removeClass('up down')
            .addClass(group.allUp ? 'up' : 'down');
}


Service.prototype = new Model();
Service.prototype.constructor = Service;

function Service(id, name) {
    Model.call(this);
    this.id    = id;
    this.name  = name;
    this.state = 'down';
}

Service.prototype.setState = function(state) {
    this.state = state;
    this.triggerChange();
};


function ServiceView(service) {
    this.service = service;
    this.div     = $(document.createElement('div'));

    this.div.append($(document.createElement('div'))
            .addClass('state'));
    this.div.append($(document.createElement('div'))
            .addClass('name')
            .text(service.name));

    this.service.addChangeListener(this);
}

ServiceView.prototype.onChange = function(service) {
    this.div.children('.state')
            .removeClass('up down')
            .addClass(service.state);
}


function GroupManager() {
    this.groups = {};
}

GroupManager.prototype.getService = function(json) {
    var group;
    if(this.groups[json.group]) {
        group = this.groups[json.group];
    } else {
        group = new Group(json.group);
        groupView = new GroupView(group);
        $('#groups').append(groupView.div);
        this.groups[json.group] = group;
    }

    return group.getService(json);
}


$(document).ready(function() {
    var groupManager = new GroupManager();

    $.get('services.json', function(json) {
        for(var i in json) {
            var service = groupManager.getService(json[i].service);
            service.setState(json[i].state);
        }

        var ws = createWebSocket('/subscribe');

        ws.onmessage = function(event) {
            var json = JSON.parse(event.data);
            var service = groupManager.getService(json.service);
            service.setState(json.state);
        };

        ws.onerror = function(event) {
            alert('Warning, WebSocket connection error!');
        };

        ws.onclose = function(event) {
            alert('Warning, WebSocket connection closed!');
        };
    });
});
