function createWebSocket(path) {
    var host = window.location.hostname;
    var port = window.location.port;
    if(host == '') host = 'localhost';
    var uri = 'ws://' + host + ':' + port + path;

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


function StateView(model) {
    this.model = model;
    this.div   = $(document.createElement('div'));

    this.div.addClass('state');

    this.model.addChangeListener(this);
    this.onChange(this.model);
}

StateView.prototype.onChange = function(model) {
    this.div.removeClass('up down');
    this.div.addClass(model.isUp() ? 'up' : 'down');
}


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
    if(this.services[json.name]) {
        service = this.services[json.name];
    } else {
        service = new Service(json.name);
        service.addChangeListener(this);
        this.services[json.name] = service;
        this.triggerChange();
    }

    return service;
};

Group.prototype.onChange = function() {
    var allUp = true;

    for(var i in this.services) {
        allUp = allUp && this.services[i].isUp();
    }

    if(allUp != this.allUp) {
        this.allUp = allUp;
        this.triggerChange();
    }
};

Group.prototype.isUp = function() {
    return this.allUp;
}


function GroupView(group) {
    this.group     = group;
    this.div       = $(document.createElement('div'));
    this.services  = {};
    this.stateView = new StateView(group);

    var servicesDiv = this.servicesDiv = $(document.createElement('div'))
            .addClass('services')
            .hide();

    this.header = $(document.createElement('div')).addClass('header');


    this.header.append($(document.createElement('a'))
            .addClass('toggle')
            .attr('href', '#')
            .click(function () {
                servicesDiv.toggle(100);
                $(this).toggleClass('show');
            }));
    this.header.append(this.stateView.div);
    this.header.append(group.name)

    this.div.append(this.header);
    this.div.append(this.servicesDiv);

    this.group.addChangeListener(this);
    this.onChange(group);
}

GroupView.prototype.onChange = function(group) {
    /* Add missing service views */
    for(var i in this.group.services) {
        if(!this.services[i]) {
            var serviceView = new ServiceView(this.group.services[i]);
            this.servicesDiv.append(serviceView.div);
            this.services[i] = serviceView;
        }
    }
}


Service.prototype = new Model();
Service.prototype.constructor = Service;

function Service(name) {
    Model.call(this);
    this.name = name;
    this.up   = false;
}

Service.prototype.setState = function(state) {
    this.up = state == 'up';
    this.triggerChange();
};

Service.prototype.isUp = function() {
    return this.up;
}


function ServiceView(service) {
    this.service   = service;
    this.div       = $(document.createElement('div'));
    this.stateView = new StateView(service);

    this.div.append(this.stateView.div);
    this.div.append($(document.createElement('div'))
            .addClass('name')
            .text(service.name));

    this.service.addChangeListener(this);
}

ServiceView.prototype.onChange = function(service) {
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
