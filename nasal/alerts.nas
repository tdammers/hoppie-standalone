if (!contains(mcdu.globals, 'alerts')) {
    mcdu.globals.alerts = {};
}

var setAlert = func(name) {
    if (!contains(mcdu.globals.alerts, name))
        mcdu.globals.alerts[name] = 2;
}

var clearAlert = func(name) {
    delete(mcdu.globals.alerts, name);
}

var getNewAlert = func {
    foreach (var k; keys(mcdu.globals.alerts)) {
        if (mcdu.globals.alerts[k] > 1) {
            mcdu.globals.alerts[k] = 1;
            return k;
        }
    }
    return nil;
}

var getActiveAlerts = func {
    var alerts = [];
    foreach (var k; keys(mcdu.globals.alerts)) {
        if (mcdu.globals.alerts[k] > 0)
            append(alerts, k);
    }
    return alerts;
};

return {
    'setAlert': setAlert,
    'clearAlert': clearAlert,
    'getNewAlert': getNewAlert,
    'getActiveAlerts': getActiveAlerts,
};
