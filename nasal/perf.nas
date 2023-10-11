if (!contains(mcdu, 'perfInitData')) {
    mcdu.perfInitData = {};
}

var getFuelCapacity = func {
    var baseNode = props.globals.getNode('/consumables/fuel');
    var tanks = baseNode.getChildren('tank');
    var totalCapacity = 0;
    foreach (var tank; tanks) {
        var capacity = tank.getValue('capacity-m3');
        var density = tank.getValue('density-kgpm3');
        if (capacity == nil or density == nil)
            continue;
        totalCapacity += density * capacity;
    }
    return totalCapacity;
};

var getFuelFlow = func {
    return mcdu.fuelSamplerVars.ffKG;
};

var getFuelOnBoard = func {
    return mcdu.fuelSamplerVars.currentKG;
};

var dumpPerfProfile = func {
    var alt = 0;
    var dist = 0;
    var perfIdx = 0;
    print("-- PERF PROFILE --");
    if (!contains(mcdu, 'perfInitData') or !contains(mcdu.perfInitData, 'climb'))
        return;
    debug.dump(mcdu.perfInitData.climb);
    for (var t = 0; t < 30; t += 1) {
        var fl = alt / 100;
        var perfEntry = mcdu.perfInitData.climb[perfIdx];
        while (perfEntry != nil and fl > perfEntry.fl) {
            perfIdx += 1;
            if (perfIdx >= size(mcdu.perfInitData.climb))
                perfEntry = nil;
            else
                perfEntry = mcdu.perfInitData.climb[perfIdx];
        }
        if (perfEntry == nil)
            break;
        var tas = (substr(perfEntry.spd ~ '', 0, 1) == 'M')
                        ? mcdu.fms.machToTAS(alt, substr(perfEntry.spd ~ '', 1) / 100)
                        : mcdu.fms.iasToTAS(alt, perfEntry.spd);
        printf("%02i FL%03i SPD %3s TAS %3i %5.1fnmi", t, alt / 100, perfEntry.spd, tas, dist);
        alt += perfEntry.roc;
        dist += tas / 60;
    }
}

var getPerfInitData = func {
    var result = {};
    foreach (var k; keys(mcdu.perfInitData)) {
        result[k] = mcdu.perfInitData[k];
    }
    var crzAlt = getprop('/autopilot/route-manager/cruise/altitude-ft') or nil;
    var crzFL = getprop('/autopilot/route-manager/cruise/flight-level') or nil;
    var crzIAS = getprop('/autopilot/route-manager/cruise/speed-kts') or nil;
    var crzMach = getprop('/autopilot/route-manager/cruise/mach') or nil;

    if (crzAlt != nil) result.crzAlt = crzAlt else delete(result, 'crzAlt');
    if (crzFL != nil) result.crzFL = crzFL else delete(result, 'crzFL');
    if (crzIAS != nil) result.crzIAS = crzIAS else delete(result, 'crzIAS');
    if (crzMach != nil) result.crzMach = crzMach else delete(result, 'crzMach');

    return result;
}

var setPerfInitData = func (data) {
    foreach (var k; keys(data)) {
        var val = data[k];
        if (k == 'crzAlt') {
            if (val == nil) val = 0;
            setprop('/autopilot/route-manager/cruise/altitude-ft', math.round(val));
        }
        if (k == 'crzFL') {
            if (val == nil) val = 0;
            setprop('/autopilot/route-manager/cruise/flight-level', math.round(val));
        }
        elsif (k == 'crzIAS') {
            if (val == nil) val = 0;
            setprop('/autopilot/route-manager/cruise/speed-kts', math.round(val));
        }
        elsif (k == 'crzMach') {
            if (val == nil) val = 0;
            setprop('/autopilot/route-manager/cruise/mach', val);
        }
        else {
            mcdu.perfInitData[k] = val;
        }
    }

    return nil;
}

return {
    'getFuelFlow': getFuelFlow,
    'getFuelOnBoard': getFuelOnBoard,
    'getFuelCapacity': getFuelCapacity,

    'getPerfInitData': getPerfInitData,
    'setPerfInitData': setPerfInitData,
};
