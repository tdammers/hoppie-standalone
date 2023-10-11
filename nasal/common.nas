
var getGroundspeed = func {
    return getprop('/velocities/groundspeed-kt');
};

var getUTCMinutes = func {
    var hour = getprop('/sim/time/utc/hour');
    var minute = getprop('/sim/time/utc/minute');
    var second = getprop('/sim/time/utc/second');
    return (hour * 60 + minute + second / 60);
};

var removeNilFields = func (h) {
    foreach (var k; keys(h)) {
        if (h[k] == nil)
            delete(h, k);
    }
    return h;
};

var iasToTAS = func (alt, ias) {
    return ias * (1.0 + 0.015 * alt / 1000);
}

var machToTAS = func (alt, mach) {
    # temperature: we'll use a simplified model here, where the tropopause is
    # assumed to be at FL350, sea level temperature is 20°C, and a constant
    # -51°C throughout the stratosphere.
    var tropopause = 35000;
    var tempC = (alt > tropopause) ? -51.0 : alt / tropopause * (-51 - 20) + 20;
    var gamma = 1.4;
    var R = 287.053;
    var T = tempC + 273.15;
    var c = math.sqrt(gamma * R * T);
    var tas_mps = mach * c;
    var tas = tas_mps * MPS2KT;
    return tas;
}

var getTransitionAlt = func {
    if (!contains(mcdu, 'perfInitData') or !contains(mcdu.perfInitData, 'transAlt'))
        return 18000;
    else
        return mcdu.perfInitData.transAlt;
}

var getFGCallsign = func {
    return getprop('/sim/multiplay/callsign');
}

var setFGCallsign = func (callsign) {
    setprop('/sim/multiplay/callsign', callsign);
    return nil;
}

var getFGAircraftType = func {
    return string.uc(getprop('/sim/aircraft'));
}


return {
    'getGroundspeed': getGroundspeed,
    'getUTCMinutes': getUTCMinutes,
    'getTransitionAlt': getTransitionAlt,
    'removeNilFields': removeNilFields,
    'iasToTAS': iasToTAS,
    'machToTAS': machToTAS,
    'getFGCallsign': getFGCallsign,
    'setFGCallsign': setFGCallsign,
    'getAircraftType': getFGAircraftType,
};
