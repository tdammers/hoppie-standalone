if (!contains(mcdu, 'modifiedFlightplan'))
    mcdu.modifiedFlightplan = nil;

if (!contains(mcdu, 'fuelSampler')) {
    mcdu.fuelSampler = maketimer(10, func { mcdu.fms._updateFuelSampler(); });
    mcdu.fuelSampler.simulatedTime = 1;
}

if (!contains(mcdu, 'perfInitData')) {
    mcdu.perfInitData = {};
}

if (!contains(mcdu, 'vnav')) {
    mcdu.vnav = {
        'todDist': nil,
    };
}

var fdm = getprop('/sim/flight-model');
var grossWeightProp = nil;
if (fdm == 'yasim')
    grossWeightProp = props.globals.getNode('/yasim/gross-weight-lbs');
elsif (fdm == 'jsb')
    grossWeightProp = props.globals.getNode('/fdm/jsbsim/inertia/weight-lbs');

var ON_STAND = 0;
var TAXI_OUT = 1;
var TAKEOFF = 2;
var CLIMB = 3;
var CRUISE = 4;
var DESCENT = 5;
var APPROACH = 6;
var TAXI_IN = 7;
var GO_AROUND = 8;

var flightPhaseName = [
        'ON_STAND',
        'TAXI_OUT',
        'TAKEOFF',
        'CLIMB',
        'CRUISE',
        'DESCENT',
        'APPROACH',
        'TAXI_IN',
        'GO_AROUND',
    ];

if (!contains(mcdu, 'flightPhase')) {
    mcdu.flightPhase = ON_STAND;
}

if (!contains(mcdu, 'alerts')) {
    mcdu.alerts = {};
}

var setAlert = func(name) {
    if (!contains(mcdu.alerts, name))
        mcdu.alerts[name] = 2;
}

var clearAlert = func(name) {
    delete(mcdu.alerts, name);
}

var getNewAlert = func {
    foreach (var k; keys(mcdu.alerts)) {
        if (mcdu.alerts[k] > 1) {
            mcdu.alerts[k] = 1;
            return k;
        }
    }
    return nil;
}

if (!contains(mcdu, 'flightPhaseChecker')) {
    mcdu.flightPhaseChecker = maketimer(1, func { mcdu.fms._checkFlightPhase(); });
    mcdu.flightPhaseChecker.simulatedTime = 1;
}

var checkFlightPhase = func {
    var groundspeed = getprop('/velocities/groundspeed-kt');
    var altitude = getprop('/instrumentation/altimeter/indicated-altitude-ft');
    var agl = getprop('/position/altitude-agl-ft');
    var vspeed = getprop('/instrumentation/vertical-speed-indicator/indicated-speed-fpm');
    var wow = getprop('/gear/gear/wow');
    var phasePrev = mcdu.flightPhase;
    if (mcdu.flightPhase == ON_STAND) {
        if (groundspeed > 5)
            mcdu.flightPhase = TAXI_OUT;
    }
    if (mcdu.flightPhase == TAXI_OUT) {
        if (groundspeed > 40)
            mcdu.flightPhase = TAKEOFF;
    }
    if (mcdu.flightPhase == TAKEOFF or mcdu.flightPhase == GO_AROUND) {
        if (agl > 800)
            mcdu.flightPhase = CLIMB;
    }
    if (mcdu.flightPhase == CLIMB) {
        var cruiseAlt = getprop('/autopilot/route-manager/cruise/altitude-ft');
        if (cruiseAlt == nil or cruiseAlt == 0) {
            cruiseAlt = getprop('/autopilot/route-manager/cruise/flight-level');
            if (cruiseAlt != nil)
                cruiseAlt *= 100;
        }
        if (cruiseAlt > 0) {
            if (altitude >= cruiseAlt - 100) {
                mcdu.flightPhase = CRUISE;
            }
        }
        else {
            if (altitude >= 4000 and vspeed < 100) {
                mcdu.flightPhase = CRUISE;
            }
        }
    }
    if (mcdu.flightPhase == CRUISE) {
        var cruiseAlt = getprop('/autopilot/route-manager/cruise/altitude-ft');
        if (cruiseAlt == nil or cruiseAlt == 0) {
            cruiseAlt = getprop('/autopilot/route-manager/cruise/flight-level');
            if (cruiseAlt != nil)
                cruiseAlt *= 100;
        }
        if (cruiseAlt > 0) {
            if (altitude < cruiseAlt - 500) {
                mcdu.flightPhase = DESCENT;
            }
        }
        else {
            if (vspeed < -400) {
                mcdu.flightPhase = DESCENT;
            }
        }
    }
    if (mcdu.flightPhase == DESCENT) {
        if (agl < 3000) {
            mcdu.flightPhase = APPROACH;
        }
    }
    if (mcdu.flightPhase == APPROACH) {
        if (wow and groundspeed < 40) {
            mcdu.flightPhase = TAXI_IN;
        }
        elsif (vspeed > 300) {
            mcdu.flightPhase = GO_AROUND;
        }
    }
    if (mcdu.flightPhase == TAXI_IN) {
        var enginesRunning = 0;
        foreach (var e; props.globals.getNode('engines').getChildren('engine')) {
            isRunning = e.getBoolValue('running');
            if (isRunning)
                enginesRunning += 1;
        }
        if (enginesRunning == 0 and groundspeed < 5) {
            mcdu.flightPhase = ON_STAND;
        }
    }
    if (mcdu.flightPhase != phasePrev) {
        printf("%s -> %s", flightPhaseName[phasePrev], flightPhaseName[mcdu.flightPhase]);
    }
    calcTOC();
    calcTOD();
    checkFlightPhaseAlerts(mcdu.flightPhase);
};

var checkMinimumTakeoffFuel = func {
    if (mcdu.flightPhase == TAXI_OUT) {
        var fuelOnBoard = fuelSamplerVars.currentKG;
        if (!contains(mcdu, 'perfInitData') or
            !contains(mcdu.perfInitData, 'toFuel') or
            mcdu.perfInitData.toFuel == nil or
            fuelOnBoard == nil) {
            clearAlert('LOW TAKEOFF FUEL');
        }
        elsif (fuelOnBoard < mcdu.perfInitData.toFuel) {
            setAlert('LOW TAKEOFF FUEL');
        }
        else {
            clearAlert('LOW TAKEOFF FUEL');
        }
    }
    else {
        clearAlert('LOW TAKEOFF FUEL');
    }
};

var checkCurrentFuel = func {
    if (mcdu.flightPhase > TAKEOFF) {
        var fuelOnBoard = fuelSamplerVars.currentKG;
        if (!contains(mcdu, 'perfInitData') or
            mcdu.perfInitData['contFuel'] == nil or
            mcdu.perfInitData['reserveFuel'] == nil or
            fuelOnBoard == nil) {
            clearAlert('FUEL WARN');
            clearAlert('FUEL EMERGENCY');
        }
        elsif (fuelOnBoard < mcdu.perfInitData.reserveFuel) {
            clearAlert('FUEL WARN');
            setAlert('FUEL EMERGENCY');
        }
        elsif (fuelOnBoard < mcdu.perfInitData.reserveFuel + mcdu.perfInitData.contFuel) {
            setAlert('FUEL WARN');
            clearAlert('FUEL EMERGENCY');
        }
        else {
            clearAlert('FUEL WARN');
            clearAlert('FUEL EMERGENCY');
        }
    }
    else {
            clearAlert('FUEL WARN');
            clearAlert('FUEL EMERGENCY');
    }
};

var checkFlightPhaseAlerts = func {
    checkMinimumTakeoffFuel();
    checkCurrentFuel();
};

var getFlightPhase = func {
    return mcdu.flightPhase;
};

var setFlightPhase = func (p) {
    mcdu.flightPhase = p;
    return nil;
};

var fuelSamplerVars = {
    deltat: 10,
    lastKG: nil,
    currentKG: nil,
    ffKG: nil,
    propKG: props.globals.getNode('/consumables/fuel/total-fuel-kg'),
};

mcdu.fuelSampler.restart(fuelSamplerVars.deltat);
mcdu.flightPhaseChecker.restart(1);

var updateFuelSampler = func {
    fuelSamplerVars.lastKG = fuelSamplerVars.currentKG;
    fuelSamplerVars.currentKG = fuelSamplerVars.propKG.getValue();
    if (fuelSamplerVars.lastKG != nil) {
        fuelSamplerVars.ffKG =
            (fuelSamplerVars.currentKG - fuelSamplerVars.lastKG) /
            fuelSamplerVars.deltat;
    }
};

# Update fuel sampler twice, so that the current fuel field is already filled
updateFuelSampler();
updateFuelSampler();

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
    return fuelSamplerVars.ffKG;
};

var getFuelOnBoard = func {
    return fuelSamplerVars.currentKG;
};

var hasFlightplanModifications = func {
    return (mcdu.modifiedFlightplan != nil);
};

var reportFlightplanCurrent = func (label='---') {
    var fp = flightplan();
    var mfp = mcdu.modifiedFlightplan;
    if (fp != nil)
        printf("%s: Active FP: %i/%i", label, fp.current, fp.getPlanSize());
    if (mfp != nil)
        printf("%s: Modified FP: %i/%i", label, mfp.current, mfp.getPlanSize());
};

var getModifyableFlightplan = func {
    # reportFlightplanCurrent('getModifyableFlightplan[pre]');
    if (mcdu.modifiedFlightplan == nil) {
        mcdu.modifiedFlightplan = flightplan().clone();
        mcdu.modifiedFlightplan.current = flightplan().current;
    }
    # reportFlightplanCurrent('getModifyableFlightplan[post]');
    return mcdu.modifiedFlightplan;
};

var getVisibleFlightplan = func {
    if (mcdu.modifiedFlightplan != nil)
        return mcdu.modifiedFlightplan;
    else
        return flightplan();
};

var cancelFlightplanEdits = func {
    mcdu.modifiedFlightplan = nil;
};

var commitFlightplanEdits = func {
    # reportFlightplanCurrent('commitFlightplanEdits[pre]');
    var current = math.max(1, mcdu.modifiedFlightplan.current);
    # TODO: if modifiedFlightplan.current < 0, then that means we have
    # deleted the previously-current waypoint from the flightplan.
    # Right now, the "solution" we pick is to just rewind the flightplan to the
    # start of the route, but of course that is not the correct way.
    mcdu.modifiedFlightplan.activate();
    # reportFlightplanCurrent('commitFlightplanEdits[after activate()]');
    fgcommand("activate-flightplan", props.Node.new({"activate": 1}));
    # reportFlightplanCurrent('commitFlightplanEdits[after fgcommand(activate)]');
    if (current < flightplan().getPlanSize())
        flightplan().current = current;
    elsif (flightplan().getPlanSize() >= 2)
        flightplan().current = 1;
    else
        flightplan().current = -1;
    mcdu.modifiedFlightplan = nil;
    calcTOD();
    calcTOC();
    # reportFlightplanCurrent('commitFlightplanEdits[post]');
    return nil;
};

var getGroundspeed = func {
    return getprop('/velocities/groundspeed-kt');
};

var getUTCMinutes = func {
    var hour = getprop('/sim/time/utc/hour');
    var minute = getprop('/sim/time/utc/minute');
    var second = getprop('/sim/time/utc/second');
    return (hour * 60 + minute + second / 60);
};

var makeLegInfo = func (idx, wp, totalDistance, totalDistanceRemaining, fuelOnBoard, groundspeed, fuelFlow) {
    var parent_id = nil;
    if (wp.wp_parent != nil)
        parent_id = wp.wp_parent.id;
    var distanceRemaining = totalDistance - wp.distance_along_route;
    var distanceToWP = totalDistanceRemaining - distanceRemaining;
    var fuelRemaining = nil;
    var fuelGS = math.max(40, groundspeed);
    if (fuelFlow != nil and fuelOnBoard != nil) {
        fuelRemaining = fuelOnBoard + fuelFlow * distanceToWP / fuelGS * 3600;
    }
    var timeRemaining = nil;
    # if (groundspeed > 40)
        timeRemaining = distanceToWP * 60 / fuelGS;
    return removeNilFields(
        { "name": wp.wp_name
        , "hdg": math.round(wp.leg_bearing)
        , "ldist": wp.leg_distance
        , "cdist": wp.distance_along_route
        , "rdist": distanceToWP
        , "spd": wp.speed_cstr
        , "spdty": wp.speed_cstr_type
        , "alt": wp.alt_cstr
        , "altty": wp.alt_cstr_type
        , "p": parent_id
        , "role": wp.wp_role
        , "efob": fuelRemaining
        , "ete": timeRemaining
        , "disc": (wp.wp_type == "discontinuity" or wp.wp_type == "vectors")
        , "ifrom": idx
        , "ito": idx
        });
};

var makeVirtualLegInfo = func (name, distanceAlongRoute, totalDistance, totalDistanceRemaining, fuelOnBoard, groundspeed, fuelFlow) {
    var distanceRemaining = totalDistance - distanceAlongRoute;
    var distanceToWP = totalDistanceRemaining - distanceRemaining;
    var fuelRemaining = nil;
    var fuelGS = math.max(40, groundspeed);
    if (fuelFlow != nil and fuelOnBoard != nil) {
        fuelRemaining = fuelOnBoard + fuelFlow * distanceToWP / fuelGS * 3600;
    }
    var timeRemaining = nil;
    # if (groundspeed > 40)
        timeRemaining = distanceToWP * 60 / fuelGS;
    return removeNilFields(
        { "name": name
        , "cdist": distanceAlongRoute
        , "rdist": distanceToWP
        , "efob": fuelRemaining
        , "ete": timeRemaining
        , "ifrom": -1
        , "ito": -1
        });
};

var getFlightplanSize = func {
    var current = nil;
    var fp = fms.getVisibleFlightplan();

    var n = 0;
    for (var i = 0; i < fp.getPlanSize(); i += 1) {
        var wp = fp.getWP(i);
        if (current != nil and
                current.name == wp.wp_name and
                wp.leg_distance < 0.001) {
            # skip
        }
        else {
            if (current != nil)
                n += 1;
            current =
                { "name": wp.wp_name
                }
        }
    }
    if (current != nil)
        n += 1;
    return n;
};

var getFlightplanLegs = func (pageSize = nil, curPage = nil, offset = nil) {
    var acpos = geo.aircraft_position();
    var fp = fms.getVisibleFlightplan();
    var result = [];
    var groundspeed = getprop('/velocities/groundspeed-kt');
    var fuelGS = math.max(40, groundspeed);
    var distanceToCurrent = getprop('/autopilot/route-manager/wp/dist');
    var totalDistanceRemaining = getprop('/autopilot/route-manager/distance-remaining-nm');
    var distanceRemaining = 0;
    var fuelRemaining = fuelSamplerVars.currentKG;
    var fuelFlow = fuelSamplerVars.ffKG;
    var first = 0;
    var length = fp.getPlanSize();
    if (pageSize != nil) {
        length = pageSize;
        if (curPage != nil) {
            first = curPage * pageSize;
            if (offset != nil) {
                first += offset;
            }
        }
    }
    var end = first + pageSize;

    var current = nil;

    var n = 0;
    for (var i = 0; i < fp.getPlanSize(); i += 1) {
        var wp = fp.getWP(i);
        var parent_id = nil;
        if (wp.wp_parent != nil)
            parent_id = wp.wp_parent.id;
        if (i == fp.current) {
            distanceRemaining = distanceToCurrent;
            if (fuelFlow != nil and fuelRemaining != nil) {
                fuelRemaining += fuelFlow * distanceToCurrent / fuelGS * 3600;
            }
        }
        elsif (i > fp.current) {
            distanceRemaining += wp.leg_distance;
            if (fuelFlow != nil and fuelRemaining != nil) {
                fuelRemaining += fuelFlow * wp.leg_distance / fuelGS * 3600;
            }
        }
        if (current != nil and
                current.name == wp.wp_name and
                wp.leg_distance < 0.001) {
            if (wp.speed_cstr > 0) {
                current.spd = wp.speed_cstr;
                current.spdty = wp.speed_cstr_type;
            }
            if (wp.alt_cstr > 0) {
                current.alt = wp.alt_cstr;
                current.altty = wp.alt_cstr_type;
            }
            current.ito = i;
        }
        else {
            if (current != nil and n >= first and n < end)
                append(result, removeNilFields(current));
            current =
                { "name": wp.wp_name
                , "hdg": math.round(geo.normdeg(wp.leg_bearing))
                , "ldist": wp.leg_distance
                , "cdist": wp.distance_along_route
                , "rdist": distanceRemaining
                , "spd": wp.speed_cstr
                , "spdty": wp.speed_cstr_type
                , "alt": wp.alt_cstr
                , "altty": wp.alt_cstr_type
                , "p": parent_id
                , "role": wp.wp_role
                , "efob": fuelRemaining
                , "disc": (wp.wp_type == "discontinuity" or wp.wp_type == "vectors")
                , "ifrom": i
                , "ito": i
                };
            n += 1;
        }
    }
    if (current != nil and n >= first and n < end)
        append(result, removeNilFields(current));
    return result;
};

var getDestinationIndex = func (fp = nil) {
    if (fp == nil)
        fp = fms.getVisibleFlightplan();
    for (var i = 1; i < fp.getPlanSize(); i += 1) {
        var wp = fp.getWP(i);
        if (wp != nil) {
            # print(wp.wp_name);
            # print(wp.wp_type);
            # print(wp.wp_role);
        
            if (wp.wp_type == 'runway') {
                # This happens when the destination exists and has a runway
                # selected.
                wpDest = wp;
                return i;
                break;
            }
            if (wp.wp_role == 'approach' and fp.destination != nil and
                # This is our fallback when no destination runway has been
                # selected; it will (hopefully) match the destination airport,
                # if any.
                wp.wp_name == fp.destination.id) {
                wpDest = wp;
                return i;
            }
        }
    }
    return -1;
}

var gpsNode = props.globals.getNode('/instrumentation/gps');

var getRnpInfo = func {
    var fp = flightplan();
    if (fp == nil)
        return nil;
    var wp = fp.getWP(fp.current);
    var rnp = nil;
    var totalDistanceRemaining = getprop('/autopilot/route-manager/distance-remaining-nm');
    if (wp != nil) {
        if (wp.wp_role == 'approach') {
            if (totalDistanceRemaining < 8.0)
                rnp = 0.3;
            else
                rnp = 1.0;
        }
        elsif (wp.wp_role == 'sid' or wp.wp_role == 'star') {
            rnp = 1.0;
        }
        else {
            rnp = 5.0;
        }
    }

    var anp = nil;
    var sensorName = nil;

    if (gpsNode != nil and gpsNode.getValue('serviceable')) {
        var gpsPosition = geo.Coord.new();
        gpsPosition.set_latlon(
            gpsNode.getValue('indicated-latitude-deg'), 
            gpsNode.getValue('indicated-longitude-deg'));
        var actualPosition = geo.aircraft_position();
        var distError = actualPosition.distance_to(gpsPosition);
        anp = distError * 2.0;
        sensorName = 'GPS-D';
    }
    if (rnp == nil and anp == nil and sensor == nil)
        return nil;
    else
        return removeNilFields({
            "rnp": rnp,
            "anp": anp,
            "sensor": sensorName
        });
}

var getProgressInfo = func () {
    var fp = fms.getVisibleFlightplan();
    var groundspeed = getprop('/velocities/groundspeed-kt');
    var fuelGS = math.max(40, groundspeed);
    var totalDistanceRemaining = getprop('/autopilot/route-manager/distance-remaining-nm');
    var totalDistance = getprop('/autopilot/route-manager/total-distance');
    var fuelOnBoard = fuelSamplerVars.currentKG;
    var fuelFlow = fuelSamplerVars.ffKG;
    var first = 0;
    var length = fp.getPlanSize();

    var wpCurrent = fp.getWP(fp.current);
    var wpNext = fp.getWP(fp.current + 1);
    var destIdx = getDestinationIndex(fp);
    var wpDest = (destIdx == nil) ? nil : fp.getWP(destIdx);

    var info = {};
    if (wpCurrent != nil)
        info.current = makeLegInfo(fp.current, wpCurrent, totalDistance, totalDistanceRemaining, fuelOnBoard, fuelGS, fuelFlow);
    if (wpNext != nil)
        info.next = makeLegInfo(fp.current + 1, wpNext, totalDistance, totalDistanceRemaining, fuelOnBoard, fuelGS, fuelFlow);
    if (wpDest != nil)
        info.destination = makeLegInfo(destIdx, wpDest, totalDistance, totalDistanceRemaining, fuelOnBoard, fuelGS, fuelFlow);
    info.fob = fuelOnBoard;
    if (grossWeightProp != nil)
        info.gw = grossWeightProp.getValue() * LB2KG;
    info.phase = mcdu.flightPhase;
    if (contains(mcdu, 'vnav')) {
        if (mcdu.vnav['tocDist'] != nil)
            info.toc = makeVirtualLegInfo('TOC', totalDistance - mcdu.vnav.tocDist, totalDistance, totalDistanceRemaining, fuelOnBoard, fuelGS, fuelFlow);
        if (mcdu.vnav['todDist'] != nil)
            info.tod = makeVirtualLegInfo('TOD', totalDistance - mcdu.vnav.todDist, totalDistance, totalDistanceRemaining, fuelOnBoard, fuelGS, fuelFlow);
    }
    info.rnp = getRnpInfo();
    var alerts = [];
    foreach (var k; keys(mcdu.alerts)) {
        if (mcdu.alerts[k] > 0)
            append(alerts, k);
    }
    if (size(alerts) > 0) {
        info.alerts = alerts;
    }
    return info;
};


var setLegAltitude = func (n, alt, altType) {
    var wp = fms.getVisibleFlightplan().getWP(n);
    if (wp == nil)
        return "NO WPT";
    wp = fms.getModifyableFlightplan().getWP(n);
    wp.setAltitude(alt, altType);
    return nil;
};

var setLegSpeed = func (n, speed, speedType) {
    var wp = fms.getVisibleFlightplan().getWP(n);
    if (wp == nil)
        return "NO WPT";
    wp = fms.getModifyableFlightplan().getWP(n);
    wp.setSpeed(speed, speedType);
    return nil;
};

var removeNilFields = func (h) {
    foreach (var k; keys(h)) {
        if (h[k] == nil)
            delete(h, k);
    }
    return h;
};

var getWaypoint = func (index) {
    var fp = fms.getVisibleFlightplan();
    var wp = fp.getWP(index);
    if (wp == nil)
        return nil;
    else
        return completeWaypoint({ "type": "leg" , "id": wp.id, "name": wp.wp_name, "wp": wp });
};

var getFPLegIndex = func (needle) {
    var fp = fms.getVisibleFlightplan();
    for (var i = fp.current or 0; i < fp.getPlanSize(); i += 1) {
        var wp = fp.getWP(i);
        if (wp.wp_name == needle)
            return completeWaypoint({ "type": "leg" , "id": wp.id, "name": wp.wp_name, "wp": wp });
    }
    return nil;
};

var findWaypoint = func (includeLegs, needle) {
    var fp = fms.getVisibleFlightplan();
    var acpos = geo.aircraft_position();
    var results = [];
    if (includeLegs) {
        for (var i = math.max(0, fp.current or 0); i < fp.getPlanSize(); i += 1) {
            var wp = fp.getWP(i);
            if (wp.wp_name == needle)
                append(results,
                    completeWaypoint({ "type": "leg" , "id": wp.id, "name": wp.wp_name, "wp": wp }, acpos));
        }
    }
    if (size(needle) == 4) {
        var airports = findAirportsByICAO(needle);
        foreach (var airport; airports) {
            append(results,
                completeWaypoint({ "type": "airport", "id": wp.id, "name": airport.name, "wp": airport }, acpos));
        }
    }
    else {
        var fixes = findFixesByID(acpos, needle);
        foreach (var fix; fixes) {
            if (fix.id == needle)
                append(results,
                    completeWaypoint({ "type": "fix", "id": fix.id, "name": fix.id, "wp": fix }, acpos));
        }
        var vors = findNavaidsByID(acpos, needle, 'vor');
        foreach (var vor; vors) {
            if (vor.id == needle)
                append(results,
                    completeWaypoint({ "type": vor.type, "id": vor.id, "name": vor.name, "wp": vor }, acpos));
        }
        var ndbs = findNavaidsByID(acpos, needle, 'ndb');
        foreach (var ndb; ndbs) {
            if (ndb.id == needle)
                append(results,
                    completeWaypoint({ "type": ndb.type, "id": ndb.id, "name": ndb.name, "wp": ndb }, acpos));
        }
        var dmes = findNavaidsByID(acpos, needle, 'dme');
        foreach (var dme; dmes) {
            if (dme.id == needle)
                append(results,
                    completeWaypoint({ "type": dme.type, "id": dme.id, "name": dme.name, "wp": dme }, acpos));
        }
    }
    var sortfunc = func (a, b) {
        # Sort FP legs first
        if (a.type == "leg" and b.type != "leg")
            return -1;
        elsif (b.type == "leg" and a.type != "leg")
            return 1;
        # Show VORs before other navaids
        elsif (a.distance < b.distance - 0.01)
            return -1;
        elsif (a.distance > b.distance + 0.01)
            return 1;
        elsif (a.type == "VOR" and b.type != "VOR")
            return -1;
        elsif (b.type == "VOR" and a.type != "VOR")
            return 1;
        else
            return 0;
    };
    return sort(results, sortfunc);
};

var completeWaypoint = func (wp, acpos = nil) {
    if (acpos == nil) {
        acpos = geo.aircraft_position();
    }
    var wpPos = geo.Coord.new();
    wpPos.set_latlon(wp.wp.lat, wp.wp.lon);
    wp.distance = acpos.distance_to(wpPos) * M2NM;
    wp.bearing = acpos.course_to(wpPos);
    return wp;
};

var getCurrentLeg = func {
    var current = nil;
    var fp = fms.getVisibleFlightplan();

    var n = 0;
    var i = 0;
    for (i = 0; i < fp.getPlanSize(); i += 1) {
        var wp = fp.getWP(i);
        if (current != nil and
                current.name == wp.wp_name and
                wp.leg_distance < 0.001) {
            # skip
        }
        else {
            if (current != nil)
                n += 1;
            current =
                { "name": wp.wp_name
                , "lat": wp.lat
                , "lon": wp.lon
                }
        }
        if (fp.current == i)
            return n + 1;
    }
    if (current != nil and n >= first and n < end)
        n += 1;
    return n + 1;
};


var getWaypointName = func (i) {
    var fp = fms.getVisibleFlightplan();
    var wp = fp.getWP(i);
    if (wp == nil) return nil;
    return wp.wp_name;
};

var deleteWaypoint = func (i) {
    var fp = fms.getModifyableFlightplan();
    if (i >= fp.getPlanSize() - 1)
        return 0;
    if (i <= 0)
        return 0;
    fp.deleteWP(i);
    return 1;
};

var findFPWaypoint = func (fp, wp) {
    if (fp == nil)
        fp = fms.getVisibleFlightplan();
    if (wp == nil)
        return nil;
    var fpWP = nil;
    var fpIndex = nil;
    for (var i = fp.current; i < fp.getPlanSize(); i += 1) {
        fpWP = fp.getWP(i);
        if (fpWP.lat == wp.lat and fpWP.lon == wp.lon and fpWP.id == wp.id) {
            fpIndex = i;
            break;
        }
    }
    return fpIndex;
};

var insertDirect = func (wp, from) {
    var fp = fms.getModifyableFlightplan();
    var acpos = geo.aircraft_position();
    if (typeof(from) == 'ghost') {
        # print("from is ghost, finding in flightplan");
        from = findFPWaypoint(fp, from);
    }
    if (from == nil) {
        var direct = createWP(acpos.lat(), acpos.lon(), "DIRECT");
        fp.insertWP(direct, fp.current);
        var newWP = createWP(wp.lat, wp.lon, wp.id);
        fp.insertWP(newWP, fp.current);
        fp.insertWP(createDiscontinuity(), fp.current);
        fp.current -= 2;
    }
    else {
        var newWP = createWP(wp.lat, wp.lon, wp.id);
        fp.insertWP(newWP, from + 1);
        fp.insertWP(createDiscontinuity(), from + 2);
    }
    return nil;
};

var getRoute = func {
    print("getRoute");
    var fp = fms.getVisibleFlightplan();
    var curParent = 'DCT';
    var curWP = nil;
    var entries = [];
    var dist = nil;
    var destinationName = (fp.destination == nil) ? nil : fp.destination.id;
    var totalDistance = getprop('/autopilot/route-manager/total-distance');

    var lastSid = nil;
    var lastSidWP = nil;
    var firstEnroute = nil;
    var firstEnrouteWP = nil;
    var lastEnroute = nil;
    var lastEnrouteWP = nil;
    var firstArrival = nil;
    var firstArrivalWP = nil;

    for (var i = 1; i < fp.getPlanSize(); i += 1) {
        var wp = fp.getWP(i);
        if (wp.wp_role == 'sid') {
            lastSid = i;
            lastSidWP = wp;
        }
        elsif (firstEnroute == nil and wp.wp_role == nil) {
            firstEnroute = i;
            firstEnrouteWP = wp;
        }
        elsif (wp.wp_role == nil) {
            lastEnroute = i;
            lastEnrouteWP = wp;
        }
        elsif (firstArrival == nil and (wp.wp_role != nil or wp.id == destinationName)) {
            firstArrival = i;
            firstArrivalWP = wp;
        }
    }
    if (firstEnroute == nil)
        firstEnroute = 1;
    if (firstArrival == nil) {
        firstArrival = fp.getPlanSize() - 1;
        firstArrivalWP = fp.getWP(firstArrival);
    }

    var rteClosed = 0;
    if (firstArrivalWP != nil) {
        if (lastEnrouteWP != nil) {
            rteClosed = lastEnrouteWP.id == firstArrivalWP.id;
        }
        elsif (lastSidWP != nil) {
            rteClosed = lastSidWP.id == firstArrivalWP.id;
        }
    }

    var lastIndex = firstEnroute;
    var isSid = 0;
    for (var i = firstEnroute; i < firstArrival; i += 1) {
        var wp = fp.getWP(i);

        if (dist == nil)
            dist = wp.distance_along_route;
        var parent = wp.wp_parent;
        var parentName = 'DCT';
        if (i == firstEnroute and lastSidWP != nil and lastSidWP.id == wp.id) {
            if (fp.sid != nil) {
                parentName = fp.sid.id;
                if (fp.sid_trans != nil) {
                    parentName ~= "." ~ fp.sid_trans.id;
                }
            }
        }
        if (parent != nil)
            parentName = parent.id;
        # printf("%s %s", parentName, wp.wp_name);
        if (parentName != curParent or parentName == 'DCT') {
            if (curParent != nil and curWP != nil) {
                var entry = { 'via': curParent, 'to': curWP.id, 'dist': dist, 'fromIndex': lastIndex, 'toIndex': i };
                if (isSid)
                    entry['is'] = 'sid';
                append(entries, entry);
                dist = 0;
                lastIndex = i;
            }
        }
        curParent = parentName;
        curWP = wp;
        isSid = (i == firstEnroute);
        dist += wp.leg_distance;
    }
    if (firstEnrouteWP == nil and fp.sid != nil) {
        isSid = 1;
    }
    if (curParent != nil and curWP != nil and dist > 0) {
        var entry = { 'via': curParent, 'to': curWP.id, 'dist': dist, 'fromIndex': lastIndex, 'toIndex': firstArrival };
        if (isSid)
            entry['is'] = 'sid';
        append(entries, entry);
    }
    if (!rteClosed)
        append(entries, nil);
    if (fp.destination != nil) {
        var destinationIndex = getDestinationIndex(fp);
        var destinationWP = fp.getWP(destinationIndex);
        var parentNameParts = [];
        if (fp.star_trans != nil and fp.star == nil) {
            append(parentNameParts, fp.star_trans.id);
        }
        if (fp.star != nil) {
            append(parentNameParts, fp.star.id);
        }
        if (fp.approach_trans != nil) {
            append(parentNameParts, fp.approach_trans.id);
        }
        if (fp.approach_trans != nil) {
            append(parentNameParts, fp.approach_trans.id);
        }
        var parentName = 'DCT';
        if (size(parentNameParts) > 0) {
            parentName = string.join('.', parentNameParts);
        }

        var destname = fp.destination.id;
        if (fp.destination_runway != nil)
         destname ~= fp.destination_runway.id;
        if (lastEnrouteWP == nil) {
            dist = 0;
        }
        else {
            dist = destinationWP.distance_along_route - lastEnrouteWP.distance_along_route;
        }
        append(entries, { 'is': 'star', 'via': parentName, 'to': destname, 'dist': dist, 'fromIndex': firstArrival, 'toIndex': destinationIndex });
    }
    return entries;
};

var deleteRouteLeg = func (fromIndex, toIndex) {
    var fp = fms.getVisibleFlightplan();
    var destinationName = (fp.destination == nil) ? nil : fp.destination.id;

    var count = toIndex - fromIndex;
    var fp = fms.getModifyableFlightplan();

    var nextWP = fp.getWP(toIndex);
    var lastWP = fp.getWP(toIndex - 1);
    # printf("Next WP: %i %s %s %s",
    #     toIndex,
    #     nextWP ? nextWP.id : '---',
    #     nextWP ? nextWP.wp_role : '---',
    #     (nextWP and nextWP.wp_parent) ? nextWP.wp_parent.id : '---');
    var newWP = createWP({lat: lastWP.lat, lon: lastWP.lon}, lastWP.id);

    while (count > 0 and fromIndex < fp.getPlanSize()) {
        # printf("DELETE %s", fp.getWP(fromIndex).id);
        fp.deleteWP(fromIndex);
        count -= 1;
    }

    if (nextWP.wp_parent != nil and nextWP.wp_role != 'star' and nextWP.wp_role != 'approach') {
        fp.insertWP(newWP, fromIndex);
    }
    return nil;
};

var insertDirectFP = func (toWP, fromWP) {
    var fp = fms.getVisibleFlightplan();
    var acpos = geo.aircraft_position();
    toIndex = findFPWaypoint(fp, toWP);
    fromIndex = findFPWaypoint(fp, fromWP);
    if (toIndex == nil) {
        insertDirect(toWP, fromIndex);
    }
    elsif (fromIndex == nil) {
        var fp = fms.getModifyableFlightplan();
        var direct = createWP(acpos.lat(), acpos.lon(), "DIRECT");
        fp.insertWP(direct, toIndex);
        fp.current = toIndex + 1;
    }
    else {
        if (toIndex <= fromIndex)
            return "INVALID SEQ";
        var fp = fms.getModifyableFlightplan();
        var fpIndex = fp.current;
        if (fpIndex > fromIndex and fpIndex < toIndex) {
            fpIndex = fromIndex + 1;
        }
        for (var i = fromIndex + 1; i < toIndex; i += 1) {
            fp.deleteWP(fromIndex + 1);
        }
        fp.insertWP(createDiscontinuity(), fromIndex + 1);
        fp.current = fpIndex;
    }
    return nil;
};

appendViaTo = func (via, toWP) {
    var err = [];
    var leg = call(createViaTo, [via, toWP], nil, nil, err);
    if (size(err) > 0) {
        debug.dump(err);
        if (string.match(err[0], "createViaTo: couldn't find airway with provided name:*"))
            return "NO AIRWAY"
        elsif (err[0] == "createViaTo: navaid not on airway")
            return "NOT ON AIRWAY"
        else
            return "INVALID"
    }
    var fp = fms.getVisibleFlightplan();
    var insertIndex = 0;

    for (var i = 1; i < fp.getPlanSize(); i += 1) {
        var wp = fp.getWP(i);
        if (wp != nil) {
            # print(wp.wp_name);
            # print(wp.wp_type);
            # print(wp.wp_role);
        
            if (wp.wp_type == 'runway' or
                    wp.wp_role == 'approach' or
                    wp.wp_role == 'star') {
                insertIndex = i;
                break;
            }
        }
    }

    fp = fms.getModifyableFlightplan();

    var current = fp.current;
    if (insertIndex != nil) {
        call(fp.insertWPAfter, [leg, insertIndex - 1], fp, {}, err);
        if (size(err) > 0) {
            debug.dump(err);
            return "INVALID"
        }
    }
    fp.current = current;
    return nil;
};

appendDirectTo = func (toWP) {
    var leg = createWPFrom(toWP);
    var fp = fms.getVisibleFlightplan();
    var insertIndex = 0;

    for (var i = 1; i < fp.getPlanSize(); i += 1) {
        var wp = fp.getWP(i);
        if (wp != nil) {
            # print(wp.wp_name);
            # print(wp.wp_type);
            # print(wp.wp_role);
        
            if (wp.wp_type == 'runway' or
                    wp.wp_role == 'approach' or
                    wp.wp_role == 'star') {
                insertIndex = i;
                break;
            }
        }
    }

    fp = fms.getModifyableFlightplan();

    var current = fp.current;
    if (insertIndex != nil) {
        fp.insertWPAfter(leg, insertIndex - 1);
    }
    fp.current = current;
    return nil;
};

var clearFlightplan = func {
    var fp = fms.getModifyableFlightplan();
    fp.cleanPlan();
    fp.departure = nil;
    fp.sid = nil;
    fp.sid_trans = nil;
    fp.destination = nil;
    fp.star = nil;
    fp.star_trans = nil;
    fp.approach = nil;
    fp.approach_trans = nil;
    return 1;
};

var setDepartureRunway = func (runwayID) {
    var fp = getModifyableFlightplan();
    if (runwayID == nil) {
        var departureAirport = fp.departure;
        fp.departure = nil;
        fp.departure = departureAirport;
        return 1;
    }
    else {
        var departure = fp.departure;
        if (departure == nil) return 0;
        var runway = departure.runways[runwayID];
        if (runway == nil) return 0;
        fp.departure_runway = runway;
        return 1;
    }
}

var listDepartureRunways = func {
    var fp = getVisibleFlightplan();
    var runways = fp.departure.runways;
    return keys(runways);
}

var getDepartureRunway = func {
    var fp = getVisibleFlightplan();
    var rwyID = ((fp.departure_runway == nil) ? nil : fp.departure_runway.id);
    return rwyID
}

var setSID = func (sidID) {
    var fp = getModifyableFlightplan();
    if (sidID == nil) {
        fp.sid = nil;
        return nil;
    }
    else {
        var departure = fp.departure;
        if (departure == nil) return "NO DEPARTURE";
        var sid = departure.getSid(sidID);
        if (sid == nil) return "INVALID";
        fp.sid = sid;
        var runways = fp.sid.runways;
        if (size(runways) == 1) {
          var runway = departure.runways[runways[0]];
          if (runway == nil) {
            return "INVALID RWY";
          }
          fp.departure_runway = runway;
        }
        elsif (size(runways) > 1) {
          var runway = fp.departure_runway;
          if (runway != nil and !contains(runways, runway.id)) {
            fp.departure = nil;
            fp.departure = departure;
          }
        }
        return nil;
    }
}

var listSIDs = func {
    var fp = getVisibleFlightplan();
    var departure = fp.departure;
    if (departure == nil) return [];
    var runway = fp.departure_runway;
    if (runway == nil)
      return departure.sids()
    else
      return departure.sids(runway.id);
}

var getSID = func {
    var fp = getVisibleFlightplan();
    return ((fp.sid == nil) ? nil : fp.sid.id);
}

var setSidTransition = func (transitionID) {
    var fp = getModifyableFlightplan();
    if (transitionID == nil) {
        fp.sid_trans = nil;
        return 1;
    }
    else {
        var departure = fp.departure;
        if (departure == nil) return "NO DEPARTURE";
        var sid = fp.sid;
        if (sid == nil) return "NO SID";
        var transitions = sid.transitions;
        if (!contains(transitions, transitionID)) return "INVALID";
        fp.sid_trans = sid.transition(transitionID);
        return nil;
    }
}

var listSidTransitions = func {
    var fp = getVisibleFlightplan();
    var departure = fp.departure;
    if (departure == nil) return [];
    var sid = fp.sid;
    if (sid == nil)
      return [];
    else
      return sid.transitions;
}

var getSidTransition = func {
    var fp = getVisibleFlightplan();
    return ((fp.sid_trans == nil) ? nil : fp.sid_trans.id);
}

var setDeparture = func (icao) {
    var fp = getModifyableFlightplan();
    if (icao == nil) {
        fp.departure = nil;
        return 1;
    }
    else {
        var airport = airportinfo(icao);
        if (airport == nil) return 0;
        fp.departure = airport;
        return 1;
    }
}

var getDeparture = func {
    var fp = getVisibleFlightplan();
    return ((fp.departure == nil) ? nil : fp.departure.id);
}

var setDestination = func (icao) {
    var fp = getModifyableFlightplan();
    if (icao == nil) {
        fp.destination = nil;
        return 1;
    }
    else {
        var airports = findAirportsByICAO(icao);
        if (size(airports) == 0) return 0;
        fp.destination = airports[0];
        return 1;
    }
}

var getDestination = func {
    var fp = getVisibleFlightplan();
    return ((fp.destination == nil) ? nil : fp.destination.id);
}

var setDestinationRunway = func (runwayID) {
    var fp = getModifyableFlightplan();
    if (runwayID == nil) {
        var destinationAirport = fp.destination;
        fp.destination = nil;
        fp.destination = destinationAirport;
        return 1;
    }
    else {
        var destination = fp.destination;
        if (destination == nil) return 0;
        var runway = destination.runways[runwayID];
        if (runway == nil) return 0;
        fp.destination_runway = runway;
        return 1;
    }
}

var listDestinationRunways = func {
    var fp = getVisibleFlightplan();
    var runways = fp.destination.runways;
    return keys(runways);
}

var getDestinationRunway = func {
    var fp = getVisibleFlightplan();
    var rwyID = ((fp.destination_runway == nil) ? nil : fp.destination_runway.id);
    return rwyID
}

var setSTAR = func (starID) {
    var fp = getModifyableFlightplan();
    if (starID == nil) {
        fp.star = nil;
        return nil;
    }
    else {
        var destination = fp.destination;
        if (destination == nil) return "NO DESTINATION";
        var star = destination.getStar(starID);
        if (star == nil) return "INVALID";
        fp.star = star;
        var runways = fp.star.runways;
        if (size(runways) == 1) {
          var runway = destination.runways[runways[0]];
          if (runway == nil) {
            return "INVALID RWY";
          }
          fp.destination_runway = runway;
        }
        elsif (size(runways) > 1) {
          var runway = fp.destination_runway;
          if (runway != nil and !contains(runways, runway.id)) {
            fp.destination = nil;
            fp.destination = destination;
          }
        }
        return nil;
    }
}

var listSTARs = func {
    var fp = getVisibleFlightplan();
    var destination = fp.destination;
    if (destination == nil) return [];
    var runway = fp.destination_runway;
    if (runway == nil)
      return destination.stars()
    else
      return destination.stars(runway.id);
}

var getSTAR = func {
    var fp = getVisibleFlightplan();
    return ((fp.star == nil) ? nil : fp.star.id);
}

var setStarTransition = func (transitionID) {
    var fp = getModifyableFlightplan();
    if (transitionID == nil) {
        fp.star_trans = nil;
        return nil;
    }
    else {
        var destination = fp.destination;
        if (destination == nil) return "NO DESTINATION";
        var star = fp.star;
        if (star == nil) return "NO STAR";
        var transitions = star.transitions;
        if (!contains(transitions, transitionID)) return "INVALID";
        fp.star_trans = star.transition(transitionID);
        return nil;
    }
}

var listStarTransitions = func {
    var fp = getVisibleFlightplan();
    var destination = fp.destination;
    if (destination == nil) return [];
    var star = fp.star;
    if (star == nil)
      return [];
    else
      return star.transitions;
}

var getStarTransition = func {
    var fp = getVisibleFlightplan();
    return ((fp.star_trans == nil) ? nil : fp.star_trans.id);
}

var setApproach = func (approachID) {
    var fp = getModifyableFlightplan();
    if (approachID == nil) {
        fp.approach = nil;
        return nil;
    }
    else {
        var destination = fp.destination;
        if (destination == nil) return "NO ARRIVAL";
        var approach = destination.getIAP(approachID);
        if (approach == nil) return "INVALID";
        fp.approach = approach;
        runways = fp.approach.runways;
        if (size(runways) == 1) {
            var runway = destination.runways[runways[0]];
            if (runway == nil) {
                return "INVALID RWY";
            }
            fp.destination_runway = runway;
        }
        elsif (size(runways) > 1) {
            var runway = fp.destination_runway;
            if (runway != nil and !contains(runways, runway.id)) {
                fp.destination = nil;
                fp.destination = destination;
            }
        }
        return nil;
    }
}

var listApproaches = func {
    var fp = getVisibleFlightplan();
    var destination = fp.destination;
    if (destination == nil) return [];
    var runway = fp.destination_runway;
    if (runway == nil)
        return destination.getApproachList()
    else
        return destination.getApproachList(runway.id);
}

var getApproach = func {
    var fp = getVisibleFlightplan();
    return ((fp.approach == nil) ? nil : fp.approach.id);
}

var setApproachTransition = func (transitionID) {
    var fp = getModifyableFlightplan();
    if (transitionID == nil) {
        fp.approach_trans = nil;
        return nil;
    }
    else {
        var destination = fp.destination;
        if (destination == nil) return "NO DESTINATION";
        var approach = fp.approach;
        if (approach == nil) return "NO APPROACH";
        var transitions = approach.transitions;
        if (!contains(transitions, transitionID)) return "INVALID";
        fp.approach_trans = approach.transition(transitionID);
        return nil;
    }
}


var listApproachTransitions = func {
    var fp = getVisibleFlightplan();
    var destination = fp.destination;
    if (destination == nil) return [];
    var approach = fp.approach;
    if (approach == nil)
        return [];
    else
        return approach.transitions;
}

var getApproachTransition = func {
    var fp = getVisibleFlightplan();
    return ((fp.approach_trans == nil) ? nil : fp.approach_trans.id);
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

var isValidSID = func () {
    var fp = getVisibleFlightplan();
    if (fp.departure == nil) return 1;
    if (fp.sid == nil) return 1;
    if (fp.departure_runway == nil) return 1;
    var runways = fp.sid.runways;
    var runway = fp.departure_runway.id;
    return contains(runways, runway);
}

var isValidSTAR = func () {
    var fp = getVisibleFlightplan();
    if (fp.destination == nil) return 1;
    if (fp.star == nil) return 1;
    if (fp.destination_runway == nil) return 1;
    var runways = fp.star.runways;
    var runway = fp.destination_runway.id;
    return contains(runways, runway);
}

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
                        ? machToTAS(alt, substr(perfEntry.spd ~ '', 1) / 100)
                        : iasToTAS(alt, perfEntry.spd);
        printf("%02i FL%03i SPD %3s TAS %3i %5.1fnmi", t, alt / 100, perfEntry.spd, tas, dist);
        alt += perfEntry.roc;
        dist += tas / 60;
    }
}

var calcTOC = func {
    var pressureAlt = getprop('/instrumentation/altimeter/pressure-alt-ft');
    var cruiseAlt = getprop('/autopilot/route-manager/cruise/altitude-ft');
    if (cruiseAlt == nil or cruiseAlt == 0) {
        cruiseAlt = getprop('/autopilot/route-manager/cruise/flight-level');
        if (cruiseAlt != nil)
            cruiseAlt *= 100;
    }
    if (cruiseAlt == nil) {
        mcdu.vnav.tocDist = nil;
        mcdu.vnav.tocDistRemaining = nil;
        return;
    }
    if (pressureAlt >= cruiseAlt - 100) {
        mcdu.vnav.tocDist = nil;
        mcdu.vnav.tocDistRemaining = 0;
    }
    var t = 0;
    var alt = pressureAlt;
    var distRemaining = 0;
    var perfIdx = 0;
    if (!contains(mcdu, 'perfInitData') or !contains(mcdu.perfInitData, 'climb')) {
        mcdu.vnav.tocDist = nil;
        mcdu.vnav.tocDistRemaining = nil;
        return;
    }
    printf("Cruise alt: %i", math.floor(cruiseAlt));
    var perfEntry = mcdu.perfInitData.climb[perfIdx];
    while (alt < cruiseAlt and perfEntry != nil) {
        var fl = alt / 100;
        while (perfEntry != nil and fl > perfEntry.fl) {
            printf("Next perf entry, because %f > %f", fl, perfEntry.fl);
            perfIdx += 1;
            if (perfIdx >= size(mcdu.perfInitData.climb))
                perfEntry = nil;
            else
                perfEntry = mcdu.perfInitData.climb[perfIdx];
        }
        if (perfEntry == nil) {
            mcdu.vnav.tocDist = nil;
            mcdu.vnav.tocDistRemaining = nil;
            return;
        }
        var tas = (substr(perfEntry.spd ~ '', 0, 1) == 'M')
                        ? machToTAS(alt, substr(perfEntry.spd ~ '', 1) / 100)
                        : iasToTAS(alt, perfEntry.spd);
        if (perfEntry.roc < cruiseAlt - alt) {
            alt += perfEntry.roc;
            distRemaining += tas / 60;
            t += 1;
        }
        else {
            var dt = (cruiseAlt - alt) / perfEntry.roc;
            distRemaining += tas * perfEntry.roc / (cruiseAlt - alt) / 60;
            alt = cruiseAlt;
            t += dt;
        }
        var minutes = math.fmod(math.floor(t), 60);
        var hours = math.floor(t / 60);
        printf("%02i%02i FL%03i TAS %3i dist %1.1fnmi", hours, minutes, math.round(alt / 100), math.round(tas), distRemaining);
    }
    mcdu.vnav.tocDistRemaining = distRemaining;
    var totalDistance = getprop('/autopilot/route-manager/total-distance');
    var totalDistanceRemaining = getprop('/autopilot/route-manager/distance-remaining-nm');
    var distanceTravelled = totalDistance - totalDistanceRemaining;
    mcdu.vnav.tocDist = distRemaining + distanceTravelled;
}

var getTransitionAlt = func {
    if (!contains(mcdu, 'perfInitData') or !contains(mcdu.perfInitData, 'transAlt'))
        return 18000;
    else
        return mcdu.perfInitData.transAlt;
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

    calcTOD();
    calcTOC();

    return nil;
}

var calcTOD = func {
    var fpa = mcdu.perfInitData['desFPA'];
    if (fpa == nil)
        fpa = 3.0;
    var destAlt = 0;
    var fp = flightplan();
    if (fp.destination != nil) {
        destAlt = (fp.destination.elevation or 0) * M2FT;
    }
    var cruiseAlt = getprop('/autopilot/route-manager/cruise/altitude-ft');
    var cruiseFL = getprop('/autopilot/route-manager/cruise/flight-level');
    if (cruiseFL > 0)
        cruiseAlt = cruiseFL * 100;
    printf("Cruise alt: %s", cruiseAlt);
    printf("Dest. alt: %s", destAlt);
    printf("FPA: %s", fpa);
    var criticalWP = nil;
    if (cruiseAlt == 0 or fpa < 0.01) {
        mcdu.vnav.todDistNominal = nil;
        mcdu.vnav.todDist = nil;
    }
    else {
        mcdu.vnav.todDistNominal = (cruiseAlt - destAlt) * FT2M / math.sin(D2R * fpa) * M2NM;
        mcdu.vnav.todDist = (cruiseAlt - destAlt) * FT2M / math.sin(D2R * fpa) * M2NM;
        var i = 0;
        for (i = 1; i < fp.getPlanSize(); i += 1) {
            var wp = fp.getWP(i);
            if (wp == nil or wp.wp_role == nil)
                break;
        }
        while (i < fp.getPlanSize()) {
            var wp = fp.getWP(i);
            if (wp == nil or wp.wp_role == 'missed')
                break;
            if (wp.alt_cstr_type == "at" or wp.alt_cstr_type == "below" or (wp.wp_role == "approach" and wp.wp_parent == nil)) {
                var constraintAlt = (wp.wp_role == "runway") ? destAlt : wp.alt_cstr;
                var totalDistance = getprop('/autopilot/route-manager/total-distance');
                var distToGo = totalDistance - wp.distance_along_route;
                var todWP = (cruiseAlt - constraintAlt) * FT2M / math.sin(D2R * fpa) * M2NM + distToGo;
                printf("%s %s %1.0i (%1.1fnmi to go) -> TOD = %1.1fnmi",
                    wp.id,
                    wp.alt_cstr_type,
                    wp.alt_cstr,
                    distToGo,
                    todWP);
                if (todWP > mcdu.vnav.todDist) {
                    criticalWP = wp;
                    mcdu.vnav.todDist = todWP;
                }
            }
            i += 1;
        }
        if (mcdu.vnav.todDistNominal == nil)
            printf("Nominal TOD to destination: unknown.");
        else
            printf("Nominal TOD to destination: %1.1fnmi", mcdu.vnav.todDistNominal);
        if (mcdu.vnav.todDist == nil)
            printf("Effective TOD to destination: unknown.");
        else
            printf("Effective TOD to destination: %1.1fnmi, based on %s",
                mcdu.vnav.todDist,
                (criticalWP == nil) ? "destination" : criticalWP.id);
    }
}

var fms = {
    '_updateFuelSampler': updateFuelSampler,
    '_checkFlightPhase': checkFlightPhase,
    'getFlightPhase': getFlightPhase,
    'setFlightPhase': setFlightPhase,

    'hasFlightplanModifications': hasFlightplanModifications,
    'getModifyableFlightplan': getModifyableFlightplan,
    'getVisibleFlightplan': getVisibleFlightplan,
    'cancelFlightplanEdits': cancelFlightplanEdits,
    'commitFlightplanEdits': commitFlightplanEdits,

    'getFGCallsign': getFGCallsign,
    'setFGCallsign': setFGCallsign,
    'getAircraftType': getFGAircraftType,

    'getWaypointName': getWaypointName,
    'deleteWaypoint': deleteWaypoint,
    'clearFlightplan': clearFlightplan,
    'findWaypoint': findWaypoint,
    'findFPWaypoint': findFPWaypoint,
    'getFPLegIndex': getFPLegIndex,
    'getWaypoint': getWaypoint,
    'insertDirect': insertDirect,
    'insertDirectFP': insertDirectFP,
    'setLegAltitude': setLegAltitude,
    'setLegSpeed': setLegSpeed,

    'getRoute': getRoute,
    'appendViaTo': appendViaTo,
    'appendDirectTo': appendDirectTo,
    'deleteRouteLeg': deleteRouteLeg,

    'getGroundspeed': getGroundspeed,
    'getFuelFlow': getFuelFlow,
    'getFuelOnBoard': getFuelOnBoard,
    'getFuelCapacity': getFuelCapacity,
    'getUTCMinutes': getUTCMinutes,
    'getFlightplanLegs': getFlightplanLegs,
    'getFlightplanSize': getFlightplanSize,
    'getCurrentLeg': getCurrentLeg,
    'getProgressInfo': getProgressInfo,

    'getPerfInitData': getPerfInitData,
    'setPerfInitData': setPerfInitData,
    'getTransitionAlt': getTransitionAlt,

    'setDeparture': setDeparture,
    'getDeparture': getDeparture,

    'setDepartureRunway': setDepartureRunway,
    'listDepartureRunways': listDepartureRunways,
    'getDepartureRunway': getDepartureRunway,

    'setSID': setSID,
    'listSIDs': listSIDs,
    'getSID': getSID,

    'isValidSID': isValidSID,
    'isValidSTAR': isValidSTAR,

    'setSidTransition': setSidTransition,
    'listSidTransitions': listSidTransitions,
    'getSidTransition': getSidTransition,

    'setDestination': setDestination,
    'getDestination': getDestination,

    'setDestinationRunway': setDestinationRunway,
    'listDestinationRunways': listDestinationRunways,
    'getDestinationRunway': getDestinationRunway,

    'setSTAR': setSTAR,
    'listSTARs': listSTARs,
    'getSTAR': getSTAR,

    'setStarTransition': setStarTransition,
    'listStarTransitions': listStarTransitions,
    'getStarTransition': getStarTransition,

    'setApproach': setApproach,
    'listApproaches': listApproaches,
    'getApproach': getApproach,

    'setApproachTransition': setApproachTransition,
    'listApproachTransitions': listApproachTransitions,
    'getApproachTransition': getApproachTransition,
};

return fms;
