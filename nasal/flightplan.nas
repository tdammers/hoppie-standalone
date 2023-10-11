if (!contains(mcdu, 'modifiedFlightplan'))
    mcdu.modifiedFlightplan = nil;

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
    # reportFlightplanCurrent('commitFlightplanEdits[post]');
    return nil;
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
    return mcdu.fms.removeNilFields(
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
    return mcdu.fms.removeNilFields(
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
    var fp = mcdu.fms.getVisibleFlightplan();

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
    var fp = mcdu.fms.getVisibleFlightplan();
    var result = [];
    var groundspeed = getprop('/velocities/groundspeed-kt');
    var fuelGS = math.max(40, groundspeed);
    var distanceToCurrent = getprop('/autopilot/route-manager/wp/dist');
    var totalDistanceRemaining = getprop('/autopilot/route-manager/distance-remaining-nm');
    var distanceRemaining = 0;
    var fuelRemaining = mcdu.fuelSamplerVars.currentKG;
    var fuelFlow = mcdu.fuelSamplerVars.ffKG;
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
                append(result, mcdu.fms.removeNilFields(current));
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
        append(result, mcdu.fms.removeNilFields(current));
    return result;
};

var getDestinationIndex = func (fp = nil) {
    if (fp == nil)
        fp = mcdu.fms.getVisibleFlightplan();
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
        return mcdu.fms.removeNilFields({
            "rnp": rnp,
            "anp": anp,
            "sensor": sensorName
        });
}

var grossWeightProp = nil;
var fdm = getprop('/sim/flight-model');
if (fdm == 'yasim')
    grossWeightProp = props.globals.getNode('/yasim/gross-weight-lbs');
elsif (fdm == 'jsb')
    grossWeightProp = props.globals.getNode('/fdm/jsbsim/inertia/weight-lbs');

var getProgressInfo = func () {
    var fp = mcdu.fms.getVisibleFlightplan();
    var groundspeed = getprop('/velocities/groundspeed-kt');
    var fuelGS = math.max(40, groundspeed);
    var totalDistanceRemaining = getprop('/autopilot/route-manager/distance-remaining-nm');
    var totalDistance = getprop('/autopilot/route-manager/total-distance');
    var fuelOnBoard = mcdu.fuelSamplerVars.currentKG;
    var fuelFlow = mcdu.fuelSamplerVars.ffKG;
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
    info.phase = mcdu.currentFlightPhase;
    if (contains(mcdu, 'vnav')) {
        if (mcdu.vnav['tocDist'] != nil)
            info.toc = makeVirtualLegInfo('TOC', mcdu.vnav.tocDist, totalDistance, totalDistanceRemaining, fuelOnBoard, fuelGS, fuelFlow);
        if (mcdu.vnav['todDist'] != nil)
            info.tod = makeVirtualLegInfo('TOD', totalDistance - mcdu.vnav.todDist, totalDistance, totalDistanceRemaining, fuelOnBoard, fuelGS, fuelFlow);
    }
    info.rnp = getRnpInfo();
    
    var alerts = mcdu.fms.getActiveAlerts();
    if (size(alerts) > 0) {
        info.alerts = alerts;
    }
    return info;
};


var setLegAltitude = func (n, alt, altType) {
    var wp = mcdu.fms.getVisibleFlightplan().getWP(n);
    if (wp == nil)
        return "NO WPT";
    wp = mcdu.fms.getModifyableFlightplan().getWP(n);
    wp.setAltitude(alt, altType);
    return nil;
};

var setLegSpeed = func (n, speed, speedType) {
    var wp = mcdu.fms.getVisibleFlightplan().getWP(n);
    if (wp == nil)
        return "NO WPT";
    wp = mcdu.fms.getModifyableFlightplan().getWP(n);
    wp.setSpeed(speed, speedType);
    return nil;
};

var getWaypoint = func (index) {
    var fp = mcdu.fms.getVisibleFlightplan();
    var wp = fp.getWP(index);
    if (wp == nil)
        return nil;
    else
        return completeWaypoint({ "type": "leg" , "id": wp.id, "name": wp.wp_name, "wp": wp });
};

var getFPLegIndex = func (needle) {
    var fp = mcdu.fms.getVisibleFlightplan();
    for (var i = fp.current or 0; i < fp.getPlanSize(); i += 1) {
        var wp = fp.getWP(i);
        if (wp.wp_name == needle)
            return completeWaypoint({ "type": "leg" , "id": wp.id, "name": wp.wp_name, "wp": wp });
    }
    return nil;
};

var findWaypoint = func (includeLegs, needle) {
    var fp = mcdu.fms.getVisibleFlightplan();
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
    var fp = mcdu.fms.getVisibleFlightplan();

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
    var fp = mcdu.fms.getVisibleFlightplan();
    var wp = fp.getWP(i);
    if (wp == nil) return nil;
    return wp.wp_name;
};

var deleteWaypoint = func (i) {
    var fp = mcdu.fms.getModifyableFlightplan();
    if (i >= fp.getPlanSize() - 1)
        return 0;
    if (i <= 0)
        return 0;
    fp.deleteWP(i);
    return 1;
};

var findFPWaypoint = func (fp, wp) {
    if (fp == nil)
        fp = mcdu.fms.getVisibleFlightplan();
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
    var fp = mcdu.fms.getModifyableFlightplan();
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
    var fp = mcdu.fms.getVisibleFlightplan();
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
    var fp = mcdu.fms.getVisibleFlightplan();
    var destinationName = (fp.destination == nil) ? nil : fp.destination.id;

    var count = toIndex - fromIndex;
    var fp = mcdu.fms.getModifyableFlightplan();

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
    var fp = mcdu.fms.getVisibleFlightplan();
    var acpos = geo.aircraft_position();
    toIndex = findFPWaypoint(fp, toWP);
    fromIndex = findFPWaypoint(fp, fromWP);
    if (toIndex == nil) {
        insertDirect(toWP, fromIndex);
    }
    elsif (fromIndex == nil) {
        var fp = mcdu.fms.getModifyableFlightplan();
        var direct = createWP(acpos.lat(), acpos.lon(), "DIRECT");
        fp.insertWP(direct, toIndex);
        fp.current = toIndex + 1;
    }
    else {
        if (toIndex <= fromIndex)
            return "INVALID SEQ";
        var fp = mcdu.fms.getModifyableFlightplan();
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
    var fp = mcdu.fms.getVisibleFlightplan();
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

    fp = mcdu.fms.getModifyableFlightplan();

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
    var fp = mcdu.fms.getVisibleFlightplan();
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

    fp = mcdu.fms.getModifyableFlightplan();

    var current = fp.current;
    if (insertIndex != nil) {
        fp.insertWPAfter(leg, insertIndex - 1);
    }
    fp.current = current;
    return nil;
};

var clearFlightplan = func {
    var fp = mcdu.fms.getModifyableFlightplan();
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

return {
    'hasFlightplanModifications': hasFlightplanModifications,
    'getModifyableFlightplan': getModifyableFlightplan,
    'getVisibleFlightplan': getVisibleFlightplan,
    'cancelFlightplanEdits': cancelFlightplanEdits,
    'commitFlightplanEdits': commitFlightplanEdits,

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

    'getFlightplanLegs': getFlightplanLegs,
    'getFlightplanSize': getFlightplanSize,
    'getCurrentLeg': getCurrentLeg,
    'getProgressInfo': getProgressInfo,

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
