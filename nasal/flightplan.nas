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

var fuelSamplerVars = {
    deltat: 10,
    lastKG: nil,
    currentKG: nil,
    ffKG: nil,
    propKG: props.globals.getNode('/consumables/fuel/total-fuel-kg'),
};

mcdu.fuelSampler.restart(fuelSamplerVars.deltat);

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


var getFuelFlow = func {
    return fuelSamplerVars.ffKG;
};

var getFuelOnBoard = func {
    return fuelSamplerVars.currentKG;
};

var hasFlightplanModifications = func {
    return (mcdu.modifiedFlightplan != nil);
};

var getModifyableFlightplan = func {
    if (mcdu.modifiedFlightplan == nil) {
        mcdu.modifiedFlightplan = flightplan().clone();
        if (flightplan().current >= mcdu.modifiedFlightplan.getPlanSize())
            mcdu.modifiedFlightplan.current = -1;
        else
            mcdu.modifiedFlightplan.current = flightplan().current;
    }
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
    var current = math.max(1, mcdu.modifiedFlightplan.current);
    # TODO: if modifiedFlightplan.current < 0, then that means we have
    # deleted the previously-current waypoint from the flightplan.
    # Right now, the "solution" we pick is to just rewind the flightplan to the
    # start of the route, but of course that is not the correct way.
    mcdu.modifiedFlightplan.activate();
    fgcommand("activate-flightplan", props.Node.new({"activate": 1}));
    if (current < flightplan().getPlanSize())
        flightplan().current = current;
    else
        flightplan().current = -1;
    mcdu.modifiedFlightplan = nil;
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

var getFlightplanSize = func {
    var fp = fms.getVisibleFlightplan();
    return fp.getPlanSize();
};

var makeLegInfo = func (wp, totalDistance, totalDistanceRemaining, fuelOnBoard, groundspeed, fuelFlow) {
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
        });
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
    var end = math.min(fp.getPlanSize(), first + length);

    for (var i = 0; i < end; i += 1) {
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
        if (i >= first) {
            append(result,
                    removeNilFields(
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
                        }));
        }
    }
    return result;
};

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
    var wpDest = nil;
    for (var i = fp.current; i < fp.getPlanSize(); i += 1) {
        var wp = fp.getWP(i);
        if (wp != nil) {
            # print(wp.wp_name);
            # print(wp.wp_type);
            # print(wp.wp_role);
        
            if (wp.wp_type == 'runway') {
                # This happens when the destination exists and has a runway
                # selected.
                wpDest = wp;
                break;
            }
            if (wp.wp_role == 'approach' and fp.destination != nil and
                # This is our fallback when no destination runway has been
                # selected; it will (hopefully) match the destination airport,
                # if any.
                wp.wp_name == fp.destination.id) {
                wpDest = wp;
            }
        }
    }

    var info = {};
    if (wpCurrent != nil)
        info.current = makeLegInfo(wpCurrent, totalDistance, totalDistanceRemaining, fuelOnBoard, fuelGS, fuelFlow);
    if (wpNext != nil)
        info.next = makeLegInfo(wpNext, totalDistance, totalDistanceRemaining, fuelOnBoard, fuelGS, fuelFlow);
    if (wpDest != nil)
        info.destination = makeLegInfo(wpDest, totalDistance, totalDistanceRemaining, fuelOnBoard, fuelGS, fuelFlow);
    info.fob = fuelOnBoard;
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

var findWaypoint = func (needle) {
    var fp = fms.getVisibleFlightplan();
    var acpos = geo.aircraft_position();
    var results = [];
    for (var i = math.max(0, fp.current or 0); i < fp.getPlanSize(); i += 1) {
        var wp = fp.getWP(i);
        if (wp.wp_name == needle)
            append(results,
                completeWaypoint({ "type": "leg" , "id": wp.id, "name": wp.wp_name, "wp": wp }, acpos));
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
        debug.dump(a.type, a.distance, b.type, b.distance);
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
    var fp = fms.getVisibleFlightplan();
    return fp.current;
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
    var fp = fms.getVisibleFlightplan();
    var curParent = 'DCT';
    var curWP = nil;
    var entries = [];
    var dist = nil;
    var destinationName = (fp.destination == nil) ? nil : fp.destination.id;

    var firstEnroute = nil;
    var firstArrival = nil;

    for (var i = 1; i < fp.getPlanSize(); i += 1) {
        var wp = fp.getWP(i);
        # printf("%s %s", wp.id, wp.wp_role);
        if (firstEnroute == nil and wp.wp_role == nil)
            firstEnroute = i;
        if (firstArrival == nil and firstEnroute != nil and (wp.wp_role != nil or wp.id == destinationName))
            firstArrival = i;
    }
    if (firstEnroute == nil)
        return [];
    if (firstArrival == nil)
        firstArrival = fp.getPlanSize() - 1;

    var lastIndex = firstEnroute;
    for (var i = firstEnroute; i < firstArrival; i += 1) {
        var wp = fp.getWP(i);

        if (dist == nil)
            dist = wp.distance_along_route;
        var parent = wp.wp_parent;
        var parentName = 'DCT';
        if (parent != nil)
            parentName = parent.id;
        # printf("%s %s", parentName, wp.wp_name);
        if (parentName != curParent or parentName == 'DCT') {
            if (curParent != nil and curWP != nil) {
                append(entries, { 'via': curParent, 'to': curWP.id, 'dist': dist, 'fromIndex': lastIndex, 'toIndex': i });
                dist = 0;
                lastIndex = i;
            }
        }
        curParent = parentName;
        curWP = wp;
        dist += wp.leg_distance;
    }
    if (curParent != nil and curWP != nil and dist > 0) {
        append(entries, { 'via': curParent, 'to': curWP.id, 'dist': dist, 'fromIndex': lastIndex, 'toIndex': firstArrival });
    }
    # printf("Plan size: %i", fp.getPlanSize());
    # foreach (var entry; entries) {
    #     printf("%s %s %i %i", entry.via, entry.to, entry.fromIndex, entry.toIndex);
    # }
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

    if (insertIndex != nil) {
        fp.insertWPAfter(leg, insertIndex - 1);
    }
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

    if (insertIndex != nil) {
        fp.insertWPAfter(leg, insertIndex - 1);
    }
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
        debug.dump(airport);
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
        var tas = iasToTAS(alt, perfEntry.spd);
        printf("%02i FL%03i IAS %3i TAS %3i %5.1fnmi", t, alt / 100, perfEntry.spd, tas, dist);
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

    calcTOD();

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
    debug.dump(mcdu.perfInitData);
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
calcTOD();

var fms = {
    '_updateFuelSampler': updateFuelSampler,

    'hasFlightplanModifications': hasFlightplanModifications,
    'getModifyableFlightplan': getModifyableFlightplan,
    'getVisibleFlightplan': getVisibleFlightplan,
    'cancelFlightplanEdits': cancelFlightplanEdits,
    'commitFlightplanEdits': commitFlightplanEdits,

    'getFGCallsign': getFGCallsign,
    'setFGCallsign': setFGCallsign,

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
    'getUTCMinutes': getUTCMinutes,
    'getFlightplanLegs': getFlightplanLegs,
    'getFlightplanSize': getFlightplanSize,
    'getCurrentLeg': getCurrentLeg,
    'getProgressInfo': getProgressInfo,

    'getPerfInitData': getPerfInitData,
    'setPerfInitData': setPerfInitData,

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
