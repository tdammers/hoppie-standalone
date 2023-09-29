if (!contains(mcdu, 'modifiedFlightplan'))
    mcdu.modifiedFlightplan = nil;

var hasFlightplanModifications = func {
    return (mcdu.modifiedFlightplan != nil);
};

var getModifyableFlightplan = func {
    if (mcdu.modifiedFlightplan == nil) {
        mcdu.modifiedFlightplan = flightplan().clone();
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
    var current = mcdu.modifiedFlightplan.current;
    mcdu.modifiedFlightplan.activate();
    fgcommand("activate-flightplan", props.Node.new({"activate": 1}));
    flightplan().current = current;
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

var getFlightplanLegs = func {
    var acpos = geo.aircraft_position();
    var fp = fms.getVisibleFlightplan();
    var result = [];
    var distanceToCurrent = getprop('/autopilot/route-manager/wp/dist');
    var totalDistanceRemaining = getprop('/autopilot/route-manager/distance-remaining-nm');
    var distanceRemaining = 0;
    for (var i = 0; i < fp.getPlanSize(); i += 1) {
        var wp = fp.getWP(i);
        var parent_id = nil;
        if (wp.wp_parent != nil)
            parent_id = wp.wp_parent.id;
        if (i == fp.current) {
            distanceRemaining = distanceToCurrent;
        }
        elsif (i > fp.current) {
            distanceRemaining += wp.leg_distance;
        }
        append(result,
                { "name": wp.wp_name
                , "heading": wp.leg_bearing
                , "leg_dist": wp.leg_distance
                , "route_dist": wp.distance_along_route
                , "remaining_dist": distanceRemaining
                , "speed": wp.speed_cstr
                , "speed_type": wp.speed_cstr_type
                , "alt": wp.alt_cstr
                , "alt_type": wp.alt_cstr_type
                , "parent": parent_id
                , "role": wp.wp_role
                , "discontinuity": (wp.wp_type == "discontinuity" or wp.wp_type == "vectors")
                });
    }
    return result;
};

var findWaypoint = func (needle) {
    var fp = fms.getVisibleFlightplan();
    var acpos = geo.aircraft_position();
    var results = [];
    for (var i = fp.current or 0; i < fp.getPlanSize(); i += 1) {
        var wp = fp.getWP(i);
        if (wp.wp_name == needle)
            append(results, { "type": "leg" , "name": wp.wp_name, "wp": wp });
    }
    if (size(needle) == 4) {
        var airports = findAirportsByICAO(needle);
        foreach (var airport; airports) {
            append(results, { "type": "airport", "name": airport.name, "wp": airport });
        }
    }
    else {
        var fixes = findFixesByID(acpos, needle);
        foreach (var fix; fixes) {
            if (fix.id == needle)
                append(results, { "type": "fix", "name": fix.id, "wp": fix });
        }
        var vors = findNavaidsByID(acpos, needle, 'vor');
        foreach (var vor; vors) {
            if (vor.id == needle)
                append(results, { "type": vor.type, "name": vor.name, "wp": vor });
        }
        var ndbs = findNavaidsByID(acpos, needle, 'ndb');
        foreach (var ndb; ndbs) {
            if (ndb.id == needle)
                append(results, { "type": ndb.type, "name": ndb.name, "wp": ndb });
        }
        var dmes = findNavaidsByID(acpos, needle, 'dme');
        foreach (var dme; dmes) {
            if (dme.id == needle)
                append(results, { "type": dme.type, "name": dme.name, "wp": dme });
        }
    }
    var acpos = geo.aircraft_position();
    foreach (var result; results) {
        var wpPos = geo.Coord.new();
        wpPos.set_latlon(result.wp.lat, result.wp.lon);
        result.distance = acpos.distance_to(wpPos) * M2NM;
        result.bearing = acpos.course_to(wpPos);
    }
    var sortfunc = func (a, b) {
        if (a.type == "leg" and b.type != "leg")
            return -1;
        elsif (b.type == "leg" and a.type != "leg")
            return 1;
        elsif (a.distance < b.distance - 0.1)
            return -1;
        elsif (a.distance > b.distance - 0.1)
            return 1;
        else
            return 0;
    };
    return sort(results, sortfunc);
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

var insertDirect = func (wp) {
    var fp = fms.getModifyableFlightplan();
    var acpos = geo.aircraft_position();
    var direct = createWP(acpos.lat(), acpos.lon(), "DIRECT");
    fp.insertWP(direct, fp.current);
    var newWP = createWP(wp.lat, wp.lon, wp.id);
    fp.insertWP(newWP, fp.current);
    fp.insertWP(createDiscontinuity(), fp.current);
    fp.current -= 2;
    return nil;
};

var insertDirectFP = func (wp) {
    var fp = fms.getModifyableFlightplan();
    var acpos = geo.aircraft_position();
    var fpWP = nil;
    var fpIndex = nil;
    for (var i = fp.current; i < fp.getPlanSize(); i += 1) {
        fpWP = fp.getWP(i);
        if (fpWP.lat == wp.lat and fpWP.lon == wp.lon and fpWP.id == wp.id) {
            fpIndex = i;
            break;
        }
    }
    if (fpIndex == nil) {
        insertDirect(wp);
    }
    else {
        var direct = createWP(acpos.lat(), acpos.lon(), "DIRECT");
        fp.insertWP(direct, fpIndex);
        fp.current = fpIndex + 1;
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
        debug.dump(transitions);
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
    print('setDeparture(' ~ icao ~ ')');
    var fp = getModifyableFlightplan();
    if (icao == nil) {
        fp.departure = nil;
        return 1;
    }
    else {
        var airports = findAirportsByICAO(icao);
        if (size(airports) == 0) return 0;
        fp.departure = airports[0];
        return 1;
    }
}

var getDeparture = func {
    var fp = getVisibleFlightplan();
    return ((fp.departure == nil) ? nil : fp.departure.id);
}

var setDestination = func (icao) {
    print(icao);
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
        debug.dump(transitions);
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
        debug.dump(transitions);
        if (!contains(transitions, transitionID)) return "INVALID";
        fp.approach_trans = approach.transition(transitionID);
        return nil;
    }
}


var listApproachTransitions = func {
    print("listApproachTransitions");
    var fp = getVisibleFlightplan();
    var destination = fp.destination;
    debug.dump(destination);
    if (destination == nil) return [];
    var approach = fp.approach;
    debug.dump(approach);
    if (approach == nil)
        return [];
    else
        return approach.transitions;
}

var getApproachTransition = func {
    var fp = getVisibleFlightplan();
    return ((fp.approach_trans == nil) ? nil : fp.approach_trans.id);
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
    'insertDirect': insertDirect,
    'insertDirectFP': insertDirectFP,

    'getGroundspeed': getGroundspeed,
    'getUTCMinutes': getUTCMinutes,
    'getFlightplanLegs': getFlightplanLegs,
    'getCurrentLeg': getCurrentLeg,

    'setDeparture': setDeparture,
    'getDeparture': getDeparture,

    'setDepartureRunway': setDepartureRunway,
    'listDepartureRunways': listDepartureRunways,
    'getDepartureRunway': getDepartureRunway,

    'setSID': setSID,
    'listSIDs': listSIDs,
    'getSID': getSID,

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
