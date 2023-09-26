print("Loading flightplan.nas");

if (!contains(mcdu, 'modifiedFlightplan'))
    mcdu.modifiedFlightplan = nil;

var hasFlightplanModifications = func {
    return (mcdu.modifiedFlightplan != nil);
};

var getModifyableFlightplan = func {
    return flightplan();
    if (mcdu.modifiedFlightplan == nil) {
        mcdu.modifiedFlightplan = flightplan().clone();
    }
    return mcdu.modifiedFlightplan;
};

var getVisibleFlightplan = func {
    return flightplan();
    if (mcdu.modifiedFlightplan != nil)
        return mcdu.modifiedFlightplan;
    else
        return flightplan();
};

var cancelFlightplanEdits = func {
    mcdu.modifiedFlightplan = nil;
};

var commitFlightplanEdits = func {
    mcdu.modifiedFlightplan.activate();
    mcdu.modifiedFlightplan = nil;
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
    var fp = fms.getVisibleFlightplan();
    var result = [];
    for (var i = 0; i < fp.getPlanSize(); i += 1) {
      var wp = fp.getWP(i);
      var parent_id = nil;
      if (wp.wp_parent != nil)
        parent_id = wp.wp_parent.id;
      append(result,
        { "name": wp.wp_name
        , "heading": wp.leg_bearing
        , "leg_dist": wp.leg_distance
        , "route_dist": wp.distance_along_route
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

var getCurrentLeg = func {
    var fp = fms.getVisibleFlightplan();
    return fp.current;
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

return {
    'hasFlightplanModifications': hasFlightplanModifications,
    'getModifyableFlightplan': getModifyableFlightplan,
    'getVisibleFlightplan': getVisibleFlightplan,
    'cancelFlightplanEdits': cancelFlightplanEdits,
    'commitFlightplanEdits': commitFlightplanEdits,

    'getGroundspeed': getGroundspeed,
    'getUTCMinutes': getUTCMinutes,
    'getFlightplanLegs': getFlightplanLegs,
    'getCurrentLeg': getCurrentLeg,

    'setDepartureRunway': setDepartureRunway,
    'listDepartureRunways': listDepartureRunways,
    'getDepartureRunway': getDepartureRunway,

    'setSID': setSID,
    'listSIDs': listSIDs,
    'getSID': getSID,
};
