print("Loading flightplan.nas");

if (!contains(mcdu, 'modifiedFlightplan'))
    mcdu.modifiedFlightplan = nil;

return {
    'hasFlightplanModifications': func {
        return (mcdu.modifiedFlightplan != nil);
    },

    'getModifyableFlightplan': func {
        if (mcdu.modifiedFlightplan == nil) {
            mcdu.modifiedFlightplan = flightplan().clone();
        }
    },

    'getVisibleFlightplan': func {
        if (mcdu.modifiedFlightplan != nil)
            return mcdu.modifiedFlightplan;
        else
            return flightplan();
    },

    'cancelFlightplanEdits': func {
        mcdu.modifiedFlightplan = nil;
    },

    'commitFlightplanEdits': func {
        mcdu.modifiedFlightplan.activate();
        mcdu.modifiedFlightplan = nil;
    },

    'getGroundspeed': func {
        return getprop('/velocities/groundspeed-kt');
    },

    'getUTCMinutes': func {
        var hour = getprop('/sim/time/utc/hour');
        var minute = getprop('/sim/time/utc/minute');
        var second = getprop('/sim/time/utc/second');
        return (hour * 60 + minute + second / 60);
    },

    'getFlightplanLegs': func {
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
    },

    'getCurrentLeg': func {
        var fp = fms.getVisibleFlightplan();
        return fp.current;
    },


};
