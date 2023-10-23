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

if (!contains(mcdu, 'currentFlightPhase')) {
    mcdu.currentFlightPhase = ON_STAND;
}

if (!contains(mcdu, 'flightPhaseChecker')) {
    mcdu.flightPhaseChecker = maketimer(1, func { mcdu.fms._checkFlightPhase(); });
    mcdu.flightPhaseChecker.simulatedTime = 1;
}

if (!contains(mcdu, 'takeoffMetrics')) {
    mcdu.takeoffMetrics = {
        'startPos': nil,
        'endPos': nil,
    };
}

var checkFlightPhase = func {
    var groundspeed = getprop('/velocities/groundspeed-kt');
    var altitude = getprop('/instrumentation/altimeter/indicated-altitude-ft');
    var agl = getprop('/position/altitude-agl-ft');
    var vspeed = getprop('/instrumentation/vertical-speed-indicator/indicated-speed-fpm');
    var wow = getprop('/gear/gear/wow');
    var phasePrev = mcdu.currentFlightPhase;
    if (mcdu.currentFlightPhase == ON_STAND) {
        if (groundspeed > 5)
            mcdu.takeoffMetrics.startPos = nil;
            mcdu.takeoffMetrics.endPos = nil;
            mcdu.currentFlightPhase = TAXI_OUT;
    }
    if (mcdu.currentFlightPhase == TAXI_OUT) {
        if (groundspeed > 40)
            mcdu.currentFlightPhase = TAKEOFF;
        else {
            foreach (var engine; props.globals.getNode('controls/engines').getChildren('engine')) {
                var throttle = engine.getValue('throttle');
                if (throttle != nil and throttle > 0.7) {
                    mcdu.currentFlightPhase = TAKEOFF;
                    break;
                }
            }
        }
        if (mcdu.currentFlightPhase == TAKEOFF) {
            mcdu.takeoffMetrics.startPos = geo.aircraft_position();
        }
    }
    if (mcdu.currentFlightPhase == TAKEOFF or mcdu.currentFlightPhase == GO_AROUND) {
        if (mcdu.takeoffMetrics.endPos == nil and agl >= 50) {
            mcdu.takeoffMetrics.endPos = geo.aircraft_position();
            var takeoffDistance = mcdu.takeoffMetrics.startPos.distance_to(mcdu.takeoffMetrics.endPos);
            printf("Takeoff distance: %i m / %i ft",
                takeoffDistance,
                takeoffDistance * M2FT);
        }
        if (agl > 800)
            mcdu.currentFlightPhase = CLIMB;
    }
    if (mcdu.currentFlightPhase == CLIMB) {
        var cruiseAlt = getprop('/autopilot/route-manager/cruise/altitude-ft');
        if (cruiseAlt == nil or cruiseAlt == 0) {
            cruiseAlt = getprop('/autopilot/route-manager/cruise/flight-level');
            if (cruiseAlt != nil)
                cruiseAlt *= 100;
        }
        if (cruiseAlt > 0) {
            if (altitude >= cruiseAlt - 100) {
                mcdu.currentFlightPhase = CRUISE;
            }
        }
        else {
            if (altitude >= 4000 and vspeed < 100) {
                mcdu.currentFlightPhase = CRUISE;
            }
        }
    }
    if (mcdu.currentFlightPhase == CRUISE) {
        var cruiseAlt = getprop('/autopilot/route-manager/cruise/altitude-ft');
        if (cruiseAlt == nil or cruiseAlt == 0) {
            cruiseAlt = getprop('/autopilot/route-manager/cruise/flight-level');
            if (cruiseAlt != nil)
                cruiseAlt *= 100;
        }
        if (cruiseAlt > 0) {
            if (altitude < cruiseAlt - 500) {
                mcdu.currentFlightPhase = DESCENT;
            }
        }
        else {
            if (vspeed < -400) {
                mcdu.currentFlightPhase = DESCENT;
            }
        }
    }
    if (mcdu.currentFlightPhase == DESCENT) {
        if (agl < 3000) {
            mcdu.currentFlightPhase = APPROACH;
        }
    }
    if (mcdu.currentFlightPhase == APPROACH) {
        if (wow and groundspeed < 40) {
            mcdu.currentFlightPhase = TAXI_IN;
        }
        elsif (vspeed > 300) {
            mcdu.currentFlightPhase = GO_AROUND;
        }
    }
    if (mcdu.currentFlightPhase == TAXI_IN) {
        var enginesRunning = 0;
        foreach (var e; props.globals.getNode('engines').getChildren('engine')) {
            isRunning = e.getBoolValue('running');
            if (isRunning)
                enginesRunning += 1;
        }
        if (enginesRunning == 0 and groundspeed < 5) {
            mcdu.currentFlightPhase = ON_STAND;
        }
    }
    if (mcdu.currentFlightPhase != phasePrev) {
        printf("%s -> %s", flightPhaseName[phasePrev], flightPhaseName[mcdu.currentFlightPhase]);
    }
    mcdu.fms.calcTOC();
    mcdu.fms.calcTOD();
    checkFlightPhaseAlerts(mcdu.currentFlightPhase);
};

var checkMinimumTakeoffFuel = func {
    if (mcdu.currentFlightPhase == TAXI_OUT) {
        var fuelOnBoard = mcdu.fuelSamplerVars.currentKG;
        if (!contains(mcdu, 'perfInitData') or
            !contains(mcdu.perfInitData, 'toFuel') or
            mcdu.perfInitData.toFuel == nil or
            fuelOnBoard == nil) {
            mcdu.fms.clearAlert('LOW TAKEOFF FUEL');
        }
        elsif (fuelOnBoard < mcdu.perfInitData.toFuel) {
            mcdu.fms.setAlert('LOW TAKEOFF FUEL');
        }
        else {
            mcdu.fms.clearAlert('LOW TAKEOFF FUEL');
        }
    }
    else {
        mcdu.fms.clearAlert('LOW TAKEOFF FUEL');
    }
};

var checkCurrentFuel = func {
    if (mcdu.currentFlightPhase > TAKEOFF) {
        var fuelOnBoard = mcdu.fuelSamplerVars.currentKG;
        if (!contains(mcdu, 'perfInitData') or
            mcdu.perfInitData['contFuel'] == nil or
            mcdu.perfInitData['reserveFuel'] == nil or
            fuelOnBoard == nil) {
            mcdu.fms.clearAlert('FUEL WARN');
            mcdu.fms.clearAlert('FUEL EMERGENCY');
        }
        elsif (fuelOnBoard < mcdu.perfInitData.reserveFuel) {
            mcdu.fms.clearAlert('FUEL WARN');
            mcdu.fms.setAlert('FUEL EMERGENCY');
        }
        elsif (fuelOnBoard < mcdu.perfInitData.reserveFuel + mcdu.perfInitData.contFuel) {
            mcdu.fms.setAlert('FUEL WARN');
            mcdu.fms.clearAlert('FUEL EMERGENCY');
        }
        else {
            mcdu.fms.clearAlert('FUEL WARN');
            mcdu.fms.clearAlert('FUEL EMERGENCY');
        }
    }
    else {
            mcdu.fms.clearAlert('FUEL WARN');
            mcdu.fms.clearAlert('FUEL EMERGENCY');
    }
};

var checkFlightPhaseAlerts = func {
    checkMinimumTakeoffFuel();
    checkCurrentFuel();
};

var getFlightPhase = func {
    return mcdu.currentFlightPhase;
};

var setFlightPhase = func (p) {
    mcdu.currentFlightPhase = p;
    return nil;
};

mcdu.flightPhaseChecker.restart(1);

return {
    '_checkFlightPhase': checkFlightPhase,
    'getFlightPhase': getFlightPhase,
    'setFlightPhase': setFlightPhase,
};
