if (!contains(mcdu, 'vnav')) {
    mcdu.vnav = {
        'todDist': nil,
    };
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
                        ? mcdu.fms.machToTAS(alt, substr(perfEntry.spd ~ '', 1) / 100)
                        : mcdu.fms.iasToTAS(alt, perfEntry.spd);
        if (perfEntry.roc < cruiseAlt - alt) {
            alt += perfEntry.roc;
            distRemaining += tas / 60;
            t += 1;
        }
        else {
            var dt = (cruiseAlt - alt) / perfEntry.roc;
            distRemaining += tas * dt / 60;
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

return {
    'calcTOC': calcTOC,
    'calcTOD': calcTOD,
};
