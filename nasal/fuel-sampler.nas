if (!contains(mcdu, 'fuelSamplerTimer')) {
    mcdu.fuelSamplerTimer = maketimer(10, func { mcdu.fms._updateFuelSampler(); });
    mcdu.fuelSamplerTimer.simulatedTime = 1;
}

if (!contains(mcdu, 'fuelSamplerVars')) {
    mcdu.fuelSamplerVars = {
        deltat: 10,
        lastKG: nil,
        currentKG: nil,
        ffKG: nil,
        propKG: props.globals.getNode('/consumables/fuel/total-fuel-kg'),
    };
}

mcdu.fuelSamplerVars.deltat = 10;

mcdu.fuelSamplerTimer.restart(mcdu.fuelSamplerVars.deltat);

var updateFuelSampler = func {
    mcdu.fuelSamplerVars.lastKG = mcdu.fuelSamplerVars.currentKG;
    mcdu.fuelSamplerVars.currentKG = mcdu.fuelSamplerVars.propKG.getValue();
    if (mcdu.fuelSamplerVars.lastKG != nil) {
        mcdu.fuelSamplerVars.ffKG =
            (mcdu.fuelSamplerVars.currentKG - mcdu.fuelSamplerVars.lastKG) /
            mcdu.fuelSamplerVars.deltat;
    }
};

# Update fuel sampler twice, so that the current fuel field is already filled
updateFuelSampler();
updateFuelSampler();

return {
    '_updateFuelSampler': updateFuelSampler,
};
