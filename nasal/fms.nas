var libs = [
    "common",
    "alerts",
    "vnav",
    "fuelSampler",
    "flightPhase",
    "perf",
    "flightplan",
];

var module = {};
foreach (var lib; libs) {
    printf("%s", lib);
    foreach (var k; keys(mcdu[lib])) {
        module[k] = mcdu[lib][k];
    }
}
return module;
