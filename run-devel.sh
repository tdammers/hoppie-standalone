#!/bin/sh
INNER='ghcid --test runColoredTests -c cabal repl exe:hoppie-mcdu'

while (true); do
    ghcid \
        --restart hoppie-standalone.cabal \
        --run=":! $INNER" -c cabal repl hoppie-standalone
    inotifywait -e modify \
        hoppie-standalone.cabal
    sleep 1
done
