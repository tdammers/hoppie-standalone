(func {
    var registerRef = func (val) {
        if (contains(globals.externalMCDU.refTable, id(val))) {
            globals.externalMCDU.refTable[id(val)].refcount += 1;
        }
        else {
            globals.externalMCDU.refTable[id(val)] =
                { value: val
                , refcount: 1
                }
        }
    };

    var releaseRef = func (ident) {
        if (contains(globals.externalMCDU.refTable, ident)) {
            globals.externalMCDU.refTable[ident].refcount -= 1;
            if (globals.externalMCDU.refTable[ident].refcount <= 0) {
                delete(globals.externalMCDU.refTable, ident);
            }
        }
    };

    var acquireRef = func (ident) {
        if (contains(globals.externalMCDU.refTable, ident)) {
            globals.externalMCDU.refTable[ident].refcount += 1;
            return globals.externalMCDU.refTable[ident].value;
        }
        else
            return nil;
    };


    var deref = func (ref) {
        if (!contains(ref, '__reftype__'))
            return nil;
        if (!contains(ref, '__refid__'))
            return nil;
        var refid = ref['__refid__'];
        if (!contains(globals.externalMCDU.refTable, refid))
            return nil;
        return globals.externalMCDU.refTable[refid].value;
    };

    var printReftable = func {
        var refTable = globals.externalMCDU.refTable;
        foreach (var k; keys(refTable)) {
            printf("%s %i", k, refTable[k].refcount);
        }
        return nil;
    };

    var release = func (ref) {
        print("release(" ~ debug.string(ref) ~ ")");
        if (typeof(ref) != 'ghost')
            return nil;
        var refid = id(ref);
        releaseRef(refid);
        printReftable();
    }

    var acquire = func (ref) {
        print("acquire(" ~ debug.string(ref) ~ ")");
        if (typeof(ref) != 'ghost')
            return nil;
        var refid = id(ref);
        acquireRef(refid);
        printReftable();
    }


    var jsonEncode = func (val, register=1) {
        var ty = typeof(val);
        var encoded = 'null';
        if (ty == 'ghost') {
            if (register) registerRef(val);
            encoded = jsonEncode({
                            '__reftype__': 'ghost',
                            '__refid__': id(val),
                            '__ghosttype__': ghosttype(val)
                        });
        }
        elsif (ty == 'func') {
            if (register) registerRef(val);
            encoded = jsonEncode({
                            '__reftype__': 'func',
                            '__refid__': id(val),
                        });
        }
        elsif (ty == 'scalar') {
            if (isint(val)) {
                if (sprintf(':%i', val) == ':' ~ val) {
                    ty = 'int';
                    encoded = sprintf('%i', val);
                }
                else {
                    ty = 'string';
                    encoded = jsonEncodeString(val ~ '');
                }
            }
            elsif (isnum(val)) {
                ty = 'float';
                encoded = sprintf('%1.12f', val);
            }
            else {
                ty = 'string';
                encoded = jsonEncodeString(val ~ '');
            }
        }
        elsif (ty == 'vector') {
            ty = 'array';
            encodedElems = [];
            foreach (var elem; val) {
                append(encodedElems, jsonEncode(elem));
            }
            encoded = '[' ~ string.join(',', encodedElems) ~ ']';
        }
        elsif (ty == 'hash') {
            ty = 'object';
            encodedPairs = [];
            foreach (var k; keys(val)) {
                append(
                    encodedPairs,
                    jsonEncodeString(k) ~ ": " ~ jsonEncode(val[k]));
            }
            encoded = '{' ~ string.join(',', encodedPairs) ~ '}';
        }
        else {
            ty = 'null';
        }
        return encoded;
    };

    var jsonEncodeString = func (str) {
        var encoded = '';
        var len = utf8.size(str);
        for (var i = 0; i < len; i += 1) {
            var c = utf8.strc(str, i);
            if (c == 92)
                encoded ~= "\\\\";
            elsif (c == 34)
                encoded ~= "\\\"";
            elsif (c >= 32 and c <= 126)
                encoded ~= utf8.chstr(c);
            else
                encoded ~= sprintf("\\u%04x", c);
        }
        return '"' ~ encoded ~ '"';
    };

    var callCounter = 0;

    var callFunction = func (fn, args) {
        var err = [];
        var result = nil;
        callCounter += 1;

        var f = call(func {
            var nameParts = split('.', fn);
            var resolved = globals.externalMCDU;
            foreach (var part; nameParts) {
                resolved = resolved[part];
                if (resolved == nil)
                    die("Not found: " ~ part);
            }
            return resolved;
        }, [], nil, {}, err);
        if (size(err) > 0) {
            result = { "error": err, "num": callCounter, "caller": caller() };
        }
        else {
            var value = call(f, args, nil, {}, err);
            if (size(err) > 0) {
                result = { "error": err, "num": callCounter, "caller": caller() };
            }
            else {
                result = { "value": value, "num": callCounter };
            }
        }
        return jsonEncode(result) ~ "\r\n";
    };

    var scripts = {};

    var beginScript = func (h) {
        scripts[h] = "";
        return "\n";
    };

    var pushScriptCode = func (h, src) {
        scripts[h] ~= src;
        return "\n";
    };

    var finishScript = func (h) {
        var result = runScript(scripts[h]);
        delete(scripts, h);
        return result;
    };

    var runScript = func (script) {
        var err = [];
        callCounter += 1;

        var f = call(func { compile(script, 'websocket'); }, nil, nil, err);
        if (size(err) > 0) {
            result = { "error": err, "num": callCounter, "caller": caller() };
        }
        else {
            var value = call(f, [], nil, {}, err);
            if (size(err) > 0) {
                result = { "error": err, "num": callCounter, "caller": caller() };
            }
            else {
                result = { "value": value, "num": callCounter };
            }
        }
        return jsonEncode(result) ~ "\r\n";
    };

    var loadModule = func (moduleHash, moduleName, moduleLoader, force=0) {
        if (contains(globals.externalMCDU.libs, moduleName) and
                !force and
                globals.externalMCDU.libs[moduleName] == moduleHash) {
            return nil;
        }
        printf("Loading module %s (%s)", moduleName, moduleHash);
        var module = moduleLoader(globals.externalMCDU);
        globals.externalMCDU.libs[moduleName] = moduleHash;
        globals.externalMCDU[moduleName] = module;
        return nil;
    };

    if (!contains(globals, 'externalMCDU'))
        globals.externalMCDU = {};

    delete(globals.externalMCDU, 'flightplan');
    globals.externalMCDU.jsonEncode = jsonEncode;
    globals.externalMCDU.runScript = runScript;
    globals.externalMCDU.beginScript = beginScript;
    globals.externalMCDU.pushScriptCode = pushScriptCode;
    globals.externalMCDU.finishScript = finishScript;
    globals.externalMCDU.callFunction = callFunction;
    globals.externalMCDU.deref = deref;
    globals.externalMCDU.acquire = acquire;
    globals.externalMCDU.release = release;
    if (!contains(globals.externalMCDU, 'refTable')) {
        globals.externalMCDU.refTable = {};
    }
    if (!contains(globals.externalMCDU, 'libs')) {
        globals.externalMCDU.libs = {};
    }
    globals.externalMCDU.loadModule = loadModule;
    globals.externalMCDU.refTable = {};
    printReftable();
})();
"\n";
