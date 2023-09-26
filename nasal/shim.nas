(func {
    var refTable = {};

    var registerRef = func (val) {
        if (contains(refTable, id(val))) {
            refTable[id(val)].refcount += 1;
        }
        else {
            refTable[id(val)] =
                { value: val
                , refcount: 1
                }
        }
    };

    var releaseRef = func (ident) {
        if (contains(refTable, ident)) {
            refTable[ident].refcount -= 1;
            if (refTable[ident].refcount <= 0) {
                delete(refTable, ident);
            }
        }
    };

    var deref = func (ref) {
        if (!contains(ref, '__reftype__'))
            return nil;
        if (!contains(ref, '__refid__'))
            return nil;
        var refid = ref['__refid__'];
        if (!contains(refTable, refid))
            return nil;
        return refTable[refid].value;
    };

    var grabRef = func (ident) {
        if (contains(refTable, ident)) {
            refTable[ident].refcount += 1;
            return refTable[ident].value;
        }
        else
            return nil;
    };

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
                ty = 'int';
                return sprintf('%i', val);
            }
            elsif (isnum(val)) {
                ty = 'float';
                return sprintf('%1.12f', val);
            }
            else {
                ty = 'string';
                return jsonEncodeString(val);
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

    # Uniquely identifies each individual call, so that the output property is
    # guaranteed to change when we write to it, even if the output value itself
    # is the same as the last one.
    var callCounter = 0;

    var runScript = func (outputPath, script) {
        var err = [];
        callCounter += 1;

        var f = call(func { compile(script, 'websocket'); }, nil, nil, err);
        if (size(err) > 0) {
            result = { "error": err, "num": callCounter };
        }
        else {
            var value = call(f, [], me, globals.hoppieStandaloneShim, err);
            if (size(err) > 0) {
                result = { "error": err, "num": callCounter };
            }
            else {
                result = { "value": value, "num": callCounter };
            }
        }
        setprop(outputPath, jsonEncode(result));

        # setprop(outputPath,
        #   script' = "setprop(\"" <> Text.pack (fgfsOutputPropertyPath conn) <> "\"," <>
        #         "(func { " <>
        #         "var err = []; "<>
        #         "var result = call(func {" <> script <> "}, [], null, {}, err);" <>
        #         "return result;" <>
        #         ")()" <>
        #         ");"
    };

    globals.hoppieStandaloneShim = {
        "jsonEncode": jsonEncode,
        "runScript": runScript,
        "deref": deref,
        "refTable": refTable,
    };
})();
