(func {
    var jsonEncode = func (val) {
        debug.dump("ENCODE:", val);
        var ty = typeof(val);
        var encoded = 'null';
        if (ty == 'scalar') {
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

    var runScript = func (outputPath, f) {
        var err = [];
        var value = call(f, [], me, globals.hoppieStandaloneShim, err);
        if (size(err) > 0) {
            result = { "error": err };
        }
        else {
            result = { "value": value };
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
    };
})();
