const screenW = 24;
const screenH = 14;

var ws = null;

function sendKey(keyname) {
    if (ws === null) {
        var xhr = new XMLHttpRequest();
        xhr.open("POST", "/key");
        xhr.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
        xhr.send("key=" + keyname);
    }
    else {
        ws.send(keyname);
    }
}

const colors = [
    'black',
    'darkred',
    'darkgreen',
    'darkyellow',
    'darkblue',
    'darkmagenta',
    'darkcyan',
    'silver',
    'lightgray',
    'red',
    'lime',
    'yellow',
    'lightblue',
    'magenta',
    'cyan',
    'white'
];

function unpackCell(data, i) {
    colorIndex = Number(data[i * 2]) + 8;
    glyph = data[i * 2 + 1];
    return [colorIndex, glyph];
}

function renderScreen(screenData) {
    var i = 0;
    var mcduScreen = document.getElementById('mcduScreen');
    var data = screenData.data;
    mcduScreen.innerHTML = '';
    for (var y = 0; y < screenH; y++) {
        for (var x = 0; x < screenW; x++) {
            var cellData = unpackCell(data, i);
            colorIndex = cellData[0];
            glyph = cellData[1];
            var cell = document.createElement('span');
            cell.id = 'cell_' + y + '_' + x;
            cell.innerText = glyph;
            cell.setAttribute('style', 'color:' + colors[colorIndex & 0x0f] + ';');
            mcduScreen.appendChild(cell);
            i += 1;
        }
        mcduScreen.appendChild(document.createElement('br'));
    }
}

function updateScreen(update) {
    var y = update.line;
    var data = update.data;
    for (var x = 0; x < screenW; x++) {
        var cellData = unpackCell(data, x);
        colorIndex = cellData[0];
        glyph = cellData[1];
        var cell = document.getElementById('cell_' + y + '_' + x);
        cell.innerText = glyph;
        cell.setAttribute('style', 'color:' + colors[colorIndex & 0x0f] + ';');
    }
}

function receiveScreen() {
    var xhr = new XMLHttpRequest();
    xhr.open("GET", "/screen");
    xhr.onload = (ev) => {
        renderScreen(JSON.parse(xhr.response));
    };
    xhr.send();
}

var wsReconnectDelay = 500;

function runWebsocket() {
    let url = "ws://" + window.location.host + "/websocket";
    console.log("Connecting to " + url + "...");
    ws = new WebSocket(url);
    ws.onopen = (ev) => {
        console.log("Connected to " + url + ".");
        wsReconnectDelay = 500;
    }
    ws.onmessage = (ev) => {
        updateScreen(JSON.parse(ev.data));
    }
    ws.onclose = (ev) => {
        ws = null;
        console.log("Websocket closed due to " + ev.reason);
        setTimeout(runWebsocket, wsReconnectDelay);
        wsReconnectDelay = Math.min(60000, wsReconnectDelay * 2);
    }
}

receiveScreen();

runWebsocket();
