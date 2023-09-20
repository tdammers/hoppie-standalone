const screenW = 24;
const screenH = 12;

function sendKey(keyname) {
    var xhr = new XMLHttpRequest();
    xhr.open("POST", "/key");
    xhr.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
    xhr.send("key=" + keyname);
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
    'lightred',
    'lime',
    'yellow',
    'lightblue',
    'magenta',
    'cyan',
    'white'
];

function renderScreen(screenData) {
    var i = 0;
    var mcduScreen = document.getElementById('mcduScreen');
    mcduScreen.innerHTML = '';
    for (var y = 0; y < screenH; y++) {
        for (var x = 0; x < screenW; x++) {
            colorIndex = screenData.data[i][0];
            glyph = String.fromCodePoint(screenData.data[i][1]);
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
        var cell = document.getElementById('cell_' + y + '_' + x);
        colorIndex = data[x][0];
        glyph = String.fromCodePoint(data[x][1]);
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

function runWebsocket() {
    var ws = new WebSocket("ws://" + window.location.host + "/screen/updates");
    ws.onmessage = (ev) => {
        updateScreen(JSON.parse(ev.data));
    }
}

receiveScreen();

runWebsocket();
