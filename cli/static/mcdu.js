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
    'lightgreen',
    'lightyellow',
    'lightblue',
    'lightmagenta',
    'lightcyan',
    'white'
];

function renderScreen(txtScreen) {
    var ofs = 0;
    var mcduScreen = document.getElementById('mcduScreen');
    mcduScreen.innerHTML = '';
    for (var y = 0; y < screenH; y++) {
        for (var x = 0; x < screenW; x++) {
            colorIndex = Number.parseInt('0x' + txtScreen.substr(ofs, 2));
            glyph = txtScreen.substr(ofs+2, 1);
            ofs += 3;
            var cell = document.createElement('span');
            cell.innerText = glyph;
            cell.setAttribute('style', 'color:' + colors[colorIndex & 0x0f] + ';');
            mcduScreen.appendChild(cell);
        }
        mcduScreen.appendChild(document.createElement('br'));
    }
}

function receiveScreen() {
    var xhr = new XMLHttpRequest();
    xhr.open("GET", "/screen");
    xhr.onloadend = (ev) => {
        renderScreen(xhr.responseText);
        receiveScreen();
    };
    xhr.send();
}

receiveScreen();
