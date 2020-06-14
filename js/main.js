const {
    Component,
} = window.Torus;

const AudioContext = window.AudioContext || window.webkitAudioContext;

const SINGLE_TICK = 150;
const SPACE_PER_TAB = 8;
// based on 100-char line being 4 bears
const BEATS_PER_CHAR = 4 / 100;

const DEFAULT_INPUT = `const http = require('http');

const hostname = '127.0.0.1';
const port = 3000;

const server = http.createServer((req, res) => {
  res.statusCode = 200;
  res.setHeader('Content-Type', 'text/plain');
  res.end('Hello World');
});

server.listen(port, hostname, () => {
  console.log('Server running.')
});`;

const Scale = [
    440,
    495,
    556.875,
    660,
    742.5,
    2 * 440,
    2 * 495,
    2 * 556.875,
    2 * 660,
    2 * 742.5,
];

function gcd(a, b) {
    if (a >= b) {
        const tmp = a;
        a = b;
        b = tmp;
    }

    let R;
    while ((a % b) > 0)  {
        R = a % b;
        a = b;
        b = R;
    }
    return b;
}

function gcds(numbers) {
    const uniqs = [...new Set(numbers).values()];
    if (uniqs[0] == 0)  {
        uniqs.shift();
    }
    if (!uniqs.length) {
        return 2;
    }
    return uniqs.reduce(gcd, uniqs[0]);
}

function debounce(fn, delayMillis) {
    let lastRun = 0;
    let to = null;
    return (...args) => {
        clearTimeout(to);
        const now = Date.now();
        const dfn = () => {
            lastRun = now;
            fn(...args);
        }
        if (now - lastRun > delayMillis) {
            dfn()
        } else {
            to = setTimeout(dfn, delayMillis);
        }
    }
}

function pentatonic(index) {
    if (index < Scale.length) {
        return Scale[index];
    }

    return Scale[Scale.length - 1];
}

function times(ss, n) {
    let s = '';
    while (n --> 0) {
        s += ss;
    }
    return s;
}

function tabToSpaces(s) {
    return s.replace(/\t/g, times(' ', SPACE_PER_TAB));
}

function tick(ticks) {
    return new Promise(res => setTimeout(res, SINGLE_TICK * ticks));
}

/**
 * For the purposes of Codesynth, a "Beat"
 * is of shape
 *
 * {
 *      note: Note, // number
 *      duration: number, // how many ticks?
 * }
 */
function inputToBeatSequence(input, tabWidth) {
    const lines = input.trim().split('\n');

    const beats = [];
    for (const line of lines) {
        if (line.trim() == '') {
            beats.push({
                note: 0,
                duration: 1,
            });
        } else {
            const indents = (line.length - line.trimStart(' ').length) / tabWidth;
            const duration = ~~(line.trim().length * BEATS_PER_CHAR) + 1;
            beats.push({
                note: pentatonic(indents),
                duration,
            });
        }
    }

    return beats;
}

class Player {
    constructor() {
        this.ctx = new AudioContext();
        this.oscillator = this.ctx.createOscillator();
        this.oscillator.type = 'sine';
        this.oscillator.connect(this.ctx.destination);

        this.stopped = false;
    }

    async play(beats, afterBeat, onInterrupt) {
        this.oscillator.start();

        beats = beats.slice(); // clone, since we'll shift()
        while (beats.length) {
            if (this.stopped) {
                onInterrupt();
                return;
            };

            const beat = beats.shift();
            afterBeat(beat);

            const {note, duration} = beat;
            this.oscillator.frequency.setValueAtTime(note, this.ctx.currentTime);
            await tick(duration);
        }

        this.stop();
    }

    stop() {
        this.stopped = true;
        this.oscillator.stop();
        this.ctx.close();
    }
}

function Popup(child) {
    return jdom`<div class="popup-shade">
        <div class="popup paper paper-border-top wrap">
            ${child}
        </div>
    </div>`;
}

class App extends Component {
    init() {
        this.input = '';
        this.tabWidth = 2;
        this.lineIdx = -1;
        this.showPopup = false;

        this.player = null;

        this.handleInput = this.handleInput.bind(this);
        this.handlePlay = this.handlePlay.bind(this);
        this.handleStop = this.handleStop.bind(this);
        this.save = debounce(this.save.bind(this), 800);

        this.restore();
        this.setTabWidth();
    }

    save() {
        window.localStorage.setItem('v0', this.input);
    }

    restore() {
        try {
            const input = window.localStorage.getItem('v0');
            if (input) {
                this.input = input;
            } else {
                this.input = DEFAULT_INPUT;
            }
        } catch (e) {
            console.error(`Error restoring editor state!`, e);
        }
    }

    handleInput(evt) {
        const input = evt.target.value;
        this.input = tabToSpaces(input);

        this.setTabWidth();
        this.render();

        this.save();
    }

    setTabWidth() {
        const lines = this.input.trim().split('\n');
        const indents = [];
        for (const line of lines) {
            indents.push(line.length - line.trimStart(' ').length);
        }
        this.tabWidth = gcds(indents);
    }

    async handlePlay() {
        const beats = inputToBeatSequence(this.input, this.tabWidth);

        let interrupted = false;
        this.player = new Player();
        await this.player.play(beats, () => {
            this.lineIdx ++;
            this.render();
        }, () => {
            interrupted = true;
        });

        if (!interrupted) {
            this._cleanup();
        }
    }

    handleStop() {
        this.player.stop();
        this._cleanup();
    }

    _cleanup() {
        this.lineIdx = -1;
        this.player = null;
        this.render();
    }

    compose() {
        function Line(line, isHighlighted) {
            return jdom`<div class="line ${isHighlighted ? 'highlighted' : ''}">
                <pre>${line}</pre>
            </div>`
        }

        let view = null;
        if (this.player === null) {
            view = jdom`<div class="editor paper wrap">
                <textarea id="cs-code" name="cs-code" cols="30" rows="10"
                    autofocus
                    spellcheck="${false}"
                    placeholder="type some code..."
                    oninput="${this.handleInput}"
                    value="${this.input}"></textarea>
                <div class="paper paper-border-right tabWidth">
                    tab = ${this.tabWidth} spaces
                </div>
            </div>`;
        } else {
            const lines = this.input.trim().split('\n');

            // scroll to line visible in player
            const playerContainer = this.node.querySelector('.player');
            let playerOffset = 0;
            if (playerContainer) {
                const {height} = playerContainer.getBoundingClientRect();
                playerOffset = this.lineIdx * 25.6 - height / 2 + 100;
            }
            if (playerOffset < 0) {
                playerOffset = 0;
            }

            view = jdom`<div class="player paper wrap">
                <div class="player-lines"
                    style="transform:translateY(-${playerOffset}px)">
                    ${lines.map((line, i)=> {
                        return Line(line, this.lineIdx == i)
                    })}
                </div>
            </div>`;
        }

        return jdom`<div class="app ${this.player ? 'playing' : 'stopped'}">
            <header class="accent paper">
                <h1>Codesynth</h1>
                <div class="metas">
                    <p class="meta">
                        Generate music from your source code.
                        <a href="#" class="no-wrap" onclick="${() => {
                            this.showPopup = true;
                            this.render();
                        }}">How does it work?</a>
                    </p>
                </div>
            </header>
            <div class="buttons">
                <div class="left">
                    <button class="movable paper button onStopped" onclick="${this.handlePlay}">
                        Play
                    </button>
                    <button class="movable paper button onPlaying" onclick="${this.handleStop}">
                        Stop
                    </button>
                </div>
                <div class="right">
                    <a class="movable paper paper-border-right button"
                        href="https://thesephist.com"
                        target="_blank">
                        <span class="desktop">Project</span>
                        by @thesephist
                    </a>
                </div>
            </div>
            ${view}
            ${this.showPopup ? Popup(jdom`<div class="howDoesItWork">
                <p>
                    <strong>How does Codesynth work?</strong>
                </p>
                <p>
                    Codesynth translates each line of source code into
                    a tune by looking at the <em>indentation</em> and
                    <em>length</em> of each line.
                </p>
                <p>
                    A farther indent indicates a higher pitch, and a
                    longer line means that pitch is held for more beats.
                    Codesynth uses a basic pentatonic scale in C.
                </p>
                <p>
                    Codesynth is
                    <a href="https://github.com/thesephist/codesynth"
                        target="_blank">open-source on GitHub</a>.
                </p>
                <button class="closeButton accent movable paper button"
                    onclick="${() => {
                        this.showPopup = false;
                        this.render();
                    }}">
                    Close
                </button>
            </div>`) : null}
        </div>`;
    }
}

const app = new App();
document.getElementById('root').appendChild(app.node);

