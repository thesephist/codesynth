const {
    Component,
} = window.Torus;

const AudioContext = window.AudioContext || window.webkitAudioContext;

const SINGLE_TICK = 150;
const SPACE_PER_TAB = 8;

// based on 100-char line being 4 bears
const BEATS_PER_CHAR = 4 / 100;

const DEFAULT_INPUT = `export default function App() {
  return (
    <Router>
      <div>
        <nav>
          <ul>
            <li>
              <Link to="/">Home</Link>
            </li>
            <li>
              <Link to="/about">About</Link>
            </li>
            <li>
              <Link to="/users">Users</Link>
            </li>
          </ul>
        </nav>

        {/* A <Switch> looks through its children <Route>s and
            renders the first one that matches the current URL. */}
        <Switch>
          <Route path="/about">
            <About />
          </Route>
          <Route path="/users">
            <Users />
          </Route>
          <Route path="/">
            <Home />
          </Route>
        </Switch>
      </div>
    </Router>
  );
}`;

const EXAMPLES = [{
    name: 'React Router',
    value: DEFAULT_INPUT,
}, {
    name: 'Node.js server',
    value: `const http = require('http');

const hostname = '127.0.0.1';
const port = 8000;

const server = http.createServer((req, res) => {
  res.statusCode = 200;
  res.setHeader('Content-Type', 'text/plain');
  res.end('Hello, World!');
});

server.listen(port, hostname, () => {
  console.log('Server running!');
});`,
}, {
    name: 'Go server',
    value: `package main

import (
    "fmt"
    "net/http"
)

func main() {
    http.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
        fmt.Fprintf(w, "Hello, World! Requested path: %s\\n", r.URL.Path)
    })

    http.ListenAndServe(":80", nil)
}`,
}, {
    name: 'Haskell Bell numbers',
    value: `bellTri :: [[Integer]]
bellTri = map snd (iterate (f . uncurry (scanl (+))) (1,[1]))
  where
    f xs = (last xs,  xs)

bell :: [Integer]
bell = map head bellTri

main = do
  putStrLn "First 10 rows of Bell's Triangle"
  mapM_ print (take 10 bellTri)
  putStrLn "First 15 Bell numbers"
  mapM_ print (take 15 bell)
  putStrLn "50th Bell number"
  print (bell !! 49)`,
}, {
    name: 'Ink FizzBuzz',
    value: `std := load('std')

log := std.log
range := std.range
each := std.each

fizzbuzz := n => each(
        range(1, n + 1, 1)
        n => [n % 3, n % 5] :: {
                [0, 0] -> log('FizzBuzz')
                [0, _] -> log('Fizz')
                [_, 0] -> log('Buzz')
                _ -> log(n)
        }
)

fizzbuzz(100)`,
}, {
    name: 'Rust Mandelbrot set',
    value: `extern crate image;
extern crate num_complex;

use std::fs::File;
use num_complex::Complex;

fn main() {
    let max_iterations = 256u16;
    let img_side = 800u32;
    let cxmin = -2f32;
    let cxmax = 1f32;
    let cymin = -1.5f32;
    let cymax = 1.5f32;
    let scalex = (cxmax - cxmin) / img_side as f32;
    let scaley = (cymax - cymin) / img_side as f32;

    let mut imgbuf = image::ImageBuffer::new(img_side, img_side);

    for (x, y, pixel) in imgbuf.enumerate_pixels_mut() {
        let cx = cxmin + x as f32 * scalex;
        let cy = cymin + y as f32 * scaley;

        let c = Complex::new(cx, cy);
        let mut z = Complex::new(0f32, 0f32);

        let mut i = 0;
        for t in 0..max_iterations {
            if z.norm() > 2.0 {
                break;
            }
            z = z * z + c;
            i = t;
        }

        *pixel = image::Luma([i as u8]);
    }

    imgbuf.save("fractal.png").unwrap();
}`,
}];

// Pentatonic A scale
const SCALE = [
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
    4 * 440,
    4 * 495,
    4 * 556.875,
    4 * 660,
    4 * 742.5,
];

// greatest common divisor of numbers, used to automatically
// infer the tab width
function gcds(numbers) {
    const uniqs = [...new Set(numbers).values()].sort((a, b) => a - b);
    if (uniqs[0] === 0)  {
        uniqs.shift();
    }
    if (!uniqs.length) {
        return 2;
    }
    return uniqs.reduce((a, b) => {
        let R;
        while ((a % b) > 0)  {
            R = a % b;
            a = b;
            b = R;
        }
        return b;
    }, uniqs[0]);
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

// Get note on pentatonic scale, bounded by max note
function pentatonic(index) {
    if (index < SCALE.length) {
        return SCALE[index];
    }

    return SCALE[SCALE.length - 1];
}

// repeat the string ss, n times
function times(ss, n) {
    let s = '';
    while (n -- > 0) {
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

// For the purposes of Codesynth, a "Beat"
// is of shape
//
// {
//      note: Note, // number
//      duration: number, // how many ticks?
// }
function inputToBeatSequence(input, tabWidth) {
    const lines = input.trim().split('\n');

    const beats = [];
    for (const line of lines) {
        if (line.trim() === '') {
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
        this.oscillator.type = 'triangle';
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
            }

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
        this.showHelp = false;
        this.showExamples = false;

        this.player = null;

        this.handleInput = this.handleInput.bind(this);
        this.handlePlay = this.handlePlay.bind(this);
        this.handleStop = this.handleStop.bind(this);
        this.save = debounce(this.save.bind(this), 1000);

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
                this.setInput(input);
            } else {
                this.setInput(DEFAULT_INPUT);
            }
        } catch (e) {
            console.error('Error restoring editor state!', e);
        }
    }

    handleInput(evt) {
        const input = evt.target.value;
        this.setInput(input);
    }

    setInput(input) {
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
        if (this.player) {
            return;
        }

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
        if (!this.player) {
            return;
        }

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
                    placeholder="copy-paste some code..."
                    oninput="${this.handleInput}"
                    value="${this.input}"></textarea>
                <div class="paper paper-border-right tabWidth"
                    title="Codesynth inferred that a tab is ${this.tabWidth} spaces in this snippet">
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
                const fontSize = window.innerWidth > 370 ? 16 : 13;
                playerOffset = (this.lineIdx * (fontSize * 1.6)) - (height / 2) + 100;
            }
            if (playerOffset < 0) {
                playerOffset = 0;
            }

            view = jdom`<div class="player paper wrap">
                <div class="player-lines"
                    style="transform:translateY(-${playerOffset}px)">
                    ${lines.map((line, i) => {
                        return Line(line, this.lineIdx === i)
                    })}
                </div>
            </div>`;
        }

        const Example = ({name, value}) => {
            return jdom`<li class="examples-item movable paper paper-border-left"
                tabIndex="1"
                onclick="${() => {
                    this.setInput(value);
                    this.showExamples = false;
                    this.render();
                }}">
                <div class="name">${name}</div>
                <div class="value">
                    <pre>${value.split('\n').slice(0, 6).join('\n')}</pre>
                    <pre>...</pre>
                </div>
            </li>`;
        }

        return jdom`<div class="app ${this.player ? 'playing' : 'stopped'}">
            <header class="accent paper">
                <h1>
                    <a href="/">Codesynth</a>
                </h1>
                <div class="metas">
                    <p class="meta">
                        Generate music from your source code.
                        <a href="#" class="no-wrap help-link" onclick="${() => {
                            this.showHelp = true;
                            this.render();
                        }}">How does it work?</a>
                    </p>
                </div>
            </header>
            <div class="buttons">
                <div class="left">
                    <button class="movable colored paper button onStopped iconButton"
                        title="Play track"
                        onclick="${this.handlePlay}">
                        &#9654;
                        <span class="desktop">Play</span>
                    </button>
                    <button class="movable colored paper button onPlaying iconButton"
                        title="Stop playing"
                        onclick="${this.handleStop}">
                        &#9632;
                        <span class="desktop">Stop</span>
                    </button>
                    <button class="movable paper button onStopped"
                        title="Show example code snippets"
                        onclick="${() => {
                            this.showExamples = true;
                            this.render();
                        }}">
                        Examples
                    </button>
                </div>
                <div class="right">
                    <a class="movable paper paper-border-right button"
                        href="https://thesephist.com"
                        target="_blank">
                        <span class="desktop">Project by @thesephist</span>
                        <div class="mobile">by Linus</div>
                    </a>
                </div>
            </div>
            ${view}
            ${this.showHelp ? Popup(jdom`<div class="howDoesItWork">
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
                    Codesynth uses a basic pentatonic scale in A.
                </p>
                <p>
                    On iOS, make sure your device isn't muted. If it is,
                    Safari may not play any sound.
                </p>
                <div class="buttons modal-buttons">
                    <div class="left"></div>
                    <div class="right">
                        <a class="movable colored paper button" href="https://github.com/thesephist/codesynth"
                            target="_blank">
                            See on GitHub
                        </a>
                        <button class="closeButton accent movable paper button"
                            title="Close popup"
                            onclick="${() => {
                                this.showHelp = false;
                                this.render();
                            }}">
                            Close
                        </button>
                    </div>
                </div>
            </div>`) : null}
            ${this.showExamples ? Popup(jdom`<div class="examples">
                <p class="examples-title">
                    <strong>Example beats</strong>
                </p>
                <div class="examples-list-container">
                    <ul class="examples-list">
                        ${EXAMPLES.map(Example)}
                    </ul>
                </div>
                <div class="buttons modal-buttons">
                    <div class="left"></div>
                    <div class="right">
                        <button class="closeButton accent movable paper button"
                            title="Close popup"
                            onclick="${() => {
                                this.showExamples = false;
                                this.render();
                            }}">
                            Close
                        </button>
                    </div>
                </div>
            </div>`) : null}
        </div>`;
    }
}

const app = new App();
document.getElementById('root').appendChild(app.node);

