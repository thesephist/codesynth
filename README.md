# Codesynth 🎹

Generate music from your source code! You can try it live at [codesynth.surge.sh](https://codesynth.surge.sh). Codesynth is built with:

- The [Web Audio API](https://developer.mozilla.org/en-US/docs/Web/API/Web_Audio_API) for audio synthesis
- [Torus](https://github.com/thesephist/torus) for the player and UI
- [paper.css](https://github.com/thesephist/paper.css) for a lightweight visual style

![Codesynth screenshot](assets/screenshot.png)

## How does it work?

From the "How does it work?" section of Codesynth:

>Codesynth translates each line of source code into a tune by looking at the indentation and length of each line.
>
>A farther indent indicates a higher pitch, and a longer line means that pitch is held for more beats. Codesynth uses a basic pentatonic scale in A.

## Deploy and build

Codesynth is an application in a [single JavaScript file](static/js/main.js), and has no build step for modern browsers.

To deploy Codesynth, I use `yarn deploy` which uses Surge to deploy the static site.