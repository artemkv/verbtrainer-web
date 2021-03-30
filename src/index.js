import "./main.css";

// inject bundled Elm app into div#app
const { Elm } = require('./Main.elm');
Elm.Main.init({
    node: document.getElementById("app")
})