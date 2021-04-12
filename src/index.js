import "./main.css";

// inject bundled Elm app into div#app
const { Elm } = require('./Main.elm');
const app = Elm.Main.init({
    node: document.getElementById("app")
})

app.ports.requestExerciseData.subscribe(function (id) {
    let exerciseData = "{\"verb\": \"Hablar\",\"tense\": \"Presente\",\"labels\": {\"firstSingular\": \"Yo\",\"secondSingular\": \"Tú\"},\"answers\": {\"firstSingular\": [\"hablo\"],\"secondSingular\": [\"hablas\"]}}"
    app.ports.exerciseDataReceived.send(exerciseData);
});