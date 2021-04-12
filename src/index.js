import "./main.css";

// inject bundled Elm app into div#app
const { Elm } = require('./Main.elm');
const app = Elm.Main.init({
    node: document.getElementById("app")
})

app.ports.requestExerciseData.subscribe(function (id) {
    // TODO: this is hard-coded implementation
    let hablar = {
        id: "hablar",
        verb: "Hablar",
        tense: "Presente",
        labels: {
            firstSingular: "Yo",
            secondSingular: "Tú",
            thirdSingular: "",
            firstPlural: "",
            secondPlural: "",
            thirdPlural: ""
        },
        answers: {
            firstSingular: ["hablo"],
            secondSingular: ["hablas"],
            thirdSingular: [""],
            firstPlural: [""],
            secondPlural: [""],
            thirdPlural: [""]
        },
        next: {
            verb: "Estar",
            id: "estar"
        }
    };

    let estar = {
        id: "estar",
        verb: "Estar",
        tense: "Presente",
        labels: {
            firstSingular: "Yo",
            secondSingular: "Tú",
            thirdSingular: "",
            firstPlural: "",
            secondPlural: "",
            thirdPlural: ""
        },
        answers: {
            firstSingular: ["estoy"],
            secondSingular: ["estás"],
            thirdSingular: [""],
            firstPlural: [""],
            secondPlural: [""],
            thirdPlural: [""]
        },
        next: {
            verb: "Ser",
            id: "ser"
        }
    };

    let result = {
        isOk: false,
        data: {
            err: "Could not load exercise data"
        }
    }
    if (id === "hablar") {
        result = {
            isOk: true,
            data: hablar
        }
    } else if (id === "estar") {
        result = {
            isOk: true,
            data: estar
        }
    }

    app.ports.exerciseDataReceived.send(JSON.stringify(result));
});