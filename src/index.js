import "./main.css";

// inject bundled Elm app into div#app
const { Elm } = require('./Main.elm');
const app = Elm.Main.init({
    node: document.getElementById("app")
})

app.ports.requestExerciseListData.subscribe(function (id) {
    let result = {
        isOk: true,
        data: {
            id: "presente",
            title: "100 Spanish Verbs", // TODO: should be a label
            subtitle: "Presente", // TODO: should be a label
            exercises: [
                {
                    id: "hablar",
                    name: "hablar"
                },
                {
                    id: "estar",
                    name: "estar"
                },
                {
                    id: "ser",
                    name: "ser"
                }
            ]
        }
    }

    setTimeout(function () {
        app.ports.exerciseListDataReceived.send(JSON.stringify(result));
    }, 250);
});

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

    setTimeout(function () {
        app.ports.exerciseDataReceived.send(JSON.stringify(result));
    }, 250);
});