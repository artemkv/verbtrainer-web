import "./main.css";
import "./spinner.css";

// inject bundled Elm app into div#app
const { Elm } = require('./Main.elm');
const app = Elm.Main.init({
    node: document.getElementById("app")
})


// TODO: the JS part didn't get enough attention, hard-coded and dirty

const exerciseList = require('./exercises/es/presente.json');
const exercises = {};

exerciseList.forEach((exercise, idx) => {
    let nextIdx = idx + 1;
    if (nextIdx >= exerciseList.length) {
        nextIdx = 0;
    }
    let nextExercise = exerciseList[nextIdx];

    let exerciseData = {
        id: exercise.id,
        listId: "presente",
        verb: exercise.verb,
        tense: "Presente",
        labels: {
            firstSingular: "Yo",
            secondSingular: "Tú",
            thirdSingular: "",
            firstPlural: "",
            secondPlural: "",
            thirdPlural: ""
        },
        answers: exercise.answers,
        exercisesInList: exerciseList.length,
        next: {
            verb: nextExercise.verb,
            id: nextExercise.id
        }
    };
    exercises[exercise.id] = exerciseData;
});

app.ports.requestExerciseListData.subscribe(function (id) {
    let result = {
        isOk: true,
        data: {
            id: "presente",
            title: "100 Spanish Verbs", // TODO: should be a label
            subtitle: "Presente", // TODO: should be a label
            exercises: exerciseList.map(x => ({
                id: x.id,
                name: x.name
            }))
        }
    }

    setTimeout(function () {
        app.ports.exerciseListDataReceived.send(JSON.stringify(result));
    }, 250);
});

app.ports.requestExerciseData.subscribe(function (id) {
    let result = {
        isOk: false,
        data: {
            err: "Could not load exercise data"
        }
    }
    if (id in exercises) {
        result = {
            isOk: true,
            data: exercises[id]
        }
    }

    setTimeout(function () {
        app.ports.exerciseDataReceived.send(JSON.stringify(result));
    }, 250);
});


let progress = [];
let progressText = localStorage.getItem("PROGRESS");
if (!progressText) {
    progress = [];
    localStorage.setItem("PROGRESS", JSON.stringify(progress));
} else {
    progress = JSON.parse(progressText);
}

app.ports.requestExerciseListProgressData.subscribe(function (id) {
    let result = {
        isOk: true,
        data: {
            id: "presente",
            exercises: progress
        }
    }
    setTimeout(function () {
        app.ports.exerciseListProgressDataReceived.send(JSON.stringify(result));
    }, 250);
});

app.ports.sendExerciseProgressData.subscribe(function ([id, isPerfect]) {
    let exerciseProgress = progress.find(x => x.id === id);
    if (exerciseProgress) {
        exerciseProgress.isPerfect = isPerfect;
    } else {
        exerciseProgress = {
            id,
            isPerfect
        };
        progress.push(exerciseProgress);
    }
    localStorage.setItem("PROGRESS", JSON.stringify(progress));

    let result = {
        isOk: true,
        data: {
            id: "presente",
            exercises: progress
        }
    }
    setTimeout(function () {
        app.ports.exerciseListProgressDataReceived.send(JSON.stringify(result));
    }, 250);
});