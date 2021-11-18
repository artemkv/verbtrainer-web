import "./main.css";
import "./spinner.css";

// this is how we fetch labels
// initially we pass them as flags
// to support language switch, we would need to fetch the labels from another json file
// and pass it back to elm to decode
const translations = require('../translations/translations.en.json');

// inject bundled Elm app into div#app
const { Elm } = require('./Main.elm');
const app = Elm.Main.init({
    node: document.getElementById("app"),
    flags: translations
})

const spanishLabels = {
    firstSingular: "Yo",
    secondSingular: "Tú",
    thirdSingular: "Él/ella",
    firstPlural: "Nosotros",
    secondPlural: "Vosotros",
    thirdPlural: "Ellos/ellas"
};

const exerciseListSpanishPresente = require('./data/exercises/es/presente.json');
const exerciseListSpanishFuturo = require('./data/exercises/es/futuro.json');

const exercisesSpanishPresente = assembleExercises("presente", exerciseListSpanishPresente, "Presente", spanishLabels);
const exercisesSpanishFuturo = assembleExercises("futuro", exerciseListSpanishFuturo, "Futuro", spanishLabels);

function assembleExercises(listId, exerciseList, tense, labels) {
    const exercises = {};

    exerciseList.forEach((exercise, idx) => {
        let nextIdx = idx + 1;
        if (nextIdx >= exerciseList.length) {
            nextIdx = 0;
        }
        let nextExercise = exerciseList[nextIdx];

        let exerciseData = {
            id: exercise.id,
            listId,
            verb: exercise.verb,
            tense,
            labels,
            answers: exercise.answers,
            exercisesInList: exerciseList.length,
            next: {
                verb: nextExercise.verb,
                id: nextExercise.id
            }
        };
        exercises[exercise.id] = exerciseData;
    });

    return exercises;
}

const exerciseListSpanishPresenteData = {
    id: "presente",
    title: "100 Spanish Verbs", // TODO: should be localized
    subtitle: "Presente",
    exercises: exerciseListSpanishPresente.map(x => ({
        id: x.id,
        name: x.name
    }))
}

const exerciseListSpanishFuturoData = {
    id: "futuro",
    title: "100 Spanish Verbs", // TODO: should be localized
    subtitle: "Futuro",
    exercises: exerciseListSpanishFuturo.map(x => ({
        id: x.id,
        name: x.name
    }))
}

const exerciseBook = {
    title: "Castilian Spanish", // TODO: should be localized
    subtitle: "All exercises", // TODO: should be localized
    exerciseLists: [exerciseListSpanishPresenteData, exerciseListSpanishFuturoData].map(x => ({
        id: x.id,
        name: x.subtitle
    }))
};

// PORTS

// Exercise data

const DATA_LOAD_DELAY = 0;

app.ports.requestExerciseBookData.subscribe(function () {
    let result = {
        isOk: true,
        data: exerciseBook
    };

    setTimeout(function () {
        app.ports.exerciseBookDataReceived.send(JSON.stringify(result));
    }, DATA_LOAD_DELAY);
});

app.ports.requestExerciseListData.subscribe(function (id) {
    let result;
    switch (id) {
        case "presente":
            result = {
                isOk: true,
                data: exerciseListSpanishPresenteData
            };
            break;
        case "futuro":
            result = {
                isOk: true,
                data: exerciseListSpanishFuturoData
            };
            break;
        default:
            result = {
                isOk: false,
                data: {
                    err: `Exercise list with id '${id}' not found.`
                }
            };
    };

    setTimeout(function () {
        app.ports.exerciseListDataReceived.send(JSON.stringify(result));
    }, DATA_LOAD_DELAY);
});

app.ports.requestExerciseData.subscribe(function ([listId, id]) {
    let result = {
        isOk: false,
        data: {
            err: `Exercise with id '${id}' not found in list '${listId}'.`
        }
    };

    switch (listId) {
        case "presente":
            if (id in exercisesSpanishPresente) {
                result = {
                    isOk: true,
                    data: exercisesSpanishPresente[id]
                }
            }
            break;
        case "futuro":
            if (id in exercisesSpanishFuturo) {
                result = {
                    isOk: true,
                    data: exercisesSpanishFuturo[id]
                }
            }
            break;
        default:
        // return default result
    }

    setTimeout(function () {
        app.ports.exerciseDataReceived.send(JSON.stringify(result));
    }, DATA_LOAD_DELAY);
});


// Exercise progress

const initialProgress = {
    version: 2,
    presente: [],
    futuro: []
};

let progress = initialProgress;
let progressText = localStorage.getItem("PROGRESS");
if (!progressText) {
    localStorage.setItem("PROGRESS", JSON.stringify(progress));
} else {
    const restoredProgress = JSON.parse(progressText);
    // Handle migrations
    if (restoredProgress.version) {
        progress = restoredProgress;
    } else {
        progress.presente = restoredProgress;
    }
}

app.ports.requestExerciseListProgressData.subscribe(function (id) {
    let result;
    switch (id) {
        case "presente":
            result = {
                isOk: true,
                data: {
                    id,
                    exercises: progress.presente
                }
            }
            break;
        case "futuro":
            result = {
                isOk: true,
                data: {
                    id,
                    exercises: progress.futuro
                }
            }
            break;
        default:
            result = {
                isOk: false,
                data: {
                    err: `Exercise list with id '${id}' not found.`
                }
            };
    };

    setTimeout(function () {
        app.ports.exerciseListProgressDataReceived.send(JSON.stringify(result));
    }, DATA_LOAD_DELAY);
});

app.ports.sendExerciseProgressData.subscribe(function ([listId, id, isPerfect]) {
    let listProgress;
    switch (listId) {
        case "presente":
            listProgress = progress.presente;
            break;
        case "futuro":
            listProgress = progress.futuro;
            break;
        default:
            result = {
                isOk: false,
                data: {
                    err: `Exercise list with id '${id}' not found.`
                }
            };
    };

    let exerciseProgress = listProgress.find(x => x.id === id);
    if (exerciseProgress) {
        exerciseProgress.isPerfect = isPerfect;
    } else {
        exerciseProgress = {
            id,
            isPerfect
        };
        listProgress.push(exerciseProgress);
    }
    localStorage.setItem("PROGRESS", JSON.stringify(progress));

    let result = {
        isOk: true,
        data: {
            id: "presente",
            exercises: listProgress
        }
    }
    setTimeout(function () {
        app.ports.exerciseListProgressDataReceived.send(JSON.stringify(result));
    }, DATA_LOAD_DELAY);
});