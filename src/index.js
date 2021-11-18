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
const exerciseListSpanishImperfecto = require('./data/exercises/es/imperfecto.json');
const exerciseListSpanishIndefinido = require('./data/exercises/es/indefinido.json');
const exerciseListSpanishFuturo = require('./data/exercises/es/futuro.json');

const exercisesSpanishPresente = assembleExercises("presente", exerciseListSpanishPresente, "Presente", spanishLabels);
const exercisesSpanishImperfecto = assembleExercises("imperfecto", exerciseListSpanishImperfecto, "Pretérito Imperfecto", spanishLabels);
const exercisesSpanishIndefinido = assembleExercises("indefinido", exerciseListSpanishIndefinido, "Pretérito Indefinido", spanishLabels);
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

const exerciseListSpanishImperfectoData = {
    id: "imperfecto",
    title: "100 Spanish Verbs", // TODO: should be localized
    subtitle: "Pretérito Imperfecto",
    exercises: exerciseListSpanishImperfecto.map(x => ({
        id: x.id,
        name: x.name
    }))
}

const exerciseListSpanishIndefinidoData = {
    id: "indefinido",
    title: "100 Spanish Verbs", // TODO: should be localized
    subtitle: "Pretérito Indefinido",
    exercises: exerciseListSpanishIndefinido.map(x => ({
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
    exerciseLists: [
        exerciseListSpanishPresenteData,
        exerciseListSpanishImperfectoData,
        exerciseListSpanishIndefinidoData,
        exerciseListSpanishFuturoData].map(x => ({
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
        case "imperfecto":
            result = {
                isOk: true,
                data: exerciseListSpanishImperfectoData
            };
            break;
        case "indefinido":
            result = {
                isOk: true,
                data: exerciseListSpanishIndefinidoData
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
        case "imperfecto":
            if (id in exercisesSpanishImperfecto) {
                result = {
                    isOk: true,
                    data: exercisesSpanishImperfecto[id]
                }
            }
            break;
        case "indefinido":
            if (id in exercisesSpanishIndefinido) {
                result = {
                    isOk: true,
                    data: exercisesSpanishIndefinido[id]
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
const EXERCISE_PROGRESS_LOCAL_STORAGE_KEY = `verbtrainer.org/progress`;

const VERSION_1 = 1;
const CURRENT_VERSION = VERSION_1;

const initialProgress = {
    version: CURRENT_VERSION,
    presente: [],
    imperfecto: [],
    indefinido: [],
    futuro: []
};

let progress = initialProgress;
let progressText = localStorage.getItem(EXERCISE_PROGRESS_LOCAL_STORAGE_KEY);
if (!progressText) {
    localStorage.setItem(EXERCISE_PROGRESS_LOCAL_STORAGE_KEY, JSON.stringify(progress));
} else {
    const restoredProgress = JSON.parse(progressText);
    // Here you can migrate the restored progress to the latest version
    progress = restoredProgress;
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
        case "imperfecto":
            result = {
                isOk: true,
                data: {
                    id,
                    exercises: progress.imperfecto
                }
            }
            break;
        case "indefinido":
            result = {
                isOk: true,
                data: {
                    id,
                    exercises: progress.indefinido
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
        case "imperfecto":
            listProgress = progress.imperfecto;
            break;
        case "indefinido":
            listProgress = progress.indefinido;
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
            setTimeout(function () {
                app.ports.exerciseListProgressDataReceived.send(JSON.stringify(result));
            }, DATA_LOAD_DELAY);
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
    localStorage.setItem(EXERCISE_PROGRESS_LOCAL_STORAGE_KEY, JSON.stringify(progress));

    let result = {
        isOk: true,
        data: {
            id: listId,
            exercises: listProgress
        }
    }
    setTimeout(function () {
        app.ports.exerciseListProgressDataReceived.send(JSON.stringify(result));
    }, DATA_LOAD_DELAY);
});