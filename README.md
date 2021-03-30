To compile to JS:

```
elm make src/VerbConjugator.elm --output vc.js
```

# Build

## Individual Build Steps

```
npm run clean
```

Cleans the dist folder

```
npm run test
```

Runs unit-tests

```
npm run build:dev
```

Runs webpack using dev configuration

```
npm run build:prod
```

Runs webpack using prod configuration

## Full Builds

```
npm run dev
```

Runs the full dev build

```
npm run prod
```

Runs the full prod build

## Testing Environment

```
npm run start
```

Runs full dev build and starts the server

```
npm run startprod
```

Runs full prod build and starts the server
