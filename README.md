# London Air UI

A user interface for viewing data from [London Air](https://www.londonair.org.uk/LondonAir/Default.aspx), built with [Elm](https://elm-lang.org/) 0.19.1 and [Leaflet JS](https://leafletjs.com/).

## Build

### Development

From the root directory, run:

```sh
elm make src/Main.elm --output elm.js
```

Then, point your favourite browser to the location of `index.html`.

### Production

From the root directory, run:

```sh
elm make src/Main.elm --output elm.js --optimize
```
