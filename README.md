# London Air UI

A user interface for viewing air quality data from [London Air](https://www.londonair.org.uk/LondonAir/Default.aspx), built with [Elm](https://elm-lang.org/) 0.19.1 and [Leaflet JS](https://leafletjs.com/).

A live version of the app can be found at [https://london-air-ui.ams3.cdn.digitaloceanspaces.com/index.html](https://london-air-ui.ams3.cdn.digitaloceanspaces.com/index.html).

![Screenshot](screenshot.png)

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
