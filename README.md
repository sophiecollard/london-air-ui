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

In `index.html`, replace the following tag:

```html
<script src="https://london-air-ui.ams3.cdn.digitaloceanspaces.com/elm.min.js.gz"></script>
```

with:

```html
<script src="elm.js"></script>
```

Then, point your favourite browser to the location of `index.html`. Don't forget to undo this change before releasing to production!

### Production

From the root directory, build the application with:

```sh
elm make src/Main.elm --output elm.js --optimize
```

Minify the resulting `elm.js` file with:

```sh
uglifyjs elm.js -o elm.min.js
```

If running this command for the first time, you may need to install uglify with `npm install -g uglify-js`.

Finally, compress the resulting `elm.min.js` file with:

```sh
gzip -k elm.min.js
```

Upload the resulting `elm.min.js.gz` to [DigitalOcean spaces](https://www.digitalocean.com/products/spaces) or similar. Don't forget to enable the CDN feature on your bucket and to configure your object metadata to include the following headers:
  * `Content-Type: application/javascript`
  * `Content-Encoding: gzip`
