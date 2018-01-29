---
title: A Temperature Map
author: Ezequiel A. Alvarez
tags: viz, d3, waterman
---

<link rel="stylesheet" type="text/css" href="/files/butterfly-temp/styles.css" />

Yesterday I finally decided to port our D3 v3 viz code to the latest D3 v4,
and it was a really pleasant experience actually!
This came as a nice surprise, since most JavaScript libraries
don't offer a nice upgrading experience, in my opinion
(`react-router` I'm looking at you).

Also, this semester me and some class mates did a lot of viz work for our [Numerical Methods class](http://www-2.dc.uba.ar/materias/metnum/homepage.html)
which resulted in some [really](/files/hist-cant-gamma-k.png) [nice](/files/ranking_distance.png) [graphics](/files/world_temp_cross_full_poly.png).

Anyway, during this time it ocurred to me how silly it would be to do a pseudo-statistical map on a polyhedral projection.
So, finally with some time available, I decided to do [another](/posts/2015-05-07-primera-division-voronoi.html) voronoi map!

<div id="map" class="center"></div>

Let me be clear: this is the least statistically significant map that can be drawn. Don't show it to your friends.

But also, it looks amazing! Show it to your boss!

(For example, some Antartida zones fall inside some California city's polygon, so yeah, nope)

### How?

The temperature data is from [Kaggle](http://kaggle.com/berkeleyearth/climate-change-earth-surface-temperature-data) and the planet from our beloved [Natural Earth](http://naturalearthdata.com).

First of all, the data is grouped by year so we have the average temperature for each city for a given year. For this map I chose 1992. Secondly, we take each city and project it's latitude and longitude using `d3.geoPolyhedralWaterman()`.

Finally, we separate the space using a voronoi diagram so each city is given a polygon that contains all the points it is closest to.

Wanna hear awesome? Initially I used `d3.geoEquirectangular()` for tying everything up, only at the end I switched to the butterfly and it worked at once!

Also I'm using Susie Lu's excellent [d3-legend](http://d3-legend.susielu.com/). The legend's entire code is:

```javascript
let legend = d3.legendColor()
    .shapeWidth(30)
    .shapePadding(0)
    .labelFormat(d => d3.format('.0f')(d) + ' ºC')
    .labelAlign('start')
    .cells(8)
    .orient('vertical')
    .ascending(true)
    .scale(scale);

let legend_y = height / 2;
svg.append('g')
    .attr('class', 'legend')
    .attr('transform', 'translate(10, ' + legend_y + ')')
    .call(legend);
```

Another interesting bit is the clip path. Since the Waterman projection is not continuous, we have to cut some pieces of the map away. For that we use two things: that `d3.geoPath()` and the projection know how to render a `{type: 'Sphere'}` GeoJSON datum, and an svg `clipPath` in a `<defs>` element (which we also use to render the limit of the world). The full code for the clipping and limit drawing is:

```javascript
var defs = svg.append('defs');

defs.append('path')
    .datum({type: 'Sphere'})
    .attr('id', 'sphere')
    .attr('d', path);

defs.append('clipPath')
    .attr('id', 'clip')
  .append('use')
    .attr('xlink:href', '#sphere');

svg.append('use')
    .attr('class', 'limit')
    .attr('xlink:href', '#sphere');
```

Followed by `.attr('clip-path', 'url(#clip)')` on the `<path>` elements we want to clip. So simple!

### Anyway

Another horribly bad thing going on in this map: `d3.voronoi()` is two dimensional, it's not made for spherical coordinates. This is greatly discussed [here](https://github.com/d3/d3/issues/1820). I have over 3500 cities here so using the `O(n^2)` algorithm mentioned there is just not possible (my tab crashed instantly).

I hope you liked it or found it useful. The code is [over at GitHub](https://github.com/alvare/clrnds-blog/tree/master/files/butterfly-temp/main.js), like always.

Cheers!

<script src="https://unpkg.com/d3@4"></script>
<script src="https://unpkg.com/lodash@4"></script>
<script src="https://unpkg.com/topojson@3"></script>
<script src="https://unpkg.com/d3-geo-projection@2"></script>
<script src="https://cdnjs.cloudflare.com/ajax/libs/d3-legend/2.24.0/d3-legend.min.js"></script>
<script src="/files/butterfly-temp/main.js"></script>
