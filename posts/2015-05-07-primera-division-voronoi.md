---
title: Primera Division Voronoi
author: Ezequiel A. Alvarez
tags: viz, fútbol, d3
---

Se me dio por hacer un mapa de los estadios de los equipos de Primera División
del fútbol argentino a la fecha.
Osea, tomando cada estadio como un punto en el plano, trazar el diagrama voronoi que se forma.

Además de ser completamente irrelevante, es un lindo ejercicio para practicar D3.

## Las shapes

Primero necesitamos dibujar la Argentina, para lo que necesitamos los shapefiles.
Mi fuente favorita es [Natural Earth](http://www.naturalearthdata.com/), específicamente la versión
*Admin 0 países* (para no entrar en discusiones de soberanía).

Para procesar shapefiles mi herramienta es [ogr2ogr](http://www.gdal.org/ogr2ogr.html),
parte del conjunto de herramientas de [gdal-bin](http://www.gdal.org/gdal_utilities.html).
No es fácil o simple de usar, pero es muy poderosa y sigue de cerca la especificación.

Mi workflow con shapefiles es muy simple:

1. Veo que trae el mismo por arriba con `ogrinfo -geom=no ne_10m_admin_0_countries.shp ne_10m_admin_0_countries` (-geom=no es crucial para poder ver algo además de muchas coordenadas).
2. Pasándolo por alguna herramienta de CLI, identifico las features que busco, en este caso `grep -i arg`.
3. Uso `ogr2ogr` para pasarlo a GeoJSON, específicamente `ogr2ogr -f GeoJSON argentina.json ne_10m_admin_0_countries.shp ne_10m_admin_0_countries -where 'SOV_A3 = "ARG"'`.

## El javascript

El código se puede ver en [el repo de este blog](https://github.com/alvare/clrnds-blog/blob/master/js/provincias-voronoi.js)

Mike Bostock tiene un formato llamado [TopoJSON](https://github.com/mbostock/topojson) que es lo que vamos a usar (porque es mas liviano mas que nada, no trae mas ventajas para esta visualización).

Primero que nada tenemos que elegir una proyección:

```javascript
var proj = d3.geo.albers()
    .center([0, -36])
    .rotate([65, 0, 0])
    .parallels([-20, -56])
    .scale(1700)
    .translate([width / 2, height_full / 2]);
```

Albers es muy linda y clásica, por lo que no hay mucha vuelta que darle,
aunque tardé un rato en encontrar una combinación que me diera el país derecho.
Se ve que lo mejor es darle un `center` vertical y buscar la feature con `rotate` que hacer todo en uno.

Luego tenemos que agarrar la frontera de la Argentina y transformarla en un `path` de SVG y en un array de puntos.
El primero para dibujarlo, y el segundo para recortar la geometría voronoi.

```javascript
var border = topojson
    .feature(arg, arg.objects.argentina);

var path = d3.geo.path()
    .pointRadius(2)
    .projection(proj);

svg.append('path')
    .datum(border)
    .style({
        'stroke': 'black',
        'fill': '#eee'
    })
    .attr('class', 'argentina')
    .attr('d', path);

var voronoi = d3.geom.voronoi();

var points = equipos.features.map(function(d){
    return proj(d.geometry.coordinates);
});

var exterior = projectLineString(border, proj);

var voronois = voronoi(points).map(function(d){
    return d3.geom.polygon(d).clip(exterior.slice());
});
```

El resto son detalle estilísticos.

Además, en el código final, puse todo en una función parametrizada por la
proyección, para poder dibujar el segunda mapa con "zoom".

## El mapa

<div id="container-full"></div>

y con detalle en Buenos Aires:

<div id="container-small"></div>

Me gustaría dibujar los partidos y provincias en otro contraste pero no pude hacer que quede lindo.

D3 es una de mis librerías favoritas, la uso para hacer tanto para SVG como Canvas y HTML.
Es muy versátil y super eficiente, aunque a la documentación a veces le falta (pero bueno, expone mil funciones).

Además, al feeling funcional no hay con que darle.

<script src="/js/async.js"></script>
<script src="/js/topojson.min.js"></script>
<script src="/js/provincias-voronoi.js"></script>
<style>
path {
    stroke-linejoin: round;
    pointer-events: all;
    fill: none;
}
.active path {
    fill: rgba(36, 132, 193, 0.22);
}
.label {
    font-family: verdana;
    font-size: 16px;
    fill: black;
    cursor: default;
}
.label-small {
    font-size: 11px;
    fill: #222;
}
path.dimmed {
    opacity: 0.1;
}
</style>
