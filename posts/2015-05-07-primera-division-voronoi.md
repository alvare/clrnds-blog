---
title: Primera Division Voronoi
author: Ezequiel A. Alvarez
tags: futbol, d3
---

Se me dió por hacer un mapa de los estadios de los equipos de Primera División
del fútbol argentino a la fecha.
Osea, tomando cada estadio como un punto en el plano, trazar el diagrama voronoi que se forma.

Además de ser completamente irrelevante, es un lindo ejercicio para aprender d3.

## ¿Cómo?

Primero necesitamos dibujar la Argentina, para lo que necesitamos los shapefiles.
Mi fuente favorita es [Natural Earth](http://www.naturalearthdata.com/), y elegí específicamente la versión
*Admin 0 países* (para no entrar en discusiónes de soberanía).

Mi herramienta favorita para procesar shapefiles es [ogr2ogr](http://www.gdal.org/ogr2ogr.html),
parte del conjunto de herramientas de [gdal-bin](http://www.gdal.org/gdal_utilities.html).
No es fácil o simple de usar, pero es muy poderosa y correcta.

Mi workflow con shapefiles es muy simple:

1. Veo que trae el mismo por arriba con `ogrinfo -geom=no ne_10m_admin_0_countries.shp ne_10m_admin_0_countries` (-geom=no es crucial para poder ver algo además de muchas coordenadas).
2. Pasandolo por alguna herramienta de CLI, identifico las features que busco, en este caso `grep -i arg`.
3. Uso `ogr2ogr` para pasarlo a GeoJSON, específicamente `ogr2ogr -f GeoJSON argentina.json ne_10m_admin_0_countries.shp ne_10m_admin_0_countries -where 'SOV_A3 = "ARG"'`.

## El mapa

<div id="container-full"></div>

y con detalle en Buenos Aires:

<div id="container-small"></div>

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
