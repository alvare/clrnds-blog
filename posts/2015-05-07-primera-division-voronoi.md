---
title: Primera Division Voronoi
author: Ezequiel A. Alvarez
tags: cartogramas, futbol, d3
---

Este es un mapa de los estadios de los equipos de Primera División
del fútbol argentino a la fecha.
Tomando cada estadio como un punto en el plano, trazamos el diagrama voronoi que forman.

<div id="container-full"></div>

Detalle de Buenos Aires.

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
