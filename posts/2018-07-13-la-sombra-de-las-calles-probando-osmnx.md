---
title: "La Sombra de las Calles: Probando OSMnx"
author: Ezequiel A. Alvarez
tags: python, viz, osmnx
---

Llegué a este [post](http://geoffboeing.com/2018/07/city-street-orientations-world/) sobre OSMnx y me gustó la idea, pero no me cerraba el gráfico.

Inspirado en eso y probablemente en estar leyendo Visual Explanations de Edward Tufte, se me ocurrió que tenia que haber una manera mas elegante de mostrar la dirección de las calles.

Justamente, las calles en sí son una visualización de su dirección, por lo que mostrando todas las calles "superpuestas" de alguna manera, deberían aparecer patrones.

Para esto, calculo el centroide para cada calle (por su nombre, lo que trae cierto error cuando hay nombres repetidos) y las traslado todas al origen, o sea el (0, 0).

Acá el [notebook](https://github.com/alvare/osmnx-calles-sombras/blob/master/notebook.ipynb) de jupyter.

Este es el resultado para las ciudades Argentinas:

<img class="img-responsive" src="/files/street_shadows_ar.png">

Y para algunas ciudades del original:

<img class="img-responsive" src="/files/street_shadows_us.png">
