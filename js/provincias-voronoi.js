"use strict";

var width = 780;
var height = 500;

var svg = d3.select("#container").append("svg")
    .attr("width", width)
    .attr("height", height);

d3.json("/files/provincias-topo.json", function(error, arg){
    if (error) return console.error(error);

    var proj = d3.geo.albers()
        .center([-33, -64])
        .rotate([0, 0, 0])
        .parallels([-20, -56])
        .scale(361)
        //.translate([width / 2, height / 2]);

    var datum = topojson.feature(arg, arg.objects.provincias);

    svg.append("path")
      .datum(datum)
      .attr("d", d3.geo.path().projection(proj));
});
