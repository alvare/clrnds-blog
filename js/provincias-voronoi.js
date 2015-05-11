'use strict';

var dest = d3.select('#container');
var width = dest.node().getBoundingClientRect().width;
var height = 640;

var svg = d3.select('#container').append('svg')
    .attr('width', width)
    .attr('height', height);

d3.json('/files/provincias-topo.json', function(error, arg){
    if (error) return console.error(error);

    var proj = d3.geo.albers()
        .center([0, -39])
        .rotate([65, 0, 0])
        .parallels([-20, -56])
        .scale(1000)
        .translate([width / 2, height / 2]);

    var datum = topojson
        .feature(arg, arg.objects.provincias);

    var path = d3.geo.path().projection(proj);

    svg.append('path')
      .datum(datum)
      .attr('d', path);
});
