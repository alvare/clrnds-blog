'use strict';

var dest = d3.select('#container');
var width = dest.node().getBoundingClientRect().width;
var height = 500;

var svg = d3.select('#container').append('svg')
    .attr('width', width)
    .attr('height', height);

var color = d3.scale.category20();

d3.json('/files/provincias-topo.json', function(error, arg){
    if (error) return console.error(error);

    var proj = d3.geo.albers()
        .center([-64, -33])
        .rotate([55, 0, 0])
        .parallels([-20, -56])
        .scale(750)
        .translate([width / 2 - 520, 400]);

    var datum = topojson
        .feature(arg, arg.objects.provincias)
        .features;

    var path = d3.geo.path().projection(proj);

    svg.selectAll('.provincia')
      .data(datum)
    .enter().append('path')
      .attr('fill', function(e){ console.log(e.id);return color(e.id); })
      .attr('d', path);
});
