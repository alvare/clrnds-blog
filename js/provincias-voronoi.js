'use strict';

var dest = d3.select('#container');
var width = dest.node().getBoundingClientRect().width;
var height = 640;

var svg = d3.select('#container').append('svg')
    .attr('width', width)
    .attr('height', height);

async.map(['/files/argentina-topo.json',
           '/files/equipos-geo.json'],
    d3.json,
    function(error, results){
        if (error) return console.error(error);

        var arg = results[0];
        var equipos = results[1];

        var proj = d3.geo.albers()
            .center([0, -39])
            .rotate([65, 0, 0])
            .parallels([-20, -56])
            .scale(1000)
            .translate([width / 2, height / 2]);

        var border = topojson
            .feature(arg, arg.objects.argentina);

        var path = d3.geo.path()
            .pointRadius(2)
            .projection(proj);

        var voronoi = d3.geom.voronoi()
            .clipExtent([[0, 0], [width, height]]);

        // border
        svg.append('path')
            .datum(border)
            .style({
                'stroke': 'black',
                'fill': 'none'
            })
            .attr('d', path);

        // stadiums
        svg.append('path')
            .datum(equipos)
            .attr('d', path);

        // voronois
        var points = equipos.features.map(function(d){
            return proj(d.geometry.coordinates);
        });

        var exterior = projectLineString(border, proj);

        svg.append('g').selectAll('.voronoi')
            .data(voronoi(points).map(function(d){
                    return d3.geom.polygon(d).clip(exterior.slice());
                })
            )
          .enter().append('path')
            .style({
                'stroke': 'black',
                'stroke-opacity': '0.5',
                'stroke-width': '.5px',
                'fill': 'none'
            })
            .attr("d", function(d){
                return "M" + d.join("L") + "Z";
            });
    });

function projectLineString(feature, projection) {
    function noop(){};

    var line;

    d3.geo.stream(feature, projection.stream({
        polygonStart: noop,
        polygonEnd: noop,
        lineStart: function() { line = []; },
        lineEnd: noop,
        point: function(x, y) { line.push([x, y]); },
        sphere: noop
    }));

    return line;
}

