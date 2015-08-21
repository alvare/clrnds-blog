'use strict';

async.map(['/files/argentina-topo.json',
           '/files/equipos-geo.json'],
           d3.json,
function(error, results){
    if (error) return console.error(error);

    var arg = results[0];
    var equipos = results[1];

    var width = d3.select('#article').node().getBoundingClientRect().width;


    var height_full = 640;
    var proj_full = d3.geo.albers()
        .center([0, -36])
        .rotate([65, 0, 0])
        .parallels([-20, -56])
        .scale(1700)
        .translate([width / 2, height_full / 2]);

    render(arg, equipos, '#container-full', width, height_full, proj_full);

    var height_small = 500;
    var proj_small = d3.geo.albers()
        .center([0, -34.8])
        .rotate([58.4, 0, 0])
        .parallels([-20, -56])
        .scale(29000)
        .translate([width / 2, height_small / 2]);

    render(arg, equipos, '#container-small', width, height_small, proj_small);
});

function render(arg, equipos, selector, width, height, proj){
    var dest = d3.select(selector);
    var svg = dest.append('svg')
        .attr('width', width)
        .attr('height', height);

    var border = topojson
        .feature(arg, arg.objects.argentina);

    var path = d3.geo.path()
        .pointRadius(2)
        .projection(proj);

    // border
    svg.append('path')
        .datum(border)
        .style({
            'stroke': 'black',
            'fill': '#eee'
        })
        .attr('class', 'argentina')
        .attr('d', path);

    // voronois
    var voronoi = d3.geom.voronoi();

    var points = equipos.features.map(function(d){
        return proj(d.geometry.coordinates);
    });

    var exterior = projectLineString(border, proj);

    var voronois = voronoi(points).map(function(d){
        return d3.geom.polygon(d).clip(exterior.slice());
    });

    svg.append('g').selectAll('.voronoi')
        .data(d3.zip(voronois, equipos.features))
      .enter().append('g')
        .on('mouseenter', highlight)
        .on('mouseleave', unhighlight)
      .append('path')
        .style({
            'stroke': 'black',
            'stroke-opacity': '0.5',
            'stroke-width': '.5px'
        })
        .attr("d", function(d){
            return "M" + d[0].join("L") + "Z";
        });

    // stadiums
    svg.append('path')
        .datum(equipos)
        .style({
            'fill': '#333'
        })
        .attr('d', path);

    function highlight(d){
        var p = proj(d[1].geometry.coordinates);
        var isleft = p[0] < width / 2;
        var t = d3.select(this)
            .classed('active', true);

        t.append('text')
            .attr('class', 'label')
            .attr('x', p[0])
            .attr('y', p[1])
            .attr('dy', '-0em')
            .attr('dx', isleft ? '10px' : '-10px')
            .attr('text-anchor', isleft ? 'start' : 'end')
            .text(d[1].properties.estadio);

        t.append('text')
            .attr('class', 'label label-small')
            .attr('x', p[0])
            .attr('y', p[1])
            .attr('dy', '1.5em')
            .attr('dx', isleft ? '10px' : '-10px')
            .attr('text-anchor', isleft ? 'start' : 'end')
            .text(d[1].properties.club);
    }

    function unhighlight(d){
        d3.select(this)
            .classed('active', false);
        svg.selectAll('.label')
            .remove();
    }
}

function projectLineString(feature, projection){

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

function noop(){};
