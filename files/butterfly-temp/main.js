'use strict';

d3.queue()
.defer(d3.json, '/files/butterfly-temp/earth.json')
.defer(d3.csv, '/files/butterfly-temp/1992.csv')
.awaitAll((err, data) => {
    if (err) throw err;

    let earth_topo = data[0];
    let cities_csv = data[1];

    var width = 900;
    let height = 520;

    //remove previous
    d3.select('#map svg').remove();

    let svg = d3.select('#map')
        .append('svg')
        .attr('class', 'svg-content')
        .attr('preserveAspectRatio', 'xMinYMin meet')
        .attr('viewBox', `0 0 ${width} ${height}`)

    let geo = topojson.feature(earth_topo, earth_topo.objects.naturalearth_lowres);

    let extent = [[0, 0], [width, height]];

    let proj = d3.geoPolyhedralWaterman()
        .rotate([20, 0])
        .fitExtent(extent, geo);

    let cities = cities_csv.map(d => {
        d.point = proj([+d.long, +d.lat]);
        d.avg = +d.avg;
        return d;
    });

    let voro = d3.voronoi()
        .x(d => d.point[0])
        .y(d => d.point[1])
        .extent(extent);

    let scale = d3.scaleSequential(d3.interpolateInferno)
        .domain(d3.extent(cities, d => d.avg));

    let path = d3.geoPath().projection(proj);

    // SVG
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

    // legend
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

    svg.append('text')
        .attr('class', 'title')
        .attr('x', width / 2)
        .attr('y', 10)
        .attr('text-anchor', 'middle')
        .text('Average temperature by city for 1992');


    // voronoi paths
    svg.append('g')
        .attr('class', 'voronoi')
        .selectAll('path')
        .data(voro(cities).polygons())
      .enter().append('path')
        .attr('fill', d => d ? scale(d.data.avg) : 'none')
        .attr('clip-path', 'url(#clip)')
        .attr('d', d => d ? 'M' + d.join('L') + 'Z' : null);

    // earth paths
    svg.append('g')
        .attr('class', 'earth')
        .selectAll('path')
        .data(geo.features)
      .enter().append('path')
        .attr('clip-path', 'url(#clip)')
        .attr('d', path);
});

function render(earth_topo, cities_csv){
}
