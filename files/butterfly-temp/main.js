"use setrict";

d3.queue()
.defer(d3.json, '/files/butterfly-temp/earth.json')
.defer(d3.csv, '/files/butterfly-temp/1992.csv')
.awaitAll((err, data) => {
    if (err) throw err;

    let ratio = 520/900;
    var width = d3.select('#map').node().getBoundingClientRect().width;
    let height = width*ratio

    let svg = d3.select('#map')
        .append('svg')
        .attr('width', width)
        .attr('height', height);

    let geo = topojson.feature(data[0], data[0].objects.naturalearth_lowres);

    let extent = [[0, 0], [width, height]];

    let proj = d3.geoPolyhedralWaterman()
        .fitExtent(extent, geo);

    let cities = data[1].map(d => {
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

    if (height > 300) {
        let legend_y = height / 2;
        svg.append('g')
            .attr('class', 'legend')
            .attr('transform', 'translate(10, ' + legend_y + ')')
            .call(legend);
    }


    // polygons
    svg.append('g')
        .attr('class', 'earth')
        .selectAll('path')
        .data(geo.features)
      .enter().append('path')
        .attr("clip-path", "url(#clip)")
        .attr('d', path);

    svg.append('g')
        .attr('class', 'voronoi')
        .selectAll('path')
        .data(voro(cities).polygons())
      .enter().append('path')
        .attr('fill', d => d ? scale(d.data.avg) : 'none')
        .attr("clip-path", "url(#clip)")
        .attr('d', d => d ? 'M' + d.join('L') + 'Z' : null);

});
