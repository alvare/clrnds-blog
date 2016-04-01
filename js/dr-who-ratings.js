'use strict';

var max_width = d3.select('#article').node().getBoundingClientRect().width;

var margin = {top: 20, right: 20, bottom: 30, left: 50};
var width = max_width - margin.left - margin.right;
var height = 300 - margin.top - margin.bottom;

var color = d3.scale.category10();

var xScale = d3.scale.ordinal()
    .rangeBands([10, width]);

var yScale = d3.scale.linear()
    .range([height - 20, 0])
    .domain([5, 10]);

var xAxis = d3.svg.axis()
    .scale(xScale)
    .orient("bottom")
    .tickFormat(function(d){
        return 'Season ' + d[0];
    });

var yAxis = d3.svg.axis()
    .scale(yScale)
    .ticks(5)
    .tickFormat(function(t){
        return d3.format('d')(t) + '★';
    })
    .orient("left");

var line = d3.svg.line()
    .x(function(d) { return xScale(d.index); })
    .y(function(d) { return yScale(d.score); });

var svg = d3.select("#viz").append("svg")
    .attr("width", width + margin.left + margin.right)
    .attr("height", height + margin.top + margin.bottom)
  .append("g")
    .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

var info = d3.select("#info")
    .style("opacity", "0");

d3.json('/files/dr-who-ratings.json', function(error, data){
    if (error) return console.error(error);

    window.dddd = data;
    var flatdata = _.flatten(data);

   _.forEach(data, function(s){
        return _.forEach(s, function(d){
            d.index = [d.season, d.number];
        })
    })

    xScale.domain(_.flatMap(data, function(s){ return _.map(s, 'index') }));

    xAxis.tickValues(_.map(data, function(s){ return s[0].index }))

    svg.append("g")
        .attr("class", "x axis")
        .attr("transform", "translate(0," + height + ")")
        .call(xAxis);

    svg.append("g")
        .attr("class", "y axis")
        .call(yAxis)
      .append("text")
        .attr("transform", "rotate(-90)")
        .attr("y", 6)
        .attr("dy", ".71em")
        .style("text-anchor", "end")
        .text("Rating");

    // regresion
    var reg = linearRegression(
            _.flatMap(data, function(d){
                return d.map(function(d2){return yScale(d2.score)})}),
            _.flatMap(data, function(d){
                return d.map(function(d2){return xScale(d2.index)})}))

    var slope = function(x){ return reg.intercept + x*reg.slope }

    console.log(reg);

    svg.append("svg:line")
        .attr("x1", 10)
        .attr("y1", slope(10))
        .attr("x2", width)
        .attr("y2", slope(width))
        .style("stroke-width", "1.5px")
        .style("stroke", "rgba(0,0,0,0.2)");

    // line charts
    _.forEach(data, function(data2, nn){
        svg.append("path")
            .datum(data2)
            .attr("class", "line")
            .attr("stroke", color(nn))
            .attr("d", line);

        _.forEach(data2, function(d){
            svg.append("svg:circle")
                .attr('cx', xScale(d.index))
                .attr('cy', yScale(d.score))
                .attr('fill', color(nn))
                .attr('r', 2);
        });
    });

    // circle overlay
    var focus = svg.append("circle")
        .attr("class", "focus")
        .attr("r", 6)
        .style("display", "none");

    svg.append("rect")
        .attr("class", "overlay")
        .attr("width", width)
        .attr("height", height)
        .on("mouseover", function() {
            info.style("opacity", 1);
            focus.style("display", null); })
        .on("mouseout", function() {
            focus.style("display", "none"); })
        .on("mousemove", mousemove);

    function mousemove(){
        var x0 = d3.mouse(this)[0];
        if (x0 <= 10) return;

        var i = d3.bisectLeft(xScale.range(), x0);
        var d = flatdata[i - 1];

        focus.attr("transform", "translate(" + xScale(d.index) +
            "," + yScale(d.score) + ")");

        info.select('#info-season').text(d.season);
        info.select('#info-number').text(d.number);
        info.select('#info-date').text(d.date);
        info.select('#info-title').text(d.title);
        info.select('#info-rating').text(d.score + '★');
    }
});

function linearRegression(y, x){
    var lr = {};
    var n = y.length;
    var sum_x = 0;
    var sum_y = 0;
    var sum_xy = 0;
    var sum_xx = 0;
    var sum_yy = 0;

    for (var i = 0; i < y.length; i++) {

        sum_x += x[i];
        sum_y += y[i];
        sum_xy += (x[i]*y[i]);
        sum_xx += (x[i]*x[i]);
        sum_yy += (y[i]*y[i]);
    }

    lr['slope'] = (n * sum_xy - sum_x * sum_y) / (n*sum_xx - sum_x * sum_x);
    lr['intercept'] = (sum_y - lr.slope * sum_x)/n;
    lr['r2'] = Math.pow((n*sum_xy - sum_x*sum_y)/Math.sqrt((n*sum_xx-sum_x*sum_x)*(n*sum_yy-sum_y*sum_y)),2);

    return lr;
}
