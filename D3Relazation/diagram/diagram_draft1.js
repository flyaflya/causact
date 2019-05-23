var width = 640,
    height = 480,
    constant = 100;

var nodes = [
    {label: 'Yiren', x: constant, y: 150 , width:100,height:70 },
    {label: 'Adam', x: constant + 80, y: 250 ,width:100,height:70 },
    {label: 'hell', x: constant + 160, y: 150 ,width:100,height:70 }
];

var links = [
    { source: 0, target: 1 },
    { source: 2, target: 1}
];

r2d3.onRender(function(graph, svg, options) {

var force = d3.layout.force()
    .size([width, height])
    .nodes(nodes)
    .links(links);
force.linkDistance(width/4);

// here should consider the path later, or could refer to attached html for
// directed marker, the line could be only rendered in r as a foregraph
var link = svg.selectAll('.link')
    .data(links)
    .enter().append('line')
    .attr('class', 'link')
    .style('stroke','red')
    .attr('fill','red')
    .style("stroke-width", "5px")
    .style("opacity", "0.7");

var node = svg.selectAll('#node')
    .data(nodes)
    .enter().append('g')
    .attr('class', 'node')
    .attr("fill","green")
.attr("transform", function(d){
    return "translate("+d.x+","+d.y+")";
});

node.append("rect")
        .attr("class", "nodeRect")
        .attr("rx", 6)
        .attr("ry", 6)
        .attr('width', function(d) { return d.width; })
        .attr('height', function(d) { return d.height; })
        .style("fill", "#2376B2");

node.append("text").style("text-anchor", "middle")
        .style("pointer-events", "none")
        .style("font-weight", 900)
        .attr("fill", "white")
        .style("stroke-width", "0.3px")
        .style("font-size", "16px")
        .attr("y", function (d){return d.height/2+6;})
        .attr("x", function (d){return d.width/2;})
        .text(function (d) {return d.label;});

 force.start();

  function ticked() {
    //constrains the nodes to be within a box
    link.attr('x1', function(d) { return d.source.x + d.source.width/2; })
        .attr('y1', function(d) { return d.source.y + d.source.height/2; })
        .attr('x2', function(d) { return d.target.x + d.target.width/2; })
        .attr('y2', function(d) { return d.target.y + d.target.height/2; });

    node
        .attr("x", function(d) { return d.x; })
        .attr("y", function(d) { return d.y; });
  }
});
