var color = d3.scaleOrdinal(d3.schemeCategory20);
var radius = 50;
var heights = 100;
var widths = 150;

var simulation = d3.forceSimulation()
    .force("link", d3.forceLink().id(function(d) { return d.id; }).distance(200))
    .force("charge", d3.forceManyBody())
    .force("center", d3.forceCenter(width/2, height/2));

r2d3.onRender(function(graph, svg, width, height, options) {
  var link = svg.append("g")
      .attr("class", "links")
    .selectAll("line")
    .data(graph.links)
    .enter().append("line")
      .attr("stroke-width", "2px")
      .style('stroke','grey')
      .style("opacity", "0.7");

  var node = svg.append("g")
      .attr("class", "nodes")
    .selectAll("rect")
    .data(graph.nodes)
    .enter().append("rect")
        .attr("class", "nodeRect")
        .attr("rx", 15)
        .attr("ry", 15)
        .attr('width', function(d) { return widths; })
        .attr('height', function(d) { return heights; })
        .style("stroke", "green")
        .attr("fill","green")
      .call(d3.drag()
          .on("start", dragstarted)
          .on("drag", dragged)
          .on("end", dragended));


//Add the SVG Text Element to the svgContainer
    var text = svg.selectAll("text")
                       .data(graph.nodes)
                       .enter()
                       .append("text");
    var textLabels = text
         .attr("x", 15)
         .attr("y", 15)
         .text("asdsd")
         .attr("font-family", "sans-serif")
         .attr("font-size", "20px")
         .attr("fill", "red");


//  node.append("text")
//        .style("text-anchor", "middle")
//        .style("pointer-events", "none")
//        .style("font-weight", 900)
//        .attr("fill", "black")
//        .style("stroke-width", "0.3px")
//        .style("font-size", "16px")
//        .attr("y", heights/2+6)
//        .attr("x", widths/2)
//        .text(function(d) { return d.id; });


//  node.append("text")
//      .style("text-anchor", "middle")
//      .style("stroke", "black")
//      .attr("fill", "black")
//      .text(node.id);

  simulation
      .nodes(graph.nodes)
      .on("tick", ticked);

  simulation.force("link")
      .links(graph.links);

  function ticked() {
    //constrains the nodes to be within a box
    node
      .attr("x", function(d) { return d.x = Math.max(widths, Math.min(width - widths, d.x)); })
      .attr("y", function(d) { return d.y = Math.max(heights, Math.min(height - heights, d.y)); });

    link
        .attr("x1", function(d) { return d.source.x + widths / 2; })
        .attr("y1", function(d) { return d.source.y + heights / 2; })
        .attr("x2", function(d) { return d.target.x + widths / 2; })
        .attr("y2", function(d) { return d.target.y + heights / 2; });

    node
        .attr("x", function(d) { return d.x; })
        .attr("y", function(d) { return d.y; });
  }
});

function dragstarted(d) {
  if (!d3.event.active) simulation.alphaTarget(0.3).restart();
  d.fx = d.x;
  d.fy = d.y;
}

function dragged(d) {
  d.fx = d3.event.x;
  d.fy = d3.event.y;
}

function dragended(d) {
  if (!d3.event.active) simulation.alphaTarget(0);
  d.fx = null;
  d.fy = null;
}
