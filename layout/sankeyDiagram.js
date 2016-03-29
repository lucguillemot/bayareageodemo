function sankeyDiagram(id, clusters) {
  var margin = {top: 1, right: 1, bottom: 6, left: 1},
      width = 350 - margin.left - margin.right,
      height = 250 - margin.top - margin.bottom;

  var svg = d3.select(id).append("svg")
      .attr("width", width + margin.left + margin.right)
      .attr("height", height + margin.top + margin.bottom)
    .append("g")
      .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

  var sankey = d3.sankey()
      .nodeWidth(15)
      .nodePadding(10)
      .size([width, height]);

  var path = sankey.link();

  sankey
      .nodes(clusters.nodes)
      .links(clusters.links)
      .layout(32);

  var link = svg.append("g").selectAll(".link")
      .data(clusters.links)
    .enter().append("path")
      .attr("class", "link")
      .attr("d", path)
      .style("stroke-width", function(d) { return Math.max(1, d.dy); })
      .sort(function(a, b) { return b.dy - a.dy; });

  link.append("title")
      .text(function(d) { return d.source.name + " → " + d.target.name; });

  var node = svg.append("g").selectAll(".node")
      .data(clusters.nodes)
    .enter().append("g")
      .attr("class", "node")
      .attr("transform", function(d) { return "translate(" + d.x + "," + d.y + ")"; })
    .call(d3.behavior.drag()
      .origin(function(d) { return d; })
      .on("dragstart", function() { this.parentNode.appendChild(this); })
      .on("drag", dragmove));

  node.append("rect")
      .attr("class", function(d){
        return d.name;
      })
      .attr("height", function(d) { return d.dy; })
      .attr("width", sankey.nodeWidth())
      //.style("fill", function(d) { return d.color = color(d.name.replace(/ .*/, "")); })
      //.style("stroke", function(d) { return d3.rgb(d.color).darker(2); })
      .style("opacity", 1)
    .append("title")
      .attr("x", -16)
      .text(function(d) { return d.name; });

  node.append("text")
      .attr("x", -6)
      .attr("y", function(d) { return d.dy / 2; })
      .attr("dy", ".35em")
      .attr("text-anchor", "end")
      .attr("transform", null)
      .text(function(d) { return d.name.slice(2); })
    .filter(function(d) { return d.x < width / 2; })
      .attr("x", 6 + sankey.nodeWidth())
      .attr("text-anchor", "start");

  function dragmove(d) {
    d3.select(this).attr("transform", "translate(" + d.x + "," + (d.y = Math.max(0, Math.min(height - d.dy, d3.event.y))) + ")");
    sankey.relayout();
    link.attr("d", path);
  }
}