const data = {
  "name": "A",
  "children": [
    {
      "name": "B",
      "children": []
    },
    {
      "name": "C",
      "children": []
    }
  ]
}

d3.json('out.json', function (err, data) {
  const height = 600
  const width = 600
  const scale = 1
  const textOffset = 10
  const root = d3.hierarchy(data);
  const links = root.links();
  const nodes = root.descendants();

  const simulation = d3.forceSimulation(nodes)
    .force("link", d3.forceLink(links).id(d => d.id).distance(100).strength(1))
    .force("charge", d3.forceManyBody().strength(-50))
    .force("x", d3.forceX())
    .force("y", d3.forceY());

  const drag = simulation => {

    function dragstarted(event, d) {
      if (!event.active) simulation.alphaTarget(0.3).restart();
      d.fx = d.x;
      d.fy = d.y;
    }

    function dragged(event, d) {
      d.fx = event.x;
      d.fy = event.y;
    }

    function dragended(event, d) {
      if (!event.active) simulation.alphaTarget(0);
      d.fx = null;
      d.fy = null;
    }

    return d3.drag()
      .on("start", dragstarted)
      .on("drag", dragged)
      .on("end", dragended);
  }
  const svg = d3.select("body").append("svg")
    .attr("viewBox", [-width / 2, -height / 2, width, height]);

  const link = svg.append("g")
    .attr("stroke", "#999")
    .attr("stroke-opacity", 0.6)
    .selectAll("line")
    .data(links)
    .join("line");

  const node = svg.append("g")
    .selectAll(".node")
    .data(nodes)
    .join("g")
    .attr("class", "node")


  const circle = node.append("circle")
    .attr("class", "circle")
    .attr("r", 10)
    .attr("stroke-width", 1.5)
    .attr("fill", d => d.children ? "#fff" : "#000") // black leaf and white parent
    .attr("stroke", d => d.children ? "#000" : "#fff")
    .on('click', function (e, d) {
      console.log(d)
      let value = prompt("current value is " + d.data.name + ". Changing to: ");
      d.data.name = value
    })
    .call(drag(simulation));



  const text = node.append("text")
    .text(d => d.data.name)
    .attr("stroke", "#999");

  node.call(drag(simulation));

  simulation.on("tick", () => {
    link
      .attr("x1", d => d.source.x * scale)
      .attr("y1", d => d.source.y * scale)
      .attr("x2", d => d.target.x * scale)
      .attr("y2", d => d.target.y * scale);
    text
      .attr("x", d => d.x * scale + textOffset)
      .attr("y", d => d.y * scale);

    circle
      .attr("cx", d => d.x * scale)
      .attr("cy", d => d.y * scale);
  });
})





function update(data) {

  var u = svg.selectAll("rect")
    .data(data)

  u
    .join("rect")
    .transition()
    .duration(1000)
    .attr("x", d => x(d.group))
    .attr("y", d => y(d.value))
    .attr("width", x.bandwidth())
    .attr("height", d => height - y(d.value))
    .attr("fill", "#69b3a2")
}