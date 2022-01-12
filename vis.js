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

// d3.json('out.json', function (err, data) {
//   data = data

//   update()

// })
const height = 600
const width = 600
const scale = 1.2
const textOffset = 10








var drag = simulation => {

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
update(data)

function update(data) {
  const root = d3.hierarchy(data);
  const links = root.links();
  const nodes = root.descendants();

  var simulation = d3.forceSimulation(nodes)
    .force("link", d3.forceLink(links).id(d => d.id).distance(100).strength(1))
    .force("charge", d3.forceManyBody().strength(-100))
    .force("x", d3.forceX())
    .force("y", d3.forceY());

  var svg = d3.select("body").append("svg")
    .attr("viewBox", [-width / 2, -height / 2, width, height]);

  const link = svg.join("g")
    .attr("stroke", "#999")
    .attr("stroke-opacity", 0.6)
    .selectAll("line")
    .data(links)
    .join("line");

  const node = svg
    .selectAll(".node")
    .data(nodes)

  // node enter
  nodeEnter = node.enter()
    .append('svg:g')
    .attr("class", "node")

  //enter
  const text = nodeEnter.append("text")
    .text(d => d.data.name)
    .attr("class", "name")
    .attr("stroke", "#999");
  const circle = nodeEnter.append("circle")
    .attr("class", "circle")
    .attr("r", 10)
    .attr("stroke-width", 1.5)
    .attr("fill", d => d.children ? "#fff" : "#000") // black leaf and white parent
    .attr("stroke", d => d.children ? "#000" : "#fff")
  // update
  const texts = node.selectAll("text")
  texts.text(d => d.data.name).attr("stroke", "#999");

  // remove
  node.exit().remove();

  nodeEnter.on('click', function (e, d) {
    console.log(d)
    let value = prompt("current value is " + d.data.name + ". Changing to: ");
    d.data.name = value
    console.log(d.data)
    update(d.data)
  })





  nodeEnter.call(drag(simulation));

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


}


