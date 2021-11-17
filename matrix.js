var matrix = [
    [11975, 5871, 8916, 2868],
    [1951, 10048, 2060, 6171],
    [8010, 16145, 8090, 8045],
    [1013, 990, 940, 6907]
];




const table = d3.select("body").append("table")

let m;
d3.json('out.json', function (err, data) {
    console.log(data)
    m = data
    td = update(data)

})

function update(data) {

    // Join
    var tr = table.selectAll("tr")
        .data(data)

    // enter + update rows
    var row = tr.enter().append("tr")
        .merge(tr)
    var row_idx = d3.local();
    var td = row.selectAll("td")
        .data(function (d, i) {
            row_idx.set(this, i)
            return d;
        })

    //enter + update cells
    td.enter().append("td")
        .on("click", function (d, i) {
            console.log(row_idx.get(this), i)
            let value = Number(prompt())
            data[row_idx.get(this)][i] = value
            update(data)
        })
        .merge(td)
        .text(function (d) {
            return d;
        })

    json = JSON.stringify(m)
    console.log(json)
    return td

}

function saveDynamicDataToFile() {



    var blob = new Blob([JSON.stringify(m)], { type: "text/json;charset=utf-8" });
    saveAs(blob, "out.json");
}

