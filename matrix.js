var matrix = [
    [11975, 5871, 8916, 2868],
    [1951, 10048, 2060, 6171],
    [8010, 16145, 8090, 8045],
    [1013, 990, 940, 6907]
];






let m;
d3.json('out.json').then((data) => {
    console.log(data)
    m = data
    td = updateTable(data)

})

export function updateTable(data) {
    const table = d3.select("body").append("table")
    // Join
    var tr = table.selectAll("tr")
        .data(data)

    var row_index = d3.local();
    // enter + update rows
    var row = tr.enter().append("tr")
        .merge(tr)
        .each(function (d, i) {
            row_index.set(this, i);            // Store index in local variable.
        })

    var td = row.selectAll("td")
        .data(d => d)

    var col_index = d3.local();
    //enter + update cells
    td.enter().append("td")
        .each(function (d, i) {
            col_index.set(this, i);            // Store index in local variable.
        })

        .on("click", function (e, d) {
            console.log(data)
            let [i, j] = [row_index.get(this), col_index.get(this)]
            console.log(i, j)
            let value = Number(prompt())

            data[i][j] = value
            updateTable(data)
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

