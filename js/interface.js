"use strict";

function show(selector) {
    document.querySelector(selector).setAttribute("open", "");
}

function hide(selector) {
    document.querySelector(selector).removeAttribute("open");
}

function load() {
    const file = document.querySelector("input[type=file]").files[0];
    const reader = new FileReader();
    reader.onload = function() {
        jsbridge.load(reader.result);
        document.querySelector("#loadedProgram").innerHTML = `Program ${file.name} loaded`;
        showCFGSection();
    };
    reader.readAsText(file);
}

function showCFGSection() {
    show("#cfgs");
    hide("#loading");
    for (const id of Object.keys(jsbridge.cfgs())) {
        document.querySelector("#listOfCFGs").innerHTML += `<button onclick="showCFG(${id} )">${id}</button>`;
    }
}

function showCFG(id) {
    d3.select("#graph").graphviz().renderDot(jsbridge.cfgs()[id].toDot());
}
