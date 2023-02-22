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
        hide("#loading");
        fillCFGSection();
    };
    reader.readAsText(file);
}

function fillCFGSection() {
    for (const id of Object.keys(jsbridge.cfgs())) {
        document.querySelector("#listOfCFGs").innerHTML += `<a onclick="showCFG(${id})">${id}</a>`;
    }
}

function showCFG(id) {
    d3.select("#d3-cfg").graphviz().renderDot(jsbridge.cfgs()[id].toDot());
}

function showCallGraph() {
    d3.select("#d3-callgraph").graphviz().renderDot(jsbridge.callgraph().toDot());
}
