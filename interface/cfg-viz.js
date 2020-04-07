"use strict";

var returnBlock = {}; /* TODO */

class View {
    constructor() {
        // Create a new directed graph
        this.g = new dagreD3.graphlib.Graph({compound: true});

        // Set an object for the graph label
        this.g.setGraph({});

        // Default to assigning a new object as a label for each new edge.
        this.g.setDefaultEdgeLabel(function() { return {}; });

        this.svg = d3.select("svg");
        let inner = this.svg.select("g");
        this.inner = inner;

        // Set up zoom support
        this.zoom = d3.zoom().on("zoom", function() {
            inner.attr("transform", d3.event.transform);
        });
        this.svg.call(this.zoom);


        // Create the renderer
        this.render = new dagreD3.render();
    }
    label(cfgIdx, block) {
        switch (block.sort) {
        case "Normal":
            var str = `(Block ${block.idx})\n`
            block.instrs.forEach(function (instr) {
                str += instr + "\n";
            });
            return { label : str };
        case "BlockEntry":
            return { label: `Block entry (${block.idx})`, shape: "diamond" };
        case "BlockExit":
            return { label: `Block exit (${block.idx})`, shape: "diamond" };
        case "LoopEntry":
            return { label: `Loop entry (${block.idx})`, shape: "diamond" };
        case "LoopExit":
            return { label: `Loop exit (${block.idx})`, shape: "diamond" };
        case "Return":
            returnBlock[cfgIdx] = `block${cfgIdx}-${block.idx}`;
            return { label : `Return` };
        case "Function":
            const instr = block.instrs[0];
            return { label : `${instr}`, shape: "circle" };
        default: return { label: block.sort };
        }
    }
    draw(cfgIdx) {
        let that = this;
        let cfg = jsbridge.getCfg(cfgIdx);

        // Create the enclosing function node
        that.g.setNode(`CFG-${cfgIdx}`, {label: `Function ${cfgIdx}`, style: "fill: #DDDDDD", clusterLabelPos: "top"});

        // Loops over the basic blocks
        cfg.blocks.forEach(function (block) {
            if (block != undefined) {
                let blockIdx = block.idx;
                const nodeName = `block${cfgIdx}-${blockIdx}`
                that.g.setNode(nodeName, that.label(cfgIdx, block));
                that.g.setParent(nodeName, `CFG-${cfgIdx}`);
            }
        });

        // Loops over the edges
        cfg.edges.forEach(function (edge, from) {
            if (edge != undefined) {
                edge.forEach(function (to) {
                    const fromName = `block${cfgIdx}-${from}`
                    const toName = `block${cfgIdx}-${to}`
                    that.g.setEdge(fromName, toName);
                });
            }
        });

        // Adds the result if they are present
        let result = jsbridge.result(cfgIdx);
        if (result != undefined) {
            this.g.setNode(returnBlock[cfgIdx], {label: `Return\nResult: ${result}`, style: "fill: #DDAAAA"});
        }

        // Resize the svg to take full width
        document.getElementById("view").width.baseVal.value = window.innerWidth;
        document.getElementById("view").height.baseVal.value = window.innerHeight;

        // Render the graph
        this.render(this.inner, this.g)

        // Center the graph
        let initialScale = 0.75;
        this.svg.call(this.zoom.transform,
                      d3.zoomIdentity.translate((this.svg.attr("width") - this.g.graph().width * initialScale) / 2, 20).scale(initialScale));

        this.svg.attr('height', this.g.graph().height * initialScale + 40);
    }
}

let view = new View();

function load() {
    // Loads the current code
    jsbridge.addLogger(function (opt, msg) {
        console.log(`[${opt}]: ${msg}`);
    });
    jsbridge.init(document.getElementById("code").value)

    // Add all CFGs to the select input
    let sel = document.getElementById("cfgIdx");
    jsbridge.cfgIndices().forEach(function (idx) {
        var opt = document.createElement("option");
        opt.appendChild(document.createTextNode(`CFG ${idx}`));
        opt.value = `${idx}`;
        sel.appendChild(opt);
    });
}

function draw() {
    let cfgIdx = document.getElementById("cfgIdx").value;
    view.draw(cfgIdx);
}

function analyze() {
    jsbridge.analyze();
    let cfgIdx = document.getElementById("cfgIdx").value;
    view.draw(cfgIdx);
}
