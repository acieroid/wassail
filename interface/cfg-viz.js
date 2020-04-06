"use strict";
function init() {
    jsbridge.init(document.getElementById("code").value)
}

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
        console.log(cfg);

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

        // Resize the svg to take full width
        document.getElementById("view").width.baseVal.value = window.innerWidth;

        // Render the graph
        this.render(this.inner, this.g)

        // Center the graph
        let initialScale = 0.75;
        this.svg.call(this.zoom.transform,
                      d3.zoomIdentity.translate((this.svg.attr("width") - this.g.graph().width * initialScale) / 2, 20).scale(initialScale));

        this.svg.attr('height', this.g.graph().height * initialScale + 40);
    }

    update(cfgIdx) {
        let result = jsbridge.result(cfgIdx);
        if (result != undefined) {
            this.g.setNode(returnBlock[cfgIdx], {label: `Return\nResult: ${result}`, style: "fill: #DDAAAA"});
        }
        this.render(this.inner, this.g);
    }
}

let view = new View();

function load() {
    init();
    view.draw(2);
}

function analyze() {
    jsbridge.analyze();
    view.update(2);
}

/*
var returnBlock = {};
function load() {
    console.log("Width is:");
    console.log(document.getElementById("view").width.baseVal.value = window.innerWidth);
    console.log(jsbridge.init(document.getElementById("code").value));

    function label(cfgIdx, blockIdx, block) {
        switch (block.sort) {
        case "normal":
            var str = `(Block ${blockIdx})\n`
            Object.keys(block.instrs).forEach(function (instrIdx, _) {
                const instr = block.instrs[instrIdx];
                str += instr + "\n";
            })
            return { label : str };
        case "block_entry":
            return { label: `Block entry (${blockIdx})`, shape: "diamond" };
        case "block_exit":
            return { label: `Block exit (${blockIdx})`, shape: "diamond" };
        case "loop_entry":
            return { label: `Loop entry (${blockIdx})`, shape: "diamond" };
        case "loop_exit":
            return { label: `Loop exit (${blockIdx})`, shape: "diamond" };
        case "return":
            returnBlock[cfgIdx] = "block" + cfgIdx + "-" + blockIdx;
            return { label : `Return` };
        case "function":
            const instr = block.instrs[0];
            return { label : `${instr}`, shape: "circle" };
        default: return { label: block.sort };
        }
    }
    cfgs = jsbridge.cfgs();
    Object.keys(cfgs).forEach(function (cfgIdx, _) {
        const cfg = cfgs[cfgIdx];
        g.setNode(`CFG-${cfgIdx}`, {label: `Function ${cfgIdx}`, style: "fill: #DDDDDD", clusterLabelPos: "top"});
        Object.keys(cfg.blocks).forEach(function (blockIdx, _) {
            const block = cfg.blocks[blockIdx];
            if (block != undefined) {
                const nodeName = "block" + cfgIdx + "-" + blockIdx;
                g.setNode(nodeName, label(cfgIdx, blockIdx, block));
                g.setParent(nodeName, `CFG-${cfgIdx}`);
            } else {
                console.log(`block ${blockIdx} is undefined`);
            }
        });
        Object.keys(cfg.edges).forEach(function (from, _) {
            if (cfg.edges[from] != undefined) {
                Object.keys(cfg.edges[from]).forEach(function (toIdx, _) {
                    const to = cfg.edges[from][toIdx];
                    const fromName = "block" + cfgIdx + "-" + from;
                    const toName = "block" + cfgIdx + "-" + to;
                    if (cfg.blocks[from] != undefined && cfg.blocks[to] != undefined) {
                        g.setEdge(fromName, toName);
                    } else {
                        console.log("a block is undefined for the edge (" + from + ", " + to + ")");
                    }
                });
            }
        });
    })


    // Run the renderer. This is what draws the final graph.
    render(inner, g);

    // Center the graph
    var initialScale = 0.75;
    svg.call(zoom.transform, d3.zoomIdentity.translate((svg.attr("width") - g.graph().width * initialScale) / 2, 20).scale(initialScale));

    svg.attr('height', g.graph().height * initialScale + 40);
}


function analyze() {
    jsbridge.analyze();
    Object.keys(cfgs).forEach(function (cfgIdx, _) {
        const result = jsbridge.result(cfgIdx);
        if (result != undefined) {
            g.setNode(returnBlock[cfgIdx], {label: `Return\nResult: ${result}`, style: "fill: #DDAAAA"});
            // g.setNode(`CFG-${cfgIdx}`, {label: `Function ${cfgIdx}\nSummary: ${result}`, style: "fill: #DDDDDD", clusterLabelPos: "top"});
        } else {
            console.log("Undefined analysis result for cfg: " + cfgIdx);
        }
    })
    render(inner, g);
}
 */
