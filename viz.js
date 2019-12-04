// Create a new directed graph
var g = new dagreD3.graphlib.Graph({compound: true});

// Set an object for the graph label
g.setGraph({});

// Default to assigning a new object as a label for each new edge.
g.setDefaultEdgeLabel(function() { return {}; });

// Add nodes to the graph. The first argument is the node id. The second is
// metadata about the node. In this case we're going to add labels to each of
// our nodes.
// g.setNode("kspacey",    { label: "Kevin Spacey",  });
// g.setNode("swilliams",  { label: "Saul Williams", });
// g.setNode("bpitt",      { label: "Brad Pitt",     });
// g.setNode("hford",      { label: "Harrison Ford", });
// g.setNode("lwilson",    { label: "Luke Wilson",   });
// g.setNode("kbacon",     { label: "Kevin Bacon",   });
// 
// // Add edges to the graph.
// g.setEdge("kspacey",   "swilliams");
// g.setEdge("swilliams", "kbacon");
// g.setEdge("bpitt",     "kbacon");
// g.setEdge("hford",     "lwilson");
// g.setEdge("lwilson",   "kbacon");
// 
// dagre.layout(g);

var svg = d3.select("svg"),
    inner = svg.select("g");

// Set up zoom support
var zoom = d3.zoom().on("zoom", function() {
      inner.attr("transform", d3.event.transform);
    });
svg.call(zoom);


// Create the renderer
var render = new dagreD3.render();

function load() {
console.log(jsbridge.init(document.getElementById("code").value));
console.log("initialized");
console.log("There are " + jsbridge.cfgs() + " CFGs");

function label(cfgIdx, blockIdx, block) {
    switch (block.sort) {
    case "normal":
        var str = "";
        Object.keys(block.instrs).forEach(function (instrIdx, _) {
            const instr = block.instrs[instrIdx];
            console.log(instr);
            str += instr + "\n";
        })
        return { label : str };
    case "block_entry":
        return { label: `Block entry`, shape: "diamond" };
    case "block_exit":
        return { label: `Block exit`, shape: "diamond" };
    case "loop_entry":
        return { label: `Loop entry`, shape: "diamond" };
    case "loop_exit":
        return { label: `Loop exit`, shape: "diamond" };
    case "return":
        return { label : `Return`, shape: "ellipse" };
    case "function":
        const instr = block.instrs[0];
        return { label : `${instr}`, shape: "circle" };
    default: return { label: block.sort };
    }
}
const cfgs = jsbridge.cfgs();
Object.keys(cfgs).forEach(function (cfgIdx, _) {
    const cfg = cfgs[cfgIdx];
    g.setNode(`CFG-${cfgIdx}`, {label: `Function ${cfgIdx}`, style: "fill: #DDDDDD", clusterLabelPos: "top"});
    Object.keys(cfg.blocks).forEach(function (blockIdx, _) {
        const block = cfg.blocks[blockIdx];
        if (block != undefined) {
            const nodeName = "block" + cfgIdx + "-" + blockIdx;
            g.setNode(nodeName, label(cfgIdx, blockIdx, block));
            g.setParent(nodeName, `CFG-${cfgIdx}`);
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
