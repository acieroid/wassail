"use strict";

function load() {
    const file = document.querySelector("input[type=file]").files[0];
    const reader = new FileReader();
    reader.onload = function() {
        jsbridge.load(reader.result);
        console.log(jsbridge.doSomething())
    };
    reader.readAsText(file);
}
