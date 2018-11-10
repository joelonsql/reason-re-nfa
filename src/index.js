import * as ReasonRegex from "./ReasonRegex.bs.js";

document.addEventListener('DOMContentLoaded',
function(event){
    document.getElementById("regexp-input").addEventListener("keyup", function(event) {
        let regexinput = document.getElementById("regexp-input").value;
        if (regexinput.length == 0) {
            return;
        }
        d3.select("#graph").graphviz().renderDot(ReasonRegex.regexp2dot(regexinput));
    });
});

export {}
