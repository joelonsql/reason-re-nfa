import * as ReasonRegex from "./ReasonRegex.bs.js";

document.addEventListener('DOMContentLoaded',
function(event){
    document.getElementById("regexp-input").addEventListener("keyup", function(event) {
        let regexinput = document.getElementById("regexp-input").value;
        if (regexinput.length == 0) {
            document.getElementById("graph").innerHTML = "";
            return;
        }
        try {
            d3.select("#graph").graphviz().renderDot(ReasonRegex.regexp2dot(regexinput));
            document.getElementById("regexp-input").style.backgroundColor = "";
        } catch (e) {
            document.getElementById("regexp-input").style.backgroundColor = "#ffaaaa";
        }
    });
});

export {}
