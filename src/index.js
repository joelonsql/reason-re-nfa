import * as ReasonRegex from "./ReasonRegex.bs.js";

document.addEventListener('DOMContentLoaded',
function(event){
    document.getElementById("regexp-input").addEventListener("keyup", function(event) {
        let regexinput = document.getElementById("regexp-input").value;
        if (regexinput.length == 0) {
            document.getElementById("graph").innerHTML = "";
            document.getElementById("parseTree").innerHTML = "";
            return;
        }
        try {
            d3.select("#graph").graphviz().renderDot(ReasonRegex.regexp2dot(regexinput));
            document.getElementById("regexp-input").style.backgroundColor = "";
            let parseTree = ReasonRegex.regexp2parseTree(regexinput);
            function createTreantNodeStructure(parseTree) {
                let node = {
                    text: { name: parseTree[0] }
                };
                if (parseTree[1].constructor === Array) {
                    let children = [];
                    children.push(createTreantNodeStructure(parseTree[1]));
                    if (parseTree[2]) {
                        children.push(createTreantNodeStructure(parseTree[2]));
                    }
                    node.children = children;
                } else {
                    node.children = [
                        {text: { name: parseTree[1] }}
                    ];
                }
                return node;
            }
            let simple_chart_config = {
                chart: {
                    container: "#parseTree",
                    node: {
                        collapsable: true
                    }
                },
                nodeStructure: createTreantNodeStructure(parseTree)
            };
            new Treant(simple_chart_config);
        } catch (e) {
            console.log(e);
            document.getElementById("regexp-input").style.backgroundColor = "#ffaaaa";
        }
    });
});

export {}
