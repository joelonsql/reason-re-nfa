import * as ReasonRegex from "./ReasonRegex.bs.js";

document.addEventListener('DOMContentLoaded',
function(event){
    document.getElementById("regexp-input").addEventListener("keyup", function(event) {
        let regexinput = document.getElementById("regexp-input").value;
        if (regexinput.length == 0) {
            document.getElementById("graph").innerHTML = "";
            document.getElementById("parseTree").innerHTML = "";
            document.getElementById("nullability").innerHTML = "";
            document.getElementById("firsts").innerHTML = "";
            document.getElementById("lasts").innerHTML = "";
            return;
        }
        try {
            d3.select("#graph").graphviz().renderDot(ReasonRegex.regexp2dot(regexinput));
            document.getElementById("regexp-input").style.backgroundColor = "";
            document.getElementById("nullability").innerHTML = ReasonRegex.nullability(regexinput) ?
                "<b>true</b> (regex accepts the empty string)" :
                "<b>false</b> (regex rejects the empty string)";
            document.getElementById("firsts").innerHTML = ReasonRegex.firsts(regexinput);
            document.getElementById("lasts").innerHTML = ReasonRegex.lasts(regexinput);
            let parseTree = ReasonRegex.regexp2parseTree(regexinput);
            function createTreantNodeStructure(parseTree) {
                let node = {
                    text: { name: parseTree[0] }
                };
                if (parseTree[1]) {
                    let children = [];
                    for (let i=1; i<parseTree.length; i++) {
                        children.push(createTreantNodeStructure(parseTree[i]));
                    }
                    node.children = children;
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
