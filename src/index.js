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
            document.getElementById("letterpairs").innerHTML = "";
            return;
        }
        let re = ReasonRegex.analyze(regexinput);
        try {
            d3.select("#graph").graphviz().renderDot(re[0]);
            document.getElementById("regexp-input").style.backgroundColor = "";
            document.getElementById("nullability").innerHTML = re[2] ?
                "<b>true</b> (regex accepts the empty string)" :
                "<b>false</b> (regex rejects the empty string)";
            document.getElementById("firsts").innerHTML = re[3];
            document.getElementById("lasts").innerHTML = re[4];
            document.getElementById("letterpairs").innerHTML = re[5];
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
                nodeStructure: createTreantNodeStructure(re[1])
            };
            new Treant(simple_chart_config);
        } catch (e) {
            console.log(e);
            document.getElementById("regexp-input").style.backgroundColor = "#ffaaaa";
        }
    });
});

export {}
