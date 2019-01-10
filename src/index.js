import * as ReasonRegex from "./ReasonRegex.bs.js";

function matrix_to_table(matrix) {
  let result = ["<table border='1'>"];
  for (let row of matrix) {
    result.push("<tr>");
    for (let cell of row) {
      result.push(`<td>${cell}</td>`);
    }
    result.push("</tr>");
  }
  result.push("</table>");
  return result.join('\n');
}

let renderGraphIntervalIds = {};

function renderGraph(id) {
  document.getElementById(id + "Graph").innerHTML = "";
  let dot = document.getElementById(id + "Dot").value;
  if (dot.length > 0) {
    d3.select("#" + id + "Graph").graphviz().zoom(false).renderDot(dot);
  }
  return;
}

let last_regexp_input = '';

document.addEventListener('DOMContentLoaded',
  function (event) {
    let visualize = function (event) {
      let regex_input = document.getElementById("regexp-input").value;
      if (regex_input == last_regexp_input) {
        // unchanged
        return;
      }
      last_regexp_input = regex_input;
      let algoSteps = ['nfa', 'dfa', 'nfa2', 'nfa2rev', 'dfa2', 'nfa3', 'nfa3rev', 'dfaMinimized'];
      if (regex_input.length == 0) {
        document.getElementById("annotated").innerHTML = "";
        document.getElementById("nullability").innerHTML = "";
        document.getElementById("firsts").innerHTML = "";
        document.getElementById("lasts").innerHTML = "";
        document.getElementById("letterpairs").innerHTML = "";
        document.getElementById("parseTree").innerHTML = "";
        for (let algoStep of algoSteps) {
          document.getElementById(algoStep + "Matrix").innerHTML = "";
          document.getElementById(algoStep + "Dot").innerHTML = "";
          document.getElementById(algoStep + "Graph").innerHTML = "";
        }
        document.getElementById("LLVMIR").innerHTML = "";
        return;
      }
      try {
        let re = ReasonRegex.analyze(regex_input);
        document.getElementById("regexp-input").style.backgroundColor = "";
        let glushkovPos = 0;
        document.getElementById("nullability").innerHTML = re[glushkovPos] ? "true" : "false";
        document.getElementById("firsts").innerHTML = re[glushkovPos + 1];
        document.getElementById("lasts").innerHTML = re[glushkovPos + 2];
        document.getElementById("letterpairs").innerHTML = re[glushkovPos + 3];
        document.getElementById("annotated").innerHTML = re[glushkovPos + 4];

        let parseTreePos = 5;
        function createTreantNodeStructure(parseTree) {
          let node = {
            text: { name: parseTree[0] }
          };
          if (parseTree[1]) {
            let children = [];
            for (let i = 1; i < parseTree.length; i++) {
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
          nodeStructure: createTreantNodeStructure(re[parseTreePos])
        };
        new Treant(simple_chart_config);

        let algoPos = 6;
        for (let algoStep of algoSteps) {
          document.getElementById(algoStep + "Matrix").innerHTML = "";
          document.getElementById(algoStep + "Dot").innerHTML = "";
          document.getElementById(algoStep + "Graph").innerHTML = "";
          let dot = re[algoPos];
          let matrix = re[algoPos + 1];
          document.getElementById(algoStep + "Dot").value = dot;
          document.getElementById(algoStep + "Matrix").innerHTML = matrix_to_table(matrix);
          if (renderGraphIntervalIds[algoStep]) {
            clearTimeout(renderGraphIntervalIds[algoStep]);
          }
          renderGraphIntervalIds[algoStep] = setTimeout(renderGraph, 50, algoStep);
          algoPos += 2;
        }
        document.getElementById("LLVMIR").innerHTML = re[algoPos];
      } catch (e) {
        console.log(e);
        document.getElementById("regexp-input").style.backgroundColor = "#ffaaaa";
      }
    };
    document.getElementById("regexp-input").addEventListener("keyup", visualize);
    visualize();
  });

export { }
