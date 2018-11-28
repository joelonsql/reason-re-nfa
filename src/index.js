import * as ReasonRegex from "./ReasonRegex.bs.js";

function matrix_to_table(matrix) {
  let result = ["<table border='1'>"];
  for(let row of matrix) {
    result.push("<tr>");
    for(let cell of row){
      result.push(`<td>${cell}</td>`);
    }
    result.push("</tr>");
  }
  result.push("</table>");
  return result.join('\n');
}

document.addEventListener('DOMContentLoaded',
function(event){
  document.getElementById("regexp-input").addEventListener("keyup", function(event) {
    let regexinput = document.getElementById("regexp-input").value;
    if (regexinput.length == 0) {
      document.getElementById("graph").innerHTML = "";
      document.getElementById("parseTree").innerHTML = "";
      document.getElementById("annotated").innerHTML = "";
      document.getElementById("nullability").innerHTML = "";
      document.getElementById("firsts").innerHTML = "";
      document.getElementById("lasts").innerHTML = "";
      document.getElementById("letterpairs").innerHTML = "";
      document.getElementById("factor_transitions").innerHTML = "";
      document.getElementById("initial_transitions").innerHTML = "";
      document.getElementById("joint_transitions").innerHTML = "";
      return;
    }
    let re = ReasonRegex.analyze(regexinput);
    try {
      d3.select("#graph").graphviz().renderDot(re[0]);
      document.getElementById("regexp-input").style.backgroundColor = "";
      document.getElementById("nullability").innerHTML = re[2] ? "true" : "false";
      document.getElementById("firsts").innerHTML = re[3];
      document.getElementById("lasts").innerHTML = re[4];
      document.getElementById("letterpairs").innerHTML = re[5];
      document.getElementById("annotated").innerHTML = re[6];
      document.getElementById("factor_transitions").innerHTML = matrix_to_table(re[7]);
      document.getElementById("initial_transitions").innerHTML = matrix_to_table(re[8]);
      document.getElementById("joint_transitions").innerHTML = matrix_to_table(re[9]);
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
