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
let last_text_input = '';
let last_max_length = 0;
let last_max_cardinality = 0;

document.addEventListener('DOMContentLoaded',
  function (event) {
    let tester = function (event) {
      let text_input = document.getElementById("text-input").value;
      if (text_input == last_text_input) {
        // unchanged
        return;
      }
      last_text_input = text_input;
      let regexp_result;
      let js_code = document.getElementById("match_dfa").value
      if (js_code == "") {
        return;
      }
      let match_dfa = new Function("s", js_code);
      let r = new RegExp("^(" + document.getElementById("regexp-input").value + ")$");
      let expect_match;
      if (match_dfa(text_input)) {
        regexp_result = "MATCH";
        if (!r.test(text_input)) {
          regexp_result += " Unexpected, Re0 matched but NativeJS missed";
        }
        expect_match = true;
      } else {
        regexp_result = "MISS";
        if (r.test(text_input)) {
          regexp_result += " Unexpected, Re0 missed but NativeJS matched";
        }
        expect_match = false;
      }

      let t0 = (new Date()).getTime();
      let iters = 1000000;
      for (let i = 0; i < iters; i++) {
        if (r.test(text_input) != expect_match) {
          throw ("Unexpected, r.test(text_input) " + r.test(text_input) + " r " + r + " text_input " + text_input + " expect_match " + expect_match);
        }
      }
      let t = (new Date()).getTime();
      regexp_result += " " + iters + " ops: NativeJS " + (t - t0) + " ms";

      t0 = (new Date()).getTime();
      for (let i = 0; i < iters; i++) {
        if (match_dfa(text_input) != expect_match) {
          throw ("Unexpected, r.test(text_input) " + r.test(text_input) + " r " + r + " text_input " + text_input + " expect_match " + expect_match);
        }
      }
      t = (new Date()).getTime();
      regexp_result += ", Re0 " + (t - t0) + " ms";

      document.getElementById("regexp-result").innerHTML = regexp_result;
    };

    let visualize = function (event) {
      let regex_input = document.getElementById("regexp-input").value;
      let max_length = document.getElementById("max_length").valueAsNumber;
      let max_cardinality = document.getElementById("max_cardinality").valueAsNumber;
      document.getElementById("max_length_label").innerHTML = max_length + " Max length";
      document.getElementById("max_cardinality_label").innerHTML = max_cardinality + " Max cardinality";
      if (regex_input == last_regexp_input && max_length == last_max_length && max_cardinality == last_max_cardinality) {
        // unchanged
        return;
      }
      last_regexp_input = regex_input;
      last_max_length = max_length;
      last_max_cardinality = max_cardinality;
      let algoSteps = ["nfa", "dfa", "nfa2", "nfa3", "dfa2", "nfa4", "nfa5", "dfa3", "dfa4", "dfa5", "dfa6", "nfa6"];
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
        let re = ReasonRegex.analyze(max_length, max_cardinality, regex_input);
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
        //        document.getElementById("LLVMIR").innerHTML = re[algoPos];
        let js_code = js_beautify(re[algoPos + 1], { indent_size: 2, "max-preserve-newlines": 1 });
        document.getElementById("JS").innerHTML = js_code;
        document.getElementById("match_dfa").value = js_code;
        last_text_input = ''; // force update
        tester();
      } catch (e) {
        console.log(e);
        document.getElementById("regexp-input").style.backgroundColor = "#ffaaaa";
        document.getElementById("match_dfa").value = "";
      }
    };
    document.getElementById("regexp-input").addEventListener("keyup", visualize);
    document.getElementById("text-input").addEventListener("keyup", tester);
    document.getElementById("max_length").addEventListener("change", visualize);
    document.getElementById("max_cardinality").addEventListener("change", visualize);
    visualize();
    tester();
  });

export { }
