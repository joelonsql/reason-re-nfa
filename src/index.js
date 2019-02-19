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
  let dot = document.getElementById(id + "Dot").value;
  if (dot.length > 0) {
    d3.select("#" + id + "Graph").graphviz().zoom(false).renderDot(dot);
  }
  return;
}

function removeDupNewLines(str) {
  return str.replace(/\n(\s*\n)+/g, '\n').replace(/^\n+/, '');
}

let last_regexp_input = '';
let last_text_input = '';
let last_max_length = 0;
let last_max_cardinality = 0;

document.addEventListener('DOMContentLoaded',
  function (event) {
    let tester = function (event) {
      let repeat_input = parseInt(document.getElementById("repeat-input").value);
      let text_input = document.getElementById("text-input").value.repeat(repeat_input);
      if (text_input == last_text_input) {
        // unchanged
        return;
      }
      last_text_input = text_input;
      let regexp_result;
      let js_code = document.getElementById("match_dfa_js").value
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
      let iters = 1000000 / repeat_input;
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
          throw ("Unexpected, match_dfa(text_input) " + match_dfa(text_input) + " text_input " + text_input + " expect_match " + expect_match);
        }
      }
      t = (new Date()).getTime();
      regexp_result += ", Re0 JavaScript " + (t - t0) + " ms";

      document.getElementById("regexp-result").innerHTML = regexp_result;

      let wat = document.getElementById("match_dfa_wasm").value
      let wabt = WabtModule();
      let features = {
        exceptions: false,
        multi_value: false,
        mutable_globals: true,
        sat_float_to_int: false,
        sign_extension: false,
        simd: false,
        tail_call: false,
        threads: false,
      };
      let module = wabt.parseWat('test.wast', wat, features);
      module.resolveNames();
      module.validate(features);
      let binaryOutput = module.toBinary({ log: true, write_debug_names: true });
      // document.getElementById("WebAssembly").innerHTML = module.toText({ foldExprs: false, inlineExport: false });
      //      console.log(binaryOutput.log);
      let binaryBuffer = binaryOutput.buffer;
      let memory = new WebAssembly.Memory({ initial: 10, maximum: 100 });
      let encodedString = new TextEncoder('utf-8').encode(text_input);
      let s = new Uint8Array(memory.buffer);
      s.set(encodedString);
      WebAssembly.instantiate(binaryBuffer, { js: { mem: memory } })
        .then(obj => {
          let match_dfa_wasm = obj.instance.exports.match_dfa;
          let t0 = (new Date()).getTime();
          for (let i = 0; i < iters; i++) {
            if (match_dfa_wasm(text_input) != expect_match) {
              throw ("Unexpected, match_dfa_wasm(text_input) " + match_dfa_wasm(text_input) + " text_input " + text_input + " expect_match " + expect_match);
            }
          }
          let t = (new Date()).getTime();
          let regexp_result = ", Re0 WebAssembly " + (t - t0) + " ms";
          document.getElementById("regexp-result").innerHTML += regexp_result;
        });
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
        document.getElementById("JS").innerHTML = "";
        document.getElementById("WebAssembly").innerHTML = "";
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
        document.getElementById("LLVMIR").innerHTML = removeDupNewLines(re[algoPos]);
        let js_code = js_beautify(re[algoPos + 1], { indent_size: 2, "max-preserve-newlines": 1 });
        let wasm_code = re[algoPos + 2];
        document.getElementById("JS").innerHTML = js_code;
        document.getElementById("WebAssembly").innerHTML = removeDupNewLines(wasm_code);
        document.getElementById("match_dfa_js").value = js_code;
        document.getElementById("match_dfa_wasm").value = wasm_code;
        last_text_input = ''; // force update
        tester();
      } catch (e) {
        console.log(e);
        document.getElementById("regexp-input").style.backgroundColor = "#ffaaaa";
        document.getElementById("match_dfa_js").value = "";
        document.getElementById("match_dfa_wasm").value = "";
      }
    };
    document.getElementById("regexp-input").addEventListener("keyup", visualize);
    document.getElementById("text-input").addEventListener("keyup", tester);
    document.getElementById("repeat-input").addEventListener("keyup", tester);
    document.getElementById("max_length").addEventListener("change", visualize);
    document.getElementById("max_cardinality").addEventListener("change", visualize);
    visualize();
    tester();

    // Show an element
    let show = function (elem) {
      elem.classList.remove('is-hidden');
    };
    // Hide an element
    let hide = function (elem) {
      elem.classList.add('is-hidden');
    };
    // Toggle element visibility
    let toggle = function (elem) {
      elem.classList.toggle('is-hidden');
    };
    // Listen for click events
    document.addEventListener('click', function (event) {
      // Make sure clicked element is our toggle
      if (!event.target.classList.contains('toggle')) return;
      // Prevent default link behavior
      event.preventDefault();
      // Get the content
      var content = document.querySelector(event.target.hash);
      if (!content) return;
      // Toggle the content
      toggle(content);
    }, false);

  });

export { }
