type targetLanguage =
  | JavaScript
  | LLVMIR
  | WebAssembly;

let match_dfa =
  fun
  | JavaScript => JavaScript.match_dfa
  | LLVMIR => LLVMIR.match_dfa
  | WebAssembly => WebAssembly.match_dfa;

let switch_case_state =
  fun
  | JavaScript => JavaScript.switch_case_state
  | LLVMIR => LLVMIR.switch_case_state
  | WebAssembly => WebAssembly.switch_case_state;

let labeled_block =
  fun
  | JavaScript => JavaScript.labeled_block
  | LLVMIR => LLVMIR.labeled_block
  | WebAssembly => WebAssembly.labeled_block;

let break_loop_switch =
  fun
  | JavaScript => JavaScript.break_loop_switch
  | LLVMIR => LLVMIR.break_loop_switch
  | WebAssembly => WebAssembly.break_loop_switch;

let make_switch =
  fun
  | JavaScript => JavaScript.make_switch
  | LLVMIR => LLVMIR.make_switch
  | WebAssembly => WebAssembly.make_switch;

let switch_case_value =
  fun
  | JavaScript => JavaScript.switch_case_value
  | LLVMIR => LLVMIR.switch_case_value
  | WebAssembly => WebAssembly.switch_case_value;

let switch_case_code =
  fun
  | JavaScript => JavaScript.switch_case_code
  | LLVMIR => LLVMIR.switch_case_code
  | WebAssembly => WebAssembly.switch_case_code;

let continue =
  fun
  | JavaScript => JavaScript.continue
  | LLVMIR => LLVMIR.continue
  | WebAssembly => WebAssembly.continue;

let goto_irreducible_state =
  fun
  | JavaScript => JavaScript.goto_irreducible_state
  | LLVMIR => LLVMIR.goto_irreducible_state
  | WebAssembly => WebAssembly.goto_irreducible_state;