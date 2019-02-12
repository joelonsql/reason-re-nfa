type targetLanguage =
  | JavaScript
  | LLVMIR;

let match_dfa =
  fun
  | JavaScript => JavaScript.match_dfa
  | LLVMIR => LLVMIR.match_dfa;

let switch_case_state =
  fun
  | JavaScript => JavaScript.switch_case_state
  | LLVMIR => LLVMIR.switch_case_state;

let labeled_block =
  fun
  | JavaScript => JavaScript.labeled_block
  | LLVMIR => LLVMIR.labeled_block;

let break_loop_switch =
  fun
  | JavaScript => JavaScript.break_loop_switch
  | LLVMIR => LLVMIR.break_loop_switch;

let make_switch =
  fun
  | JavaScript => JavaScript.make_switch
  | LLVMIR => LLVMIR.make_switch;

let switch_case_value =
  fun
  | JavaScript => JavaScript.switch_case_value
  | LLVMIR => LLVMIR.switch_case_value;

let switch_case_code =
  fun
  | JavaScript => JavaScript.switch_case_code
  | LLVMIR => LLVMIR.switch_case_code;

let continue =
  fun
  | JavaScript => JavaScript.continue
  | LLVMIR => LLVMIR.continue;

let goto_irreducible_state =
  fun
  | JavaScript => JavaScript.goto_irreducible_state
  | LLVMIR => LLVMIR.goto_irreducible_state;