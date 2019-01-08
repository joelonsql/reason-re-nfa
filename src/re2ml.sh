#!/bin/sh
refmt --parse=re --print=ml AnnotatedRegex.re > ~/src/re0/AnnotatedRegex.ml
refmt --parse=re --print=ml Brzozowski.re > ~/src/re0/Brzozowski.ml
refmt --parse=re --print=ml CharMap.re > ~/src/re0/CharMap.ml
refmt --parse=re --print=ml CharSet.re > ~/src/re0/CharSet.ml
refmt --parse=re --print=ml CharSetMap.re > ~/src/re0/CharSetMap.ml
refmt --parse=re --print=ml CharSetSet.re > ~/src/re0/CharSetSet.ml
refmt --parse=re --print=ml Common.re > ~/src/re0/Common.ml
refmt --parse=re --print=ml Dfa.re > ~/src/re0/Dfa.ml
refmt --parse=re --print=ml Glushkov.re > ~/src/re0/Glushkov.ml
refmt --parse=re --print=ml Letter.re > ~/src/re0/Letter.ml
refmt --parse=re --print=ml Letter2Set.re > ~/src/re0/Letter2Set.ml
refmt --parse=re --print=ml LetterSet.re > ~/src/re0/LetterSet.ml
refmt --parse=re --print=ml Nfa.re > ~/src/re0/Nfa.ml
refmt --parse=re --print=ml RabinScott.re > ~/src/re0/RabinScott.ml
refmt --parse=re --print=ml Re0.re > ~/src/re0/Re0.ml
refmt --parse=re --print=ml Regex.re > ~/src/re0/Regex.ml
refmt --parse=re --print=ml RegexParser.re > ~/src/re0/RegexParser.ml
refmt --parse=re --print=ml StateMap.re > ~/src/re0/StateMap.ml
refmt --parse=re --print=ml StateSet.re > ~/src/re0/StateSet.ml
refmt --parse=re --print=ml StateSetMap.re > ~/src/re0/StateSetMap.ml
refmt --parse=re --print=ml StateSetSet.re > ~/src/re0/StateSetSet.ml
refmt --parse=re --print=ml Test.re > ~/src/re0/Test.ml

ocamlformat -i ~/src/re0/AnnotatedRegex.ml
ocamlformat -i ~/src/re0/Brzozowski.ml
ocamlformat -i ~/src/re0/CharMap.ml
ocamlformat -i ~/src/re0/CharSet.ml
ocamlformat -i ~/src/re0/CharSetMap.ml
ocamlformat -i ~/src/re0/CharSetSet.ml
ocamlformat -i ~/src/re0/Common.ml
ocamlformat -i ~/src/re0/Dfa.ml
ocamlformat -i ~/src/re0/Glushkov.ml
ocamlformat -i ~/src/re0/Letter.ml
ocamlformat -i ~/src/re0/Letter2Set.ml
ocamlformat -i ~/src/re0/LetterSet.ml
ocamlformat -i ~/src/re0/Nfa.ml
ocamlformat -i ~/src/re0/RabinScott.ml
ocamlformat -i ~/src/re0/Re0.ml
ocamlformat -i ~/src/re0/Regex.ml
ocamlformat -i ~/src/re0/RegexParser.ml
ocamlformat -i ~/src/re0/StateMap.ml
ocamlformat -i ~/src/re0/StateSet.ml
ocamlformat -i ~/src/re0/StateSetMap.ml
ocamlformat -i ~/src/re0/StateSetSet.ml
ocamlformat -i ~/src/re0/Test.ml
