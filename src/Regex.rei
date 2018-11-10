/** Regular expression combinators */;

/** The type of regular expressions */

type t;

/** [empty] rejects every string */

let empty: t;

/** [eps] accepts only the empty string */

let eps: t;

/** [any] accepts any single character */

let any: t;

/** [range l h] accepts any character in the range [l]..[h] */

let range: (char, char) => t;

/** [chr c] accepts exactly the character [c] */

let chr: char => t;

/** [seq x y] accepts strings [rs] where [x] accepts [r] and [y]
    accepts [s] */

let seq: (t, t) => t;

/** [alt x y] accepts any string accepted by either [x] or [y] */

let alt: (t, t) => t;

/** [opt r] accepts any string accepted by [r], and the empty string */

let opt: t => t;

/** [star r] accepts any string consisting of zero or more copies of
    a string accepted by [r] */

let star: t => t;

/** [star r] accepts any string consisting of one or more copies of
    a string accepted by [r] */

let plus: t => t;

/** Parse a regular expression using the following grammar:

      r ::= (r)          (parenthesized regex)
            .            (match any character)
            rr           (sequencing)
            r|r          (alternation)
            r?           (zero or one)
            r*           (zero or more)
            r+           (one or more)
            c            (literal character)

   Raises [Parse_error] on parse error
*/

let parse: string => t;

/** [compile r] translates [r] to an NFA that succeeds on exactly
    those strings matched by [r] */

let compile: t => Nfa.nfa;

/** Raised when [parse] is given an invalid regex */

exception Parse_error(string);
