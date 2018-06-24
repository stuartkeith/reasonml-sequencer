// Generated by BUCKLESCRIPT VERSION 3.1.5, PLEASE EDIT WITH CARE
'use strict';

var Caml_array = require("bs-platform/lib/js/caml_array.js");

function emptyLane(initialValue) {
  return /* record */[
          /* values */Caml_array.caml_make_vect(16, initialValue),
          /* index */0,
          /* visualIndex */0,
          /* loopAfterIndex */7
        ];
}

function advance(lane) {
  var nextIndex = lane[/* index */1] + 1 | 0;
  var match = nextIndex > lane[/* loopAfterIndex */3];
  return /* record */[
          /* values */lane[/* values */0],
          /* index */match ? 0 : nextIndex,
          /* visualIndex */lane[/* index */1],
          /* loopAfterIndex */lane[/* loopAfterIndex */3]
        ];
}

function reset(lane) {
  return /* record */[
          /* values */lane[/* values */0],
          /* index */0,
          /* visualIndex */0,
          /* loopAfterIndex */lane[/* loopAfterIndex */3]
        ];
}

exports.emptyLane = emptyLane;
exports.advance = advance;
exports.reset = reset;
/* No side effect */
