// Generated by BUCKLESCRIPT VERSION 4.0.7, PLEASE EDIT WITH CARE
'use strict';

var Caml_obj = require("bs-platform/lib/js/caml_obj.js");
var Js_primitive = require("bs-platform/lib/js/js_primitive.js");

var l = /* :: */[
  "a",
  /* :: */[
    "b",
    /* :: */[
      "c",
      /* :: */[
        "d",
        /* [] */0
      ]
    ]
  ]
];

function last(_l) {
  while(true) {
    var l = _l;
    if (l) {
      var rest = l[1];
      if (rest) {
        _l = rest;
        continue ;
      } else {
        return Js_primitive.some(l[0]);
      }
    } else {
      return undefined;
    }
  };
}

console.log(Caml_obj.caml_equal(last(l), "d"));

exports.l = l;
exports.last = last;
/*  Not a pure module */
