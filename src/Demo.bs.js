// Generated by BUCKLESCRIPT VERSION 4.0.5, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var $$Array = require("bs-platform/lib/js/array.js");
var Readline = require("bs-readline/src/Readline.js");
var Caml_array = require("bs-platform/lib/js/caml_array.js");
var Caml_int32 = require("bs-platform/lib/js/caml_int32.js");
var ArrayLabels = require("bs-platform/lib/js/arrayLabels.js");
var Caml_builtin_exceptions = require("bs-platform/lib/js/caml_builtin_exceptions.js");

var board = $$Array.init(9, (function () {
        return /* Empty */2;
      }));

function spaceToString(s) {
  switch (s) {
    case 0 : 
        return "X";
    case 1 : 
        return "O";
    case 2 : 
        return "_";
    
  }
}

function move(s, c, b) {
  return Caml_array.caml_array_set(b, Caml_int32.imul(c[1], 3) + c[0] | 0, s);
}

function get(c, b) {
  return Caml_array.caml_array_get(b, Caml_int32.imul(c[1], 3) + c[0] | 0);
}

function checkSetForWin(set) {
  var setAsList = ArrayLabels.to_list(set);
  var xWins = List.for_all((function (s) {
          return s === /* X */0;
        }), setAsList);
  var oWins = List.for_all((function (s) {
          return s === /* O */1;
        }), setAsList);
  if (xWins) {
    return /* X */0;
  } else if (oWins) {
    return /* O */1;
  } else {
    return /* Empty */2;
  }
}

function checkBoardForWin(board) {
  var sets_000 = $$Array.sub(board, 0, 3);
  var sets_001 = /* :: */[
    $$Array.sub(board, 3, 3),
    /* :: */[
      $$Array.sub(board, 6, 3),
      /* :: */[
        /* array */[
          get(/* tuple */[
                0,
                0
              ], board),
          get(/* tuple */[
                1,
                0
              ], board),
          get(/* tuple */[
                2,
                0
              ], board)
        ],
        /* :: */[
          /* array */[
            get(/* tuple */[
                  0,
                  1
                ], board),
            get(/* tuple */[
                  1,
                  1
                ], board),
            get(/* tuple */[
                  2,
                  1
                ], board)
          ],
          /* :: */[
            /* array */[
              get(/* tuple */[
                    0,
                    2
                  ], board),
              get(/* tuple */[
                    1,
                    2
                  ], board),
              get(/* tuple */[
                    2,
                    2
                  ], board)
            ],
            /* :: */[
              /* array */[
                get(/* tuple */[
                      0,
                      0
                    ], board),
                get(/* tuple */[
                      1,
                      1
                    ], board),
                get(/* tuple */[
                      2,
                      2
                    ], board)
              ],
              /* :: */[
                /* array */[
                  get(/* tuple */[
                        2,
                        0
                      ], board),
                  get(/* tuple */[
                        1,
                        1
                      ], board),
                  get(/* tuple */[
                        0,
                        2
                      ], board)
                ],
                /* [] */0
              ]
            ]
          ]
        ]
      ]
    ]
  ];
  var sets = /* :: */[
    sets_000,
    sets_001
  ];
  var exit = 0;
  var result;
  try {
    result = List.find((function (x) {
            return x !== /* Empty */2;
          }), List.map(checkSetForWin, sets));
    exit = 1;
  }
  catch (exn){
    if (exn === Caml_builtin_exceptions.not_found) {
      return "-";
    } else {
      throw exn;
    }
  }
  if (exit === 1) {
    return spaceToString(result);
  }
  
}

function printGame(game) {
  console.log("");
  for(var x = 0; x <= 2; ++x){
    var row = $$Array.sub(game[/* board */0], Caml_int32.imul(x, 3), 3);
    console.log($$Array.map(spaceToString, row));
  }
  console.log("");
  console.log(spaceToString(game[/* turn */1]) + "'s Turn");
  return /* () */0;
}

var startState_000 = /* board */$$Array.init(9, (function () {
        return /* Empty */2;
      }));

var startState = /* record */[
  startState_000,
  /* turn : X */0,
  /* winner : Empty */2
];

function play(state) {
  printGame(state);
  if (state[/* winner */2] === /* Empty */2) {
    console.log("enter x coordinate...");
    return Readline.readline((function (x) {
                  console.log(x);
                  return /* () */0;
                }));
  } else {
    console.log(spaceToString(state[/* winner */2]));
    return /* () */0;
  }
}

play(startState);

exports.board = board;
exports.spaceToString = spaceToString;
exports.move = move;
exports.get = get;
exports.checkSetForWin = checkSetForWin;
exports.checkBoardForWin = checkBoardForWin;
exports.printGame = printGame;
exports.startState = startState;
exports.play = play;
/* board Not a pure module */
