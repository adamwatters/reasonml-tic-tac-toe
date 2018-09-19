type space =
  | X
  | O
  | Empty;

type game = {
  board: array(space),
  turn: space,
  winner: space,
};

type coord = (int, int);

let board = Array.init(9, _i => Empty);

let spaceToString = s =>
  switch (s) {
  | X => "X"
  | O => "O"
  | Empty => "_"
  };

let move = (s: space, c: coord, b) => {
  let (x, y) = c;
  b[y * 3 + x] = s;
};

let get = (c: coord, b) => {
  let (x, y) = c;
  b[y * 3 + x];
};

let checkSetForWin = set => {
  let setAsList = ArrayLabels.to_list(set);
  let xWins = List.for_all(s => s == X, setAsList);
  let oWins = List.for_all(s => s == O, setAsList);
  if (xWins) {
    X;
  } else if (oWins) {
    O;
  } else {
    Empty;
  };
};

let checkBoardForWin = board => {
  let sets = [
    Array.sub(board, 0, 3),
    Array.sub(board, 3, 3),
    Array.sub(board, 6, 3),
    [|get((0, 0), board), get((1, 0), board), get((2, 0), board)|],
    [|get((0, 1), board), get((1, 1), board), get((2, 1), board)|],
    [|get((0, 2), board), get((1, 2), board), get((2, 2), board)|],
    [|get((0, 0), board), get((1, 1), board), get((2, 2), board)|],
    [|get((2, 0), board), get((1, 1), board), get((0, 2), board)|],
  ];

  switch (List.find(x => x != Empty, List.map(checkSetForWin, sets))) {
  | exception Not_found => "-"
  | result => spaceToString(result)
  };
};

let printGame = game => {
  Js.log("");
  for (x in 0 to 2) {
    let row = Array.sub(game.board, x * 3, 3);
    Js.log(Array.map(spaceToString, row));
  };
  Js.log("");
  Js.log(spaceToString(game.turn) ++ "'s Turn");
};

let startState: game = {
  board: Array.init(9, _i => Empty),
  turn: X,
  winner: Empty,
};

let play = state => {
  printGame(state);
  if (state.winner == Empty) {
    Js.log("enter x coordinate...");
    Readline.readline(x => Js.log(x));
  } else {
    Js.log(spaceToString(state.winner));
  };
};

play(startState);