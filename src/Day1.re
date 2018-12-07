open Belt;
open Util;

module IntSet = Set.Int;

let solve = () =>
  Util.readLines("input/day1.txt")->List.map(parseInt)->List.reduce(0, (+));

let computeFrequency = (freq, (computed, history)) => {
  let newFreq = computed + freq;
  if (IntSet.has(history, newFreq)) {
    Stop((newFreq, history));
  } else {
    Continue((newFreq, IntSet.add(history, newFreq)));
  };
};

let solve2 = () => {
  let (freq, _) =
    Util.readLines("input/day1.txt")
    ->List.map(parseInt)
    ->Util.toCyclingStream
    ->Util.reduceUntil((0, IntSet.empty), computeFrequency);
  freq;
};

let () = Js.log(solve());
let () = Js.log(solve2());