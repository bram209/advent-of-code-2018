open Belt;

let readLines = fileName => {
  let lineArray =
    Node.Fs.readFileAsUtf8Sync(fileName)
    |> Js.String.trim
    |> Js.String.split("\n");

  List.fromArray(lineArray);
};

let toCyclingStream = items => {
  let buf = ref([]);
  let rec next = _ => {
    if (buf^ == []) {
      buf := items;
    };
    switch (buf^) {
    | [h, ...t] =>
      buf := t;
      Some(h);
    | [] => None
    };
  };
  Stream.from(next);
};

type t('a, 'b) =
  | Continue('a)
  | Stop('b);

let rec reduceUntil = (stream, acc, f) => {
  let next = Stream.peek(stream);
  let _ = Stream.junk(stream);
  switch (next) {
  | None => acc
  | Some(next) =>
    switch (f(next, acc)) {
    | Stop(result) => result
    | Continue(acc) => reduceUntil(stream, acc, f)
    }
  };
};

/* TODO: find better alternative */
let parseInt: string => int = [%raw {| x => parseInt(x, 10) |}];