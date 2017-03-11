open Core.std

let spec =
  let open Command.Spec in
  empty
  +> anon ("filename" %: string)

let command =
  Command.basic
    ~summary:"Compile monoidal categories into their associated string diagrams"
    ~readme:(fun() -> "See https://github.com/jack-willturner/fyp-repository for more information")
    spec
      (fun filename() -> filename)

let () =
  Command.run ~version:"1.0" ~build_info:"RWO" command
