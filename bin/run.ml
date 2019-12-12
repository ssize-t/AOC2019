open Core
open Aoc

let solve num =
    match num with
    | 1 -> P1.solve ()
    | 2 -> P2.solve ()
    | 3 -> P3.solve ()
    | 4 -> P4.solve ()
    | 5 -> P5.solve ()
    | 6 -> P6.solve ()
    | 7 -> P7.solve ()
    | 8 -> P8.solve ()
    | 9 -> P9.solve ()
    | 10 -> P10.solve ()
    | 11 -> P11.solve ()
    | 12 -> P12.solve ()
    | _ -> printf "Not solved yet, sorry :-)\n"

let intdebug filename =
    let prog = In_channel.read_all filename in
    let tape = P9.make_tape prog in
    printf "
IntDebug

=== commands ===
  run (r):             Run program
  step (s):            Step forwards
  print tape (pt):     Print current content of tape (within 10 of ic)
  print ic (pic):      Print current ic
  print rb (prb):      Print current rb
  print opcode (po):   Print current opcode
  print ics (pics):    Print 10 most recent ic values
  print rbs (prbs):    Print 10 most recent rb values
  backtrack (bt):      Step backwards
  parse (p):           Parse opcode and print itvto stdout
  parse addr (paddr):  Parse opcode at addr and printvit to stdout
  print addr (praddr): Print value at addr
  eval (e):            Evaluate opcode given on stdin
  eval addr (eaddr):   Evaluate opcode at addr

  help (h):            Display this message
";
    P9.debug tape 0 0 None

let intdebug_command =
    Command.basic
        ~summary:"Debugger for IntCode"
        Command.Let_syntax.(
            let%map_open filename = anon ("filename" %: string) in
            fun () -> intdebug filename
        )

let extra_command =
    Command.group
        ~summary:"Extras for Advent of Code 2019"
        [ "intdebug", intdebug_command ]

let solve_command =
    Command.basic
        ~summary:"Solve an Advent of Code 2019 puzzle"
        Command.Let_syntax.(
            let%map_open puzzle_number = anon ("puzzle number" %: int) in
            fun () -> solve puzzle_number
        )

let () =
    Command.group
        ~summary:"AOC 2019"
        [ "solve", solve_command;
          "extra", extra_command ]
    |> Command.run