# Adent of Code 2019 in OCaml
My solutions to AOC 2019 in OCaml + an IntCode debugger

## Build
```bash
> make
```

## Run solution
```bash
> aoc solve <puzzle number>
<answer>
```

## Run intcode debugger
```bash
> aoc extra intdebug <source filename>

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
  parse (p):           Parse opcode and print it to stdout
  parse addr (paddr):  Parse opcode at addr and print it to stdout
  print addr (praddr): Print value at addr
  eval (e):            Evaluate opcode given on stdin
  eval addr (eaddr):   Evaluate opcode at addr

  help (h):            Display this message
> (...)
```