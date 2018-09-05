$ cat repro.t
First comment

  $ echo toto
  onoes

Second comment (dropped)

  $ echo titi
  woops

Last comment (dropped)
$ cram repro.t
$ cat repro.t.corrected
First comment

  $ echo toto
  toto
  $ echo titi
  titi
