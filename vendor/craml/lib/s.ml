type line = [
  | `Output  of string
  | `Command of string list
  | `Comment of string
  | `Part    of string
  | `Ellipsis
  | `Non_det of [`Command|`Output]
  | `Exit    of int
]
