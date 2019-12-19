type attr = string
type set = Solution | Changed | New | Removed | Up | Down
type rematch =
  | ExactMatch of string
  | Regexp of string
type crit =
  | Count of (set * (string * rematch) option)
  | Sum of (set * attr)
  | Unsatrec of set
  | Aligned of (set * attr * attr)
  | NotUptodate of set
type predicate = Minimize of crit | Maximize of crit
type criteria = predicate list
