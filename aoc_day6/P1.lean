import Init.System.FilePath
import Init.System.IO
import AocDay6

def main : IO Unit := do
  let contents ← IO.FS.readFile "input.txt"
  let s := readState contents
  let reachedStates := s :: getFutureStates s
  let futureLocs := reachedStates.map (fun s => (s.pose.x, s.pose.y))
  let uniqueFutureLocs := futureLocs.foldl (fun acc p => if acc.contains p then acc else p :: acc) []
  IO.print s!"Num unique locs : {uniqueFutureLocs.length}\n"
