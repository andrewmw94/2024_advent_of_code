import Init.System.FilePath
import Init.System.IO
import AocDay6

-- TODO: prove termination
partial def checkForCycle (s : State) : Bool :=
  match getNextState s with
  | .newState s' => checkForCycle s'
  | .leftGrid => false
  | .cycle => true

def wouldAddingObstacleInduceCycle (initialState: State) (obsAddState:State) : Option (Int × Int) :=
  let x := obsAddState.pose.x + obsAddState.pose.heading.toStep.fst
  let y := obsAddState.pose.y + obsAddState.pose.heading.toStep.snd

  if x < 0 || x >= ((obsAddState.grid.data.get! 0).size : Int) || y < 0 || y >= (obsAddState.grid.data.size : Int) then
    none
  else if (obsAddState.grid.getCell x y) == some '#' then
    none
  else
    let newGrid := {obsAddState.grid with extraObs := some (x, y)}
    if checkForCycle {initialState with grid := newGrid} then
      some (x, y)
    else
      none

def main : IO Unit := do
  let contents ← IO.FS.readFile "input.txt"
  let s := readState contents
  let reachedStates := s :: getFutureStates s
  let cycleOpportunities := reachedStates.map (wouldAddingObstacleInduceCycle s) |>.filterMap id
  let uniqueCycleOpportunities := cycleOpportunities.foldl (fun acc p => if acc.contains p then acc else p :: acc) []
  let finalCycleOpportunities := uniqueCycleOpportunities.filter (fun (x, y) => (x,y) != (s.pose.x, s.pose.y))
  IO.print s!"Num places for obstacle : {finalCycleOpportunities.length}\n"
