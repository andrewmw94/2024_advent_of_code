import Init.System.FilePath
import Init.System.IO
import Std.Data.HashSet
import AocDay6

def checkForCycle (grid: Grid) (s : State) : Bool :=
  match h_match: getNextState grid s with
  | .newState s' =>
    have _: s'.unvisitedSet.size < s.unvisitedSet.size := next_state_decrease_unvisited grid s h_match
    checkForCycle grid s'
  | .leftGrid => false
  | .cycle => true
termination_by s.unvisitedSet.size

def wouldAddingObstacleInduceCycle (grid: Grid) (initialState: State) (obsAddState:State) : Option (Int × Int) :=
  let x := obsAddState.pose.x + obsAddState.pose.heading.toStep.fst
  let y := obsAddState.pose.y + obsAddState.pose.heading.toStep.snd

  if x < 0 || x >= ((grid.data.get! 0).size : Int) || y < 0 || y >= (grid.data.size : Int) then
    none
  else if (grid.getCell x y) == some '#' then
    none
  else
    let newGrid := {grid with extraObs := some (x, y)}
    if checkForCycle newGrid initialState then
      some (x, y)
    else
      none

def main : IO Unit := do
  let contents ← IO.FS.readFile "input.txt"
  let (grid, s) := readState contents
  let reachedStates := s :: getFutureStates grid s
  let cycleOpportunities := reachedStates.map (wouldAddingObstacleInduceCycle grid s) |>.filterMap id
  let uniqueCycleOpportunities := cycleOpportunities.foldl (fun acc p => if acc.contains p then acc else p :: acc) []
  let finalCycleOpportunities := uniqueCycleOpportunities.filter (fun (x, y) => (x,y) != (s.pose.x, s.pose.y))
  IO.print s!"Num places for obstacle : {finalCycleOpportunities.length}\n"
