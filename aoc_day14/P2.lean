import Init.System.FilePath
import Init.System.IO
import AocDay14

def width := 101
def height := 103

def checkTenInARow (robots: List Robot) : Id Bool := do
  for y in [:height] do
    for x in [:width/10] do
      let found := robots.filter (λ r => (r.pos_y.toNat == y && r.pos_x.toNat >= x*10 && r.pos_x.toNat <= (x+1)*10))
      if found.length >= 10 then
        return true
  false

def renderScene (robots : List Robot) : Id String := do
  let mut scene := ""
  for y in [:height] do
    for x in [:width] do
      let found := robots.any (λ r => r.pos_x == x && r.pos_y == y)
      if found then
        scene := scene.push '*'
      else
        scene := scene.push '.'
    scene := scene.push '\n'
  scene

def main : IO Unit := do
  let contents ← IO.FS.readFile "input.txt"
  let robots := readProblem contents
  let num_steps := List.map Int.ofNat (List.range 10000)
  let tens := (num_steps.map (λ steps => (robots.map (λ r => moveRobot r 101 103 steps), steps))).filter (λ p => checkTenInARow p.fst)

  IO.println s!"tens: {tens.map (λ p => p.snd)}"

  let min_idx := 7492
  let min_scene := robots.map (λ r => moveRobot r 101 103 min_idx)

  IO.println s!"min scene: {min_idx}"
  IO.print s!"{renderScene min_scene}"
