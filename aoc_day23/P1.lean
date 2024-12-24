import Init.System.FilePath
import Init.System.IO
import Std.Data.HashMap
import AocDay23

def Edge.mk_undirected (src dest : Node) (weight : Nat := 0) : List Edge :=
  [{src := src, dest := dest, weight := weight},
   {src := dest, dest := src, weight := weight}]

def buildGraph (lines : List String) : Id (Graph × (Std.HashMap String Nat)) := do
  let lines := lines.filter (· != "")

  let mut nameToId : Std.HashMap String Nat := Std.HashMap.empty
  let mut idToName : Std.HashMap Nat String := Std.HashMap.empty
  let mut nextId := 0

  -- First pass: build name <-> id mappings
  for line in lines do
    let parts := line.split (· == '-')
    let name1 := parts[0]!
    let name2 := parts[1]!

    if nameToId.contains name1 then
      continue
    else
      nameToId := nameToId.insert name1 nextId
      idToName := idToName.insert nextId name1
      nextId := nextId + 1
    if nameToId.contains name2 then
      continue
    else
      nameToId := nameToId.insert name2 nextId
      idToName := idToName.insert nextId name2
      nextId := nextId + 1
  --
  -- Next, build graph
  let edgeArray := Array.mk (List.replicate nextId [])
  let finalEdges := lines.foldl (fun edges line =>
    let parts := line.split (· == '-')
    let id1 := nameToId.get! parts[0]!
    let id2 := nameToId.get! parts[1]!

    -- Add both directions since graph is undirected
    let edges1 := edges.modify id1 (fun l => {src := ⟨id1⟩, dest := ⟨id2⟩, weight := 0} :: l)
    let edges2 := edges1.modify id2 (fun l => {src := ⟨id2⟩, dest := ⟨id1⟩, weight := 0} :: l)
    edges2
  ) edgeArray

  (⟨finalEdges⟩, nameToId)

def find_t_nodes (nodes : Std.HashMap String Nat) : List Node :=
  nodes.fold (fun acc k v =>
    if k.startsWith "t" then
      {id := v} :: acc
    else acc
  ) []

def check_3_cycle (g : Graph) (t_node : Node) : Bool :=
  (g.outEdges! t_node).any (fun e1 =>
    (g.outEdges! e1.dest).any (fun e2 =>
      e2.dest == t_node
    )
  )

structure Triangle where
  n1 : Node
  n2 : Node
  n3 : Node
deriving Repr, BEq

-- Assumes n1, n2 and n3 are distinct
def Triangle.make (n1 n2 n3 : Node) : Triangle :=
  if n1.id < n2.id then
    if n2.id < n3.id then
      {n1 := n1, n2 := n2, n3 := n3}
    else
      if n1.id < n3.id then
        {n1 := n1, n2 := n3, n3 := n2}
      else
        {n1 := n3, n2 := n1, n3 := n2}
  else if n1.id < n3.id then
    if n2.id < n3.id then
      {n1 := n2, n2 := n1, n3 := n3}
    else
      {n1 := n2, n2 := n1, n3 := n3}
  else
    if n2.id < n3.id then
      {n1 := n2, n2 := n3, n3 := n1}
    else
      {n1 := n3, n2 := n2, n3 := n1}


def getTriangles (g : Graph) (t_node : Node) : Id (List Triangle) := do
  let mut l := []
  let one_hops := (g.outEdges! t_node).map (λ e => e.dest)
  for n in one_hops do
    let two_hops := (g.outEdges! n).map (λ e => e.dest)
    for n2 in two_hops do
      if g.outEdges! n2 |>.any (λ e => e.dest == t_node) then
        l := (Triangle.make t_node n n2) :: l
  l

def solve (input : String) : Nat :=
  let lines := input.split (· == '\n')
  let (graph, nodes) := buildGraph lines
  let t_nodes := find_t_nodes nodes
  let t_nodes_in_cycles := t_nodes.filter (λ t_node => check_3_cycle graph t_node)
  let triangles := (t_nodes_in_cycles.map (getTriangles graph)).flatten
  let unique_triangles := triangles.foldl (fun acc t => if acc.contains t then acc else t :: acc) []
  unique_triangles.length

def main : IO Unit := do
  let contents ← IO.FS.readFile "input.txt"
  let res := solve contents
  IO.println s!"{res}"
