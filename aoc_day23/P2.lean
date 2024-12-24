import Init.System.FilePath
import Init.System.IO
import Std.Data.HashMap
import AocDay23

def Edge.mk_undirected (src dest : Node) (weight : Nat := 0) : List Edge :=
  [{src := src, dest := dest, weight := weight},
   {src := dest, dest := src, weight := weight}]

def buildGraph (lines : List String) : Id (Graph × (Std.HashMap String Nat) × (Std.HashMap Nat String)) := do
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

  (⟨finalEdges⟩, nameToId, idToName)

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

structure Clique where
  nodes : List Node
deriving Repr, BEq

instance : Inhabited Clique where
  default := {nodes := []}

def Triangle.toClique (t : Triangle) : Clique :=
  {nodes := [t.n1, t.n2, t.n3]}

def Clique.make (l: List Node) : Clique :=
  {nodes := l.mergeSort (λ n1 n2 => n1.id < n2.id)}

def getKNPlusOnes (kNs: List Clique) (baseNode: Node) : Id (List Clique) := do
  let relevantNodes := kNs.foldl (fun acc c => c.nodes ++ acc) []
  let relevantNodes := relevantNodes.foldl (fun acc n => if acc.contains n then acc else n :: acc) []

  let mut res := []
  for kN in kNs do
    for n in relevantNodes do
      if kN.nodes.contains n then -- n is already in the clique
        continue
      let mut missing := false
      for idx in List.range kN.nodes.length do
        if kN.nodes.get! idx == baseNode then -- Don't replace the base node
          continue
        let otherKN := kN.nodes.set idx n
        if kNs.contains (Clique.make otherKN) then
          continue
        else
          missing := true
          break
      if !missing then
        res := (Clique.make (n :: kN.nodes)) :: res
  res.foldl (fun acc c => if acc.contains c then acc else c :: acc) []

#eval getKNPlusOnes [Clique.make [{id := 1}, {id := 2}, {id := 3}], Clique.make [{id := 1}, {id := 2}, {id := 4}], Clique.make [{id := 1}, {id := 3}, {id := 4}]] {id := 1}

def solve (input : String) : IO String := do
  let lines := input.split (· == '\n')
  let (graph, nodeNameToId, nodeIdToName) := buildGraph lines
  -- let t_nodes := find_t_nodes nodeNameToId
  let t_nodes := (nodeIdToName.keys).map (λ id => {id := id}) -- For part 2, we actually consider cliques without t nodes
  let t_nodes_in_cycles := t_nodes.filter (λ t_node => check_3_cycle graph t_node)

  let mut largestClique := Clique.make []
  for t_node in t_nodes_in_cycles do
    let triangles := getTriangles graph t_node
    let relevantNodes := triangles.foldl (fun acc t => t.n1 :: t.n2 :: t.n3 :: acc) []
    let uniqueRelevantNodes := relevantNodes.foldl (fun acc n => if acc.contains n then acc else n :: acc) []

    let k4s := getKNPlusOnes (triangles.map (λ t => t.toClique)) t_node
    let k5s := getKNPlusOnes k4s t_node
    let k6s := getKNPlusOnes k5s t_node
    let k7s := getKNPlusOnes k6s t_node
    let k8s := getKNPlusOnes k7s t_node
    let k9s := getKNPlusOnes k8s t_node
    let k10s := getKNPlusOnes k9s t_node
    let k11s := getKNPlusOnes k10s t_node
    let k12s := getKNPlusOnes k11s t_node
    let k13s := getKNPlusOnes k12s t_node
    if k13s.length > 0 then
      IO.println s!"Num k13s: {k13s.length}"
      IO.println s!"K13s: {repr k13s}"
      largestClique := k13s.head!

  let s := largestClique.nodes.map (λ n => nodeIdToName.get! n.id) |>.mergeSort |>.toString
  pure s

def main : IO Unit := do
  let contents ← IO.FS.readFile "input.txt"
  let foo ← solve contents
  IO.println s!"{foo}"
