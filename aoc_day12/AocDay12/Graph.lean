structure Node where
  id: Nat
deriving Repr, BEq, Ord, Hashable

structure Edge where
  src: Node
  dest: Node
  weight: Nat
deriving Repr, BEq, Hashable

structure Graph where
  edges: Array (List Edge)
deriving Repr, BEq, Hashable

def Graph.numNodes (g : Graph) : Nat :=
  g.edges.size

def Graph.outEdges? (g : Graph) (n : Node) : Option (List Edge) :=
  g.edges.get? n.id

def Graph.outEdges! (g : Graph) (n : Node) : List Edge :=
  g.edges.get! n.id

structure IntermediateStep where
  new_queue: List Node
  new_visit: List Node
deriving Repr, BEq

def dedupQueue (queue: List Node) (visited: List Node): List Node :=
  queue.filter (fun n => !visited.contains n)

theorem remainingEdgesDecreasing (l m n : Nat) : l - (m + n) < l - (m + 1 + n) := by sorry

def dfs_helper (g: Graph) (queue: List Node) (visited: List Node) : IntermediateStep :=
  match queue with
  | head :: tail =>
    if visited.contains head then
      have h: g.edges.size - (tail.length + visited.length) < g.edges.size - (tail.length + 1 + visited.length) := by exact remainingEdgesDecreasing g.edges.size tail.length visited.length
      dfs_helper g tail visited
    else
      let out_edges := (g.outEdges! head).map (fun e => e.dest)
      let fresh_out_edges := out_edges.filter (fun n => !visited.contains n)
      let new_visited := head :: visited
      let new_queue := dedupQueue (fresh_out_edges ++ tail) new_visited
      have h: g.edges.size - (tail.length + visited.length) < g.edges.size - (tail.length + 1 + visited.length) := by exact remainingEdgesDecreasing g.edges.size tail.length visited.length
      dfs_helper g new_queue new_visited
  | [] => {new_queue := [], new_visit := visited}
termination_by g.edges.size - (queue.length + visited.length)

def Graph.dfs (g: Graph) (start_node: Node) : List Node :=
  let queue := [start_node]
  (dfs_helper g queue []).new_visit
