import Std.Data.HashSet

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



-----
structure SearchNode where
  state : Node
  cost : Nat
deriving BEq

instance : Ord SearchNode where
  compare a b := compare a.cost b.cost

instance : Inhabited SearchNode where
  default := { state := { id := 0}, cost := 0 }

instance : Min SearchNode where
  min a b := if a.cost < b.cost then a else b

structure Queue where
  nodes : Array SearchNode

instance : Inhabited Queue where
  default := { nodes := Array.empty }

def Queue.make (l: List SearchNode): Queue := { nodes := l.toArray }

def Queue.min (q: Queue) : SearchNode :=
  match q.nodes.toList.min? with
  | some n => n
  | none => panic! "empty queue"

def Queue.isEmpty (q: Queue) : Bool :=
  q.nodes.isEmpty

def Queue.deleteMin (q: Queue) : Queue :=
  match q.nodes.toList.min? with
  | some n => { nodes := q.nodes.erase n }
  | none => panic! "empty queue"

def Queue.insert (q: Queue) (n: SearchNode) : Queue :=
  {q with nodes := q.nodes.push n}

def Graph.dijkstra (g: Graph) (start: Node) (endNode: Node) : Option Nat := do
  -- Initialize priority queue with start node
  let mut queue := Queue.make []
  queue := queue.insert ({ state := start, cost := 0 : SearchNode})
  -- Track visited nodes
  let mut visited := Std.HashSet.empty

  while !queue.isEmpty do
    -- Get the node with minimum cost
    let node := queue.min
    queue := queue.deleteMin

    -- Skip if already visited
    if visited.contains node.state then
      continue

    -- Add to visited set
    visited := visited.insert node.state

    -- Check if we reached the end
    if node.state == endNode then
      return node.cost

    -- Process all outgoing edges
    match g.outEdges? node.state with
    | none => continue
    | some edges =>
      for edge in edges do
        let newCost := node.cost + edge.weight
        if !visited.contains edge.dest then
          queue := queue.insert { state := edge.dest, cost := newCost }

  none
