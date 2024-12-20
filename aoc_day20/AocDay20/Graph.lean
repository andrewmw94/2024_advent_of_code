import Std.Data.HashMap

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


-- Get the cost to each reachable Node from the start Node
def Graph.dijkstra (g: Graph) (start: Node) : Id (Std.HashMap Node Nat) := do
  -- Initialize priority queue with start node
  let mut queue := Queue.make []
  queue := queue.insert ({ state := start, cost := 0 : SearchNode})
  -- Track visited nodes
  let mut visited := Std.HashMap.empty

  while !queue.isEmpty do
    -- Get the node with minimum cost
    let node := queue.min
    queue := queue.deleteMin

    -- Skip if already visited
    if visited.contains node.state then
      continue

    -- Add to visited set
    visited := visited.insert node.state node.cost

    -- Process all outgoing edges
    match g.outEdges? node.state with
    | none => continue
    | some edges =>
      for edge in edges do
        let newCost := node.cost + edge.weight
        if !visited.contains edge.dest then
          queue := queue.insert { state := edge.dest, cost := newCost }

  visited
