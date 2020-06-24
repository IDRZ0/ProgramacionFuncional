type Vertex = Int
type Graph = List[List[Vertex]]

def DFS(g: Graph, start: Vertex): List[Vertex] = {
  def inner(v: Vertex, visited: List[Vertex]): List[Vertex] =
    if (visited.contains(v))
      visited
    else {
      val neighbours = g(v).filterNot((x) => visited.contains(x))
      neighbours.foldLeft(v :: visited)((acc, e) => inner(e, acc))
    }

  inner(start, Nil)
}