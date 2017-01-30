import java.util.Optional

import streams.Bloxorz.InfiniteLevel
import streams.{Solver, StringParserTerrain}

class A extends StringParserTerrain {
  override val level: String = ""
}

val level = Vector(Vector('-', 'T'), Vector('o', 'o'), Vector('o', 'o'))
val a = new A
val f = a.terrainFunction(level)
f(a.Pos(1,1))
f(a.Pos(1,0))
for (r <- 0 until 3; c <- 0 until 2) yield f(a.Pos(r, c))
a.findChar('o', level)

/*
def from(initial: Stream[(Block, List[Move])],
         explored: Set[Block]): Stream[(Block, List[Move])] = initial match {
         */

""

val paths = InfiniteLevel.pathsFromStart
paths.take(1).head
paths.drop(1).head
