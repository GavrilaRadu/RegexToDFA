import scala.collection.mutable

class Nfa[A](val alphabet: Set[Char]) {
  var states = mutable.Set[A]()
  val transitions = mutable.Map[A, mutable.Map[Char, mutable.Set[A]]]()
  var initialState: A = _
  var acceptState: A = _

  // The following methods are only the methods directly called by the test suite. You can (and should) define more.

  def addState(state: A): Unit = {
    states += state
    transitions += (state -> mutable.Map[Char, mutable.Set[A]]())
  }

  def addTransition(fromState: A, c: Char, toState: A): Unit = {
    transitions(fromState).get(c) match {
      case Some(toStates) => toStates += toState
      case None => transitions(fromState) += (c -> mutable.Set(toState))
    }
  }

  def setInitialState(state: A): Unit = {
    initialState = state
  }

  def setAcceptState(state: A): Unit = {
    acceptState = state
  }

  def map[B](f: A => B): Nfa[B] = {
    val newNfa = new Nfa[B](alphabet)
    // Apply f to all states
    states.foreach(state => newNfa.addState(f(state)))
    newNfa.initialState = f(initialState)
    newNfa.acceptState = f(acceptState)
    // Copy the previous nfa, but with f(state), not state
    transitions.foreach {
      case (fromState, cs) =>
        cs.foreach {
          case (c, toStates) =>
            toStates.foreach(toState => newNfa.addTransition(f(fromState), c, f(toState)))
        }
    }
    newNfa
  }

  def next(state: A, c: Char): Set[A] = {
    var nextStates = Set[A]()

    // Get the set of next states reached by the eps transition, starting from the current state
    def getTTransitions(state: A): Set[A] = {
      // Get the transitions for the current state and the eps input symbol
      val cTransitions = transitions.get(state).flatMap(_.get(1)).getOrElse(Set())

      // Recursively get the next states for each of the eps transitions
      cTransitions.flatMap(s => getTTransitions(s) + s).toSet
    }

    def helper(state: A): Unit = {
      // eps transitions before consuming the input
      val epsStates = transitions.get(state).flatMap(_.get(1)).getOrElse(Set())

      // Get the transitions for the current state and input symbol
      val cTransitions = transitions.get(state).flatMap(_.get(c)).getOrElse(Set())

      // If the char we are looking for was not found, we ignore the branch
      if (cTransitions.isEmpty && epsStates.isEmpty)
        return

      if (cTransitions.nonEmpty)
        nextStates = nextStates ++ cTransitions ++ cTransitions.flatMap(s => getTTransitions(s))

      // Ex: eps -> a
      for (s <- epsStates)
        helper(s)
    }

    helper(state)
    nextStates
  }


  def accepts(str: String): Boolean = {
    var currentStates = mutable.Set(initialState)

    // Check for eps transitions if the input is empty
    if (str == "")
      currentStates = currentStates ++ currentStates.flatMap(state => next(state, 1))

    for (c <- str) {
      currentStates = currentStates.flatMap(state => next(state, c))
    }
    println(currentStates)
    currentStates.contains(acceptState)
  }

  def getStates : Set[A] = states.toSet

  def isFinal(state: A): Boolean = acceptState.equals(state)
}

sealed trait AST
case class UNION(left: AST, right: AST) extends AST
case class STAR(node: AST) extends AST
case class CONCAT(left: AST, right: AST) extends AST
case class PLUS(node: AST) extends AST
case class MAYBE(node: AST) extends AST
case class ATOM(c: Char) extends AST
case object Empty extends AST

// This is a companion object to the Nfa class. This allows us to call the method fromPrenex without instantiating the Nfa class beforehand.
// You can think of the methods of this object like static methods of the Nfa class
object Nfa {

  def helperAST(s: mutable.Stack[AST], chr: AST): s.type = {
    if (s.isEmpty) {
      s.push(chr)
      return s
    }

    val tmp = s.pop()
    tmp match {
      case UNION(Empty, _) => s.push(UNION(chr, Empty))
      case UNION(l, Empty) => helperAST(s, UNION(l, chr))
      case STAR(Empty) => helperAST(s, STAR(chr))
      case CONCAT(Empty, _) => s.push(CONCAT(chr, Empty))
      case CONCAT(l, Empty) => helperAST(s, CONCAT(l, chr))
      case PLUS(Empty) => helperAST(s, CONCAT(chr, STAR(chr)))
      case MAYBE(Empty) => helperAST(s, UNION(ATOM(1), chr))
    }
    s
  }

  def toAST(str: String): (AST, Set[Char]) = {
    val aux = str.split(" ").toList
    var s = mutable.Stack[AST]()
    var cnt = 0
    var cIteration = 0
    var alphabet = Set[Char]()

    for (x <- aux) yield {
      cIteration += 1
      x match {
        case "UNION" => s.push(UNION(Empty, Empty))
        case "STAR" => s.push(STAR(Empty))
        case "CONCAT" => s.push(CONCAT(Empty, Empty))
        case "PLUS" => s.push(PLUS(Empty))
        case "MAYBE" => s.push(MAYBE(Empty))
        case _ =>
          var chr: Char = 1
          if (x == "eps")
            chr = 1
          else if (x == "void")
            chr = 0
          else if ((x.length == 3) && (x.head == '\'') && (x.last == '\''))
            chr = x.tail.head
          else
            chr = x.head
          
          if (x == "'") {
            cnt = cnt + 1

            if (cnt == 2) {
              chr = ' '
              cnt = 0
            }
          }

          // If it's not ' ', but it is '\'', we add it
          if ((cnt == 1) && (chr != '\'')) {
            if (!alphabet.contains('\''))
              alphabet += '\''
            s = helperAST(s, ATOM('\''))
            cnt = 0;
          }

          // check if ' is the last char
          if ((cnt == 1) && (aux.length == cIteration)) {
            cnt = 0;
          }

          // Make sure not to add ' ' from the space
          if (cnt != 1) {
            if (!alphabet.contains(chr) && (chr != 1))
              alphabet += chr
            s = helperAST(s, ATOM(chr))
          }
      }
    }

    (s.pop(), alphabet)
  }

  def fromPrenex(str: String): Nfa[Int] = {
    val (ast, alphabet) = toAST(str)

    val nfa = new Nfa[Int](alphabet)
    var nextState = 0

    def fromAST(n: AST): (Int, Int) = {
      n match {
        case UNION(l, r) =>
          val (startL, acceptL) = fromAST(l)
          val (startR, acceptR) = fromAST(r)
          nfa.addState(nextState)
          val start = nextState
          nextState += 1
          nfa.addTransition(start, 1, startL)
          nfa.addTransition(start, 1, startR)
          nfa.addState(nextState)
          val accept = nextState
          nextState += 1
          nfa.addTransition(acceptL, 1, accept)
          nfa.addTransition(acceptR, 1, accept)
          (start, accept)

        case CONCAT(l, r) =>
          val (startL, acceptL) = fromAST(l)
          val (startR, acceptR) = fromAST(r)
          nfa.addTransition(acceptL, 1, startR)
          (startL, acceptR)

        case STAR(n) =>
          val (start, accept) = fromAST(n)
          nfa.addTransition(accept, 1, start)
          nfa.addState(nextState)
          val newStart = nextState
          nextState += 1
          nfa.addTransition(newStart, 1, start)
          nfa.addState(nextState)
          val newAccept = nextState
          nextState += 1
          nfa.addTransition(accept, 1, newAccept)
          nfa.addTransition(newStart, 1, newAccept)
          (newStart, newAccept)

        case ATOM(c) =>
          nfa.addState(nextState)
          val start = nextState
          nextState += 1

          var accept = start

          if (c != 1) {
            nfa.addState(nextState)
            accept = nextState
            nextState += 1
          }

          if ((c != 0) && (c != 1))
            nfa.addTransition(start, c, accept)

          (start, accept)
      }
    }

    // Get initial state and final state
    val (start, accept) = fromAST(ast)
    nfa.setInitialState(start)
    nfa.setAcceptState(accept)
    nfa
  }
}