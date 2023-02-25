import scala.collection.mutable

class Dfa[A] (val alphabet: Set[Char]){
  private var states = mutable.Set[A]()
  private val transitions = mutable.Map[A, mutable.Map[Char, A]]()
  private var initialState: A = _
  private var acceptStates = mutable.Set[A]()
  private var sinkState: A = _

  // The following methods are only the methods directly called by the test suite. You can (and should) define more.
  def addState(state: A): Unit = {
    states += state
    transitions += (state -> mutable.Map[Char, A]())
  }

  def addTransition(fromState: A, c: Char, toState: A): Unit = {
    transitions.get(fromState) match {
      case Some(_) =>
      case None => addState(fromState)
    }

    transitions.get(toState) match {
      case Some(_) =>
      case None => addState(toState)
    }

    transitions(fromState).get(c) match {
      case Some(toState1) =>
      case None => transitions(fromState) += (c -> toState)
    }
  }

  def setInitialState(state: A): Unit = {
    initialState = state
  }

  def setAcceptState(state: A): Unit = {
    acceptStates += state
  }

  def map[B](f: A => B): Dfa[B] = {
    val newDfa = new Dfa[B](alphabet)
    newDfa.setInitialState(f(initialState))

    states.foreach { state =>
      val newState = f(state)
      newDfa.addState(newState)

      if (isFinal(state)) {
        newDfa.setAcceptState(newState)
      }
    }
    transitions.foreach {
      case (fromState, cs) =>
        cs.foreach {
          case (c, toState) => newDfa.addTransition(f(fromState), c, f(toState))
      }
    }
    newDfa
  }

  def next(state:A, c: Char): A = {
    transitions.get(state) match {
      case Some(trans) => trans.get(c) match {
        case Some(nextState) => nextState
        case None => sinkState
      }
      case None => sinkState
    }
  }

  def accepts(str: String): Boolean = {
    var state = initialState
    for (c <- str) {
      state = next(state, c)
      if (state == sinkState)
        return false
    }
    isFinal(state)
  }

  def getStates : Set[A] = states.toSet // TODO implement getStates

  def isFinal(state: A): Boolean = acceptStates.contains(state)  // TODO implement isFinal
}

// This is a companion object to the Dfa class. This allows us to call the method fromPrenex without instantiating the Dfa class beforehand.
// You can think of the methods of this object like static methods of the Dfa class
object Dfa {
  def fromPrenex(str: String): Dfa[Int] = {
    val nfa = Nfa.fromPrenex(str)
    val alphabet = nfa.alphabet
    val dfa = new Dfa[Int](alphabet)
    // Initialize the worklist with the initial group of states of the NFA (1 is eps)
    var worklist = List(Set(nfa.initialState) ++ Set(nfa.initialState).flatMap(state => nfa.next(state, 1)))

    // Initialize the mapping from NFA states to DFA states with the initial state
    var nfaToDfa = Map[Set[Int], Int]((Set(nfa.initialState) ++ Set(nfa.initialState).flatMap(state => nfa.next(state, 1))) -> 0)
    var nextState = 1

    while (worklist.nonEmpty) {
      val dfaState = worklist.head
      worklist = worklist.tail

      // For each symbol in the alphabet, compute the next NFA states and add the corresponding DFA state to the worklist
      alphabet.foreach { c =>
        val nextNfaStates = dfaState.flatMap(state => nfa.next(state, c))
        if (nextNfaStates.nonEmpty) {
          val nextDfaState = nfaToDfa.getOrElse(nextNfaStates, {
            val state = nextState
            nextState += 1
            worklist = nextNfaStates :: worklist
            nfaToDfa += (nextNfaStates -> state)
            state
          })
          dfa.addTransition(nfaToDfa(dfaState), c, nextDfaState)
        }
      }
    }

    dfa.initialState = 0

    for (states <- nfaToDfa.keySet) {
      if (states.contains(nfa.acceptState))
        dfa.acceptStates += nfaToDfa(states)
    }

    dfa.addState(nextState)
    dfa.sinkState = nextState
    dfa
  }
}