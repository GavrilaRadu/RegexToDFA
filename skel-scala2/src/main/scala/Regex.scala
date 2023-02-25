import scala.collection.immutable.List
import scala.collection.mutable
import scala.util.Either

object Regex {
  /*
    This function should:
    -> Classify input as either character(or string) or operator
    -> Convert special inputs like [0-9] to their correct form
    -> Convert escaped characters
  */

  def preprocess(s: List[Char]): List[Either[Char, Char]] = {
    var list: List[Either[Char, Char]] = Nil
    val allLetters = "abcdfghijklmnopqrstuvwxyz" // except e
    var i = 0

    // Add concat if necessary
    def addLeftWithConcat(c: Char, list: List[Either[Char, Char]]): List[Either[Char, Char]] = {
      if (list.isEmpty)
        Left(c) :: list
      else list.head match {
        case Left(chr) => Left(c) :: Right('.') :: list
        case Right(chr) =>
          if ((chr == ')') || (chr == '*'))
            Left(c) :: Right('.') :: list
          else
            Left(c) :: list
      }
    }

    // Add concat if necessary
    def addRightWithConcat(c: Char, list: List[Either[Char, Char]]): List[Either[Char, Char]] = {
      if (list.isEmpty)
        Right(c) :: list
      else list.head match {
        case Left(chr) => Right(c) :: Right('.') :: list
        case Right(chr) =>
          if ((chr == ')') || (chr == '*'))
            Right(c) :: Right('.') :: list
          else
            Right(c) :: list
      }
    }

    // Add union if necessary
    def addLeftWithUnion(c: Char, list: List[Either[Char, Char]]): List[Either[Char, Char]] = {
      if (list.isEmpty)
        Left(c) :: list
      else list.head match {
        case Left(chr) => Left(c) :: Right('|') :: list
        case Right(chr) =>
          if (c == ')')
            Left(c) :: Right('|') :: list
          else
            Left(c) :: list
      }
    }

    // Get the last expression Ex: e, (a.b.c.d), (a.(b.c.d))
    def getLastExpr(list: List[Either[Char, Char]]): List[Either[Char, Char]] = {
      list.head match {
        case Left(c) => List(Left(c))
        case Right(c) =>
          // Gets here only if the last char is ')'
          var cnt = 1
          var resList: List[Either[Char, Char]] = List(Right(')'))
          var helperList: List[Either[Char, Char]] = list
          while (cnt != 0) {
            helperList = helperList.tail
            helperList.head match {
              case Left(c) => resList = Left(c) :: resList
              case Right(c) =>
                if (c == '(') {
                  cnt -= 1
                  resList = Right(c) :: resList
                }
                else if (c == ')') {
                  cnt += 1
                  resList = Right(c) :: resList
                }
                else
                  resList = Right(c) :: resList
            }
          }
          resList.reverse
      }
    }

    while (i < s.length) {
      // Add them one by one (A LOT of cases)
      if (allLetters.contains(s(i)))
        list = addLeftWithConcat(s(i), list)
      else if ((allLetters.toUpperCase() ++ "E").contains(s(i)))
        list = addLeftWithConcat(s(i), list)
      else if (s(i).isDigit)
        list = addLeftWithConcat(s(i), list)
      else if (s(i) == 'e') {
        if (i + 1 < s.length)
          if (s(i + 1) == 'p')
            if (i + 2 < s.length)
              if (s(i + 2) == 's') {
                list = addLeftWithConcat(1: Char, list)
                i += 2
              } else {
                list = addLeftWithConcat('e', list)
                i += 1
                list = addLeftWithConcat('p', list)
              }
            else {
              list = addLeftWithConcat('e', list)
              i += 1
              list = addLeftWithConcat('p', list)
            }
          else
            list = addLeftWithConcat('e', list)
        else
          list = addLeftWithConcat('e', list)
      }
      else if (s(i) == '(')
        list = addRightWithConcat(s(i), list)
      else if (")|*".contains(s(i)))
        list = Right(s(i)) :: list
      else if (s(i) == '+') {
        val auxList = getLastExpr(list)
        list = Right('*') :: (auxList ::: (Right('.') :: list))
      }
      else if (s(i) == '?') {
        val auxList = getLastExpr(list)
        list = list.drop(auxList.length)
        list = addRightWithConcat('(', list)
        list = auxList ::: list
        list = Right('|') :: list
        list = Left(1: Char) :: list
        list = Right(')') :: list
      }
      else if (s(i) == '[') {
        var fst = s(i + 1)
        val last = s(i + 3)
        list = addRightWithConcat('(', list)
        while (fst <= last) {
          list = addLeftWithUnion(fst, list)
          fst = (fst + 1).toChar
        }
        list = Right(')') :: list
        i += 4
      }
      else if (s(i) == '\'') {
        list = addLeftWithConcat(s(i + 1), list)
        i += 2
      }
      i += 1
    }
    list.reverse
  }

  // Get the output of preprocess and make it postfix using shunting yard alg
  def toPostfix(list: List[Either[Char, Char]]): List[Either[Char, Char]] = {
    var resList: List[Either[Char, Char]] = Nil
    var op: List[Char] = Nil

    def cmpPrecedence(c: Char, resList: List[Either[Char, Char]], op: List[Char]): (List[Either[Char, Char]], List[Char]) = {
      var auxRes = resList
      var auxOp = op

      c match {
        case '*' =>
          if (auxOp.head == '*') {
            auxRes = Right(auxOp.head) :: auxRes
            auxOp = auxOp.tail
            auxOp = c :: auxOp
          }
          else
            auxOp = c :: auxOp
        case '.' =>
          if ((auxOp.head == '*') || (auxOp.head == '.')) {
            auxRes = Right(auxOp.head) :: auxRes
            auxOp = auxOp.tail
            auxOp = c :: auxOp
          }
          else
            auxOp = c :: auxOp
        case '|' =>
          if ((auxOp.head == '*') || (auxOp.head == '.') || (auxOp.head == '|')) {
            auxRes = Right(auxOp.head) :: auxRes
            auxOp = auxOp.tail
            auxOp = c :: auxOp
          }
          else
            auxOp = c :: auxOp
      }
      (auxRes, auxOp)
    }

    for (el <- list) {
      el match {
        case Left(chr) => resList = Left(chr) :: resList
        case Right(chr) =>
          if (chr == '*')
            resList = Right(chr) :: resList
          else if (op.isEmpty)
            op = chr :: op
          else if (chr == '(')
            op = chr :: op
          else if (chr == ')') {
            while (op.head != '(') {
              resList = Right(op.head) :: resList
              op = op.tail
            }
            op = op.tail
          }
          else {
            val (tmpResList, tmpOp) = cmpPrecedence(chr, resList, op)
            resList = tmpResList
            op = tmpOp
          }
      }
    }
    while (op.nonEmpty) {
      resList = Right(op.head) :: resList
      op = op.tail
    }
    resList.reverse
  }

  // This function should construct a prenex expression out of a normal one.
  // Takes the output of postfix and makes it prefix
  def toPrenex(str: String): String = {
    val list = toPostfix(preprocess(str.toList))
    val stack = new mutable.Stack[String]

    for (el <- list) {
      el match {
        case Left(chr) => stack.push(chr.toString)
        case Right(chr) =>
          var op = ""
          chr match {
            case '*' => op = "STAR"
            case '.' => op = "CONCAT"
            case '|' => op = "UNION"
          }

          var s2 = stack.pop()
          if (s2 == (1: Char).toString)
            s2 = "eps"
          if (s2 == " ")
            s2 = "' '"

          if (op != "STAR") {
            var s1 = stack.pop()
            if (s1 == (1: Char).toString)
              s1 = "eps"
            if (s1 == " ")
              s1 = "' '"
            stack.push(op ++ " " ++ s1 ++ " " ++ s2)
          }
          else
            stack.push(op ++ " " ++ s2)
      }
    }
    stack.pop
  }
}