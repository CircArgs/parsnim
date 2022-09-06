import parsnim
import std/[re, strutils]

type
  NodeKind = enum # the different node types
    nkUnk,
    nkNumber,     # a leaf with a number value
    nkAdd,        # an addition
    nkSub,        # a subtraction

  Node= ref object of RootObj

  Number= ref object of Node
    val: float
  
  Operator= ref object of Node
    kind: NodeKind
    left, right: Node

let add = test_char('+').pad.map(nkAdd)
let sub = test_char('-').pad.map(nkSub)
let number = test_regex(re"[0-9]+", "number").pad.map(nkNumber)

let e = number.then(add).then(number)
echo e.parse("5 + 4")
#@[nkNumber, nkAdd, nkNumber]

var expression: Parser[NodeKind, Operator] 

let op_expr = proc(op: static NodeKind): Parser[NodeKind, Operator] =
  proc expression_parser(state: var State[NodeKind]): Result[Operator] =
    let index = state.stream[state.index..<state.stream.len].find(op)
    if index == -1:
      return failure[Operator](state.index, state.index+1, "")
    let left = expression.parse_partial(state.stream[state.index..<index])
    let right = expression.parse_partial(state.stream[
        index+1..<state.stream.len])
    state.index = right.end_index
    success(state.index, right.end_index, @[Operator(kind: op, left: left.value[0], right: right.value[0])], "")
  Parser[NodeKind, Operator](fn: expression_parser)

let number_expr = test_item(nkNumber, "number").map(Number())
expression = op_expr(nkSub) or op_expr(nkAdd) or number_expr