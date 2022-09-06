import parsnim
import std/[re, strutils, strformat]
import sugar
type
  NodeKind = enum # the different node types
    nkUnk,
    nkNumber,     # a leaf with a number value
    nkAdd,        # an addition
    nkSub,        # a subtraction

  Node = ref NodeObj

  NodeObj = object
    case kind: NodeKind # the `kind` field is the discriminator
    of nkUnk: discard
    of nkNumber: val: float
    of nkAdd, nkSub:
      leftOp, rightOp: Node

proc `$`(node: Node): string =
  case node.kind:
  of nkAdd, nkSub:
    fmt"{node.kind}({node.leftOp}, {node.rightOp})"
  else:
    $node.kind

let add = test_char('+').pad.map(nkAdd)
let sub = test_char('-').pad.map(nkSub)
let number = test_regex(re"[0-9]+", "number").pad.map(nkNumber)

let e = (number or sub or add).many
echo e
# echo e.parse("5+ 4")
#@[nkNumber, nkAdd, nkNumber]

var expression = test_item(nkUnk, "").map(Node(kind: nkUnk))

let op_expr = proc(op: NodeKind): Parser[NodeKind, Node] =
  proc expression_parser(state: var State[NodeKind]): Result[Node] =
    let index = state.stream[state.index..<state.stream.len].find(op)
    # echo index
    if index == -1:
      return failure[Node](state.index, state.index+1, "")
    let left = expression.parse_partial(state.stream[state.index..<index])
    let right = expression.parse_partial(state.stream[
        index+1..<state.stream.len])
    state.index = right.end_index + index+1
    case op:
    of nkAdd, nkSub:
      success(left.start_index, state.index, @[Node(kind: op,
          leftOp: left.value[0],
        rightOp: right.value[0])], "")
    else:
      raise newException(Exception, "not an operator")
  Parser[NodeKind, Node](fn: expression_parser)

let number_expr = test_item(nkNumber, "number").map(Node(kind: nkNumber))
expression = op_expr(nkSub) or op_expr(nkAdd) or number_expr

let tokens = e.parse("5 - 4")
echo tokens
let nodes = expression.parse(tokens)
echo nodes
