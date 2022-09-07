import parsnim
import std/[re, strutils, strformat, tables]
import sugar
type
  NodeKind = enum # the different node types
    Unk,
    Number,       # a leaf with a number value
    Add,          # an addition
    Sub,          # a subtraction
    Mul,          # a multiplication
    Div           # a division

  Node = ref NodeObj

  NodeObj = object
    case kind: NodeKind 
    of Unk: discard
    of Number: val: float
    of Add, Sub, Mul, Div:
      leftOp, rightOp: Node

proc `$`(node: Node): string =
  case node.kind:
  of Add, Sub, Mul, Div:
    fmt"{node.kind}({node.leftOp}, {node.rightOp})"
  of Number:
    fmt"Number({node.val})"
  else:
    $node.kind


let op_map = {'+': Add, '-': Sub, '*': Mul, '/':Div}.toTable
let add = test_char('+').str#.pad.map(nkAdd)
let mul = test_char('*').str#.pad.map(nkAdd)
let sub = test_char('-').str#.pad.map(nkSub)
let divide = test_char('/').str#.pad.map(nkSub)
let num_re =re"[+-]?([0-9]*[.])?[0-9]+" 
let number = test_regex(num_re, "number")#.pad.map(proc(s: auto): Node(kind: Number, val: s.parse_float))

let tokenizer = (number or sub or add or mul or divide).pad.many()
echo tokenizer
echo tokenizer.parse("5+ 4")
#@["5", "+", "4"]

# base expression will be replaced later
var expression = test_item("", "").map(Node(kind: Unk))

let op_expr = proc(op: char): Parser[string, Node] =
  proc expression_parser(state: var State[string]): Result[Node] =
    let index = state.stream[state.index..<state.stream.len].find($op)
    if index == -1:
      return failure[Node](state.index, state.index+1, "")
    let left = expression.parse_partial(state.stream[state.index..<index])
    let right = expression.parse_partial(state.stream[
        index+1..<state.stream.len])
    state.index = right.end_index + index+1
    let kind = op_map[op]
    case kind:
    of Add, Sub, Mul, Div:
      success(left.start_index, state.index, @[Node(kind: kind,
          leftOp: left.value[0],
        rightOp: right.value[0])], "")
    else:
      raise newException(Exception, "not an operator")
  Parser[string, Node](fn: expression_parser, description: $op)

let number_expr = map[string, string, Node](test_regex_string(num_re, "number"), proc(s: string): Node = Node(kind: Number, val: s.parse_float))
expression = op_expr('-') or op_expr('+') or op_expr('*') or op_expr('/') or number_expr
echo expression
#<Parser: - or + or * or / or number>

let tokens = tokenizer.parse("3.14 - 4/-2")
echo tokens
#@[Sub(Number(3.14), Div(Number(4.0), Number(-2.0)))]
let nodes = expression.parse(tokens)
echo nodes
#@[Sub(Number(3.14), Div(Number(4.0), Number(-2.0)))]