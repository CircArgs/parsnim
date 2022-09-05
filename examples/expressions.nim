import parsnim
import std/[re, strutils]

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

let add = test_char('+').pad.map(nkAdd)
let sub = test_char('-').pad.map(nkSub)
let number = test_regex(re"[0-9]+", "number").pad.map(nkNumber)

let e = number.then(add).then(number)
echo e.parse("5 + 4")
#@[nkNumber, nkAdd, nkNumber]
# def op_expr(op):
#     @Parser
#     def a_op_b(stream, index):
# #         import pdb; pdb.set_trace()
#         if op not in stream[index:]:
#             return Result.failure(index, op)
#         idx = stream.index(op)
# #         print(idx)
#         a = expression(stream[index:idx], 0)
# #         print(a)
#         a=a._replace(index = index)
#         if not a.status: # end early
#             return a
#         b = expression(stream[idx+1:], 0)
#         b=b._replace(index=idx+b.index+1)
#         if not b.status:
#             return b
#         b=b._replace(value = BinOp(a.value, op, b.value))
#         return b.aggregate(a)
#     return a_op_b

# expression = op_expr(Operator.Minus)\
#                 | op_expr(Operator.Plus)\
#                 | op_expr(Operator.Div)\
#                 | op_expr(Operator.Mul)\
#                 | number

var expression = test_item(nkUnk, "").map(Node(kind: nkUnk))

let op_expr = proc(op: NodeKind): Parser[NodeKind, Node] =
  proc expression_parser(state: var State[NodeKind]): Result[Node] =
    let index = state.stream[state.index..<state.stream.len].find(op)
    if index == -1:
      return failure[Node](state.index, state.index+1, "")
    let left = expression.parse_partial(state.stream[state.index..<index])
    let right = expression.parse_partial(state.stream[
        index+1..<state.stream.len])
    state.index = right.end_index
    success(state.index, right.end_index, @[Node(kind: op, leftOp: left.value[0],
        rightOp: right.value[0])], "")
  Parser[NodeKind, Node](fn: expression_parser)
