import parsnim
import std/re

type
  NodeKind = enum  # the different node types
    nkUnk,
    nkNumber,        # a leaf with a number value
    nkAdd,          # an addition
    nkSub,          # a subtraction

  Node = ref NodeObj

  NodeObj = object
    case kind: NodeKind  # the `kind` field is the discriminator
    of nkUnk: discard
    of nkNumber: val: float
    of nkAdd, nkSub:
      leftOp, rightOp: Node

let add = test_char('+').pad.map(nkAdd)
let sub = test_char('-').pad.map(nkSub)
let number = test_regex(re"[0-9]+", "number").pad.map(nkNumber)

let e = number.then(add).then(number)
echo e.parse("5  +4")
#@[nkNumber, nkAdd, nkNumber]