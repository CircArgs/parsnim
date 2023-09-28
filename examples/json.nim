import parsnim
import re
import strutils
import sugar

type
  JsonNodeKind = enum
    jsonNumber, jsonString, jsonBool, jsonObject, jsonArray, jsonNull

  JsonNode = object
    case kind: JsonNodeKind
    of jsonNumber:
      nbrVal: float
    of jsonString:
      strVal: string
    of jsonBool:
      boolVal: bool
    of jsonObject:
      objVal: ref JsonNode
    of jsonArray:
      arrayVal: seq[JsonNode]
    of jsonNull: discard

# Primitives
proc lexeme(p: Parser): Parser=p<<pad.optional

let `true` = lexeme("true").to(JsonNode(kind: jsonBool, boolVal: true))
let `false` = lexeme("false").to(JsonNode(kind: jsonBool, boolVal: false))
let null = lexeme("null").to(JsonNode(kind: jsonNull))
let number = lexeme(re"-?(0|[1-9][0-9]*)([.][0-9]+)?([eE][+-]?[0-9]+)?").map((s:string)=>JsonNode(kind: jsonNumber, nbrVal: s.parse_float))

let str_base = generate[char, string] "string":
  discard step '"'
  result = step re"""[^"]+"""
  discard step '"'

let str = str_base.map((s: string)=>JsonNode(kind: jsonString, strVal: s))

# echo str
# echo str.then(number).parse(""""hello"5""")
# # Data structures
# json_value = forward_declaration()
# object_pair = seq(quoted << colon, json_value).map(tuple)
# json_object = lbrace >> object_pair.sep_by(comma).map(dict) << rbrace
# array = lbrack >> json_value.sep_by(comma) << rbrack

# # Everything
# json_value.become(quoted | number | json_object | array | true | false | null)
# json_doc = whitespace >> json_value


let simple = test_regex(re"[0-9]+")
var exp = simple
let group = generate[char, string] "group":
  discard step lparen
  var match = step exp
  var curr = match
  discard step ' '
  while curr:
    curr = step exp
    if curr:
      match=match&curr
      discard step " ".optional
  discard step rparen

exp = simple or group

echo exp.parse("(0 1 (2 3) (4 5 6) 7 8)")