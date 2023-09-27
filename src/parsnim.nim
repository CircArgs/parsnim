import std/[sequtils, strformat, strutils, re, options]
{.experimental: "views".}
type
  Stream*[T] = openArray[T]

  ParseError* = object of ValueError

  ResultType* = enum
    Match, Failure

  Result*[T] = object of RootObj ## Result objects are returned by all parsers
    start_index*, end_index*: int  ## start and end indices for the value/expected
    description*: string ## describes what the value would represent. used when erroring on failed parsing
    case kind*: ResultType             ## states whether the result is a `Match` or `Failure`\
    of Match:
      value*: seq[T] ## `Parser`s translate from one stream to another hence result values are `Stream`s
    of Failure:
      expected*: seq[T] ## for failures, what value wanted to be. used when erroring on failed parsing

  ParseFn*[T, R] = proc(stream: Stream[T]; index: var int): Result[R] ## function defined to manipulate `State` for the next parser and return a result
  Parser*[T, R] = object ## central object to wrap all `ParseFn`s in
    fn*: ParseFn[T, R]
    description*: string

proc success*[T](start_index, end_index: int; value: seq[T]; description: string = ""): Result[T] =
  ## function to generate `Match` `Results`
  Result[T](kind: ResultType.Match, value: value, start_index: start_index,
      end_index: end_index, description: description)
proc success*[T](start_index, end_index: int; value: T; description: string = ""): Result[T] = success(start_index, end_index, @[value], description)

proc failure*[T](start_index, end_index: int; expected: seq[T]; description: string = ""): Result[T] =
  ## function to generate `Failure` `Results` with an expected value
  Result[T](kind: ResultType.Failure, expected: expected,
      start_index: start_index, end_index: end_index, description: description)

proc failure*[T](start_index, end_index: int; expected: T; description: string = ""): Result[T] = failure[T](start_index, end_index, @[expected], description)

proc `&`*[T](a, b: Result[T]): Result[T] = 
  if (a.kind, b.kind)==(Match, Match):
    return success[T](a.start_index.min(b.start_index), a.end_index.max(b.end_index), a.value & b.value)
  elif (a.kind, b.kind)==(Match, Failure):
    return b
  elif (a.kind, b.kind)==(Failure, Match):
    return a
  else:
    return failure[T](a.start_index.min(b.start_index), a.end_index.max(b.end_index), a.expected & b.expected)

converter to_bool*(res: Result): bool {.inline.} = res.kind == ResultType.Match ## Results are truthy/falsey

proc describe*(parser: var Parser, desc: string): Parser =
  ## describe a parser
  parser.description = desc
  parser

proc map*[T, R, NR](parser: Parser[T, R], map_fn: proc(x: seq[R]): seq[NR]): Parser[T, NR] =
  ## takes a parser that goes from Stream[T]->Stream[R] and transforms it to Stream[T]->Stream[NR]
  proc map_parser(stream: Stream[T]; index: var int): Result[NR] =
    let old = parser.fn(stream, index)
    if old.kind == Match:
      success(old.start_index, old.end_index, old.value, old.description)
    else:
      failure(old.start_index, old.end_index, old.expected, old.description)
  Parser[T, NR](fn: map_parser, description: parser.description)

proc map*[T, R, NR](parser: Parser[T, R], map_fn: proc(x: R): NR): Parser[T, NR] =
  ## takes a parser that goes from Stream[T]->Stream[R] and transforms it to Stream[T]->Stream[NR]
  map[T, R, NR](parser, proc(s: seq[R]): seq[NR] = s.map_it(map_fn(it)))

proc map*[T, R, NR](parser: Parser[T, R], item: NR): Parser[T, NR] =
  ## takes a parser that goes from Stream[T]->Stream[R] and transforms it to Stream[T]->Stream[NR] of just `item`
  map[T, R, NR](parser, proc(s: seq[R]): seq[NR] = s.map(proc(x: auto): auto = item))

proc `$`*(parser: Parser): string =
  fmt"<Parser: {parser.description}>"

proc parse_partial*[T, R](parser: Parser[T, R]; stream: Stream[T]; index: var int): Result[R] =
  result = parser.fn(stream, index)
  if not result:
    var expected=""
    if result.expected.len==1:
      expected = $(result.expected[0])
      if result.expected[0] is string:
        expected= &"\"{expected}\""
    elif result.expected is seq[string]:
      expected = &"\"{result.expected.join}\""
    else:
      expected = ($result.expected)[1..^1]

    var show = ""
    let show_seq = if result.start_index!=result.end_index:
      echo result, " ", min(result.end_index, stream.len), " ", stream.len, " ", result.end_index
      stream[result.start_index..<min(result.end_index, stream.len)] 
    else: 
      stream[result.start_index..<result.start_index+1]
    if show_seq.len == 1:
      show = $show_seq[0]
    elif result.expected is seq[string]:
      show = &"\"{show_seq.join}\""
    else:
      show = ($show_seq)[1..^1]

    let got = try: fmt"got {show}" except IndexDefect, RangeDefect: "got `out of stream` or malformed result" 
    let range = if result.start_index!=result.end_index: fmt"{result.start_index}:{result.end_index}" else: fmt"{result.start_index}:..."
    let description = if result.description.len>0: fmt" ({result.description}) " else: " "
    raise newException(ParseError, fmt"Failed to parse. Expected {expected}{description}{got} @ {range}")

proc eos_parser[T, R](stream: Stream[T]; index: var int): Result[R] =
  if index >= stream.len:
    return success[R](index, index, @[], description = "`end of stream`")
  failure[R](index, index, @[], description = "`end of stream`")

proc eos[T, R]: auto =
  ## parser spots the end of a stream
  Parser[T, R](fn: eos_parser[T, R],
    description: "`end of stream`")

proc parse*[T, R](parser: Parser[T, R]; stream: Stream[T]; index: var int): seq[R] =
  ## run a parser on a stream
  return parser.skip(eos[T, R]()).parse_partial(stream, index).value

proc parse*[T, R](parser: Parser[T, R]; stream: Stream[T]; index: int = 0): seq[R] =
  ## run a parser on a stream
  var index = index
  return parser.skip(eos[T, R]()).parse_partial(stream, index).value

proc skip*[T, R](parser, other: Parser[T, R]): Parser[T, R] =
  ## parses `parser` then `other` but ignores `other` result
  proc skip_parser(stream: Stream[T]; index: var int): Result[R] =
    # dump state
    # dump parser
    # dump other
    let first = parser.fn(stream, index)
    # dump first
    if not first:
      return first
    let to_skip = other.fn(stream, index)
    # dump to_skip
    if not to_skip:
      return to_skip
    first
  Parser[T, R](fn: skip_parser, description: fmt"{parser.description} skip {other.description}")

proc `<<`*[T,R](parser, other: Parser[T, R]): auto = skip(parser, other)

proc then*[T, R](parser, other: Parser[T, R]): Parser[T, R] =
  ##[
    parses `parser` then `other` and rolls them together

  .. code-block:: nim
    let t = test_char('c').then(test_char('p'))
    echo t
    echo t.parse("cp")
    # <Parser: char c then char p>
    # @['c', 'p']
  ]##
  let description = fmt"{parser.description} then {other.description}"
  proc then_parser(stream: Stream[T]; index: var int): Result[R] =
    result = parser.fn(stream, index)
    if not result:
      return
    let other_result = other.fn(stream, index)
    if not other_result:
      return other_result
    result = success(result.start_index, other_result.end_index, concat(
        result.value, other_result.value), description)
  Parser[T, R](fn: then_parser, description: description)

proc `>>`*[T,R](parser, other: Parser[T, R]): auto = then(parser, other)

proc then*[T,R](a, b: Parser[T, R]; rest: varargs[Parser[T, R]]): auto =
  result = a >> b
  for p in rest:
    result = result >> p

proc test_proc[T](test_proc_var: proc(x: T): bool; description: string; expected: Option[T]): Parser[T, T] =
  ## tests a procedure against the next value in the stream
  let exp = (if expected.isSome: @[expected.get] else: @[])
  proc test_proc_parser(stream: Stream[T]; index: var int): Result[T] =
    if index < stream.len:
      if test_proc_var(stream[index]):
        result = success[T](index, index+1, @[stream[
            index]], description)
        index+=1
        return
    failure[T](index, index+1, exp, description)
  Parser[T, T](fn: test_proc_parser, description: description)

proc test_proc*[T](test_proc_var: proc(x: T): bool;
    description: string): Parser[T, T] =
  ##[
  tests a procedure against the next value in the stream

  .. code-block:: nim
    let p = test_proc(proc(c: char): auto = parseInt($c)>2, "greater than 2")
    echo p
    echo p.parse("4")
    # <Parser: greater than 2>
    # @['4']
  ]##
  test_proc[T](test_proc_var, description, none(T))

proc test_proc*[T](test_proc_var: proc(x: T): bool; description: string; 
    expected: T): Parser[T, T] =
  ##[
  tests a procedure against the next value in the stream

  .. code-block:: nim
    let p = test_proc(proc(c: char): auto = c== '4', "the character '4'", "'4'")
    echo p
    echo p.parse("4")
    # <Parser: the character '4'>
    # @['4']
  ]##
  test_proc[T](test_proc_var, description, some(expected))

proc test_item*[T](test: T; description: string): auto =
  ## test if a single `test` item is next in the stream
  test_proc(proc(x: T): auto = test == x, description, test)

proc test_char*(test: char): Parser[char, char] =
  ## test if a char is next in the stream (usually a string for char test)
  test_item(test, fmt"`char {test}`")

converter to_parser*(c: char): auto = test_char(c)



# converter str*[T, R](parser: Parser[T, R]): Parser[T, R] =  
#   map[T, R, R](parser, proc(x: seq[R]): seq[R] = @[x.map_it(it).join])  

# converter str*(parser: Parser[char, string]): Parser[char, string] =  
#   map[char, string, string](parser, proc(x: seq[string]): seq[string] = @[x.map_it(it).join])  

# converter str*[T](parser: Parser[T, char]): Parser[T, string] =  
#   map[T, char, string](parser, proc(x: seq[char]): seq[string] = @[x.map_it($it).join])

proc anything*[T] = test_proc(proc(_: [T]): bool = true, "anything")
proc nothing*[T] = test_proc(proc(_: [T]): bool = false, "nothing")

proc test_string*(test: string): auto =
  ##[
    tests if a string is next in a stream (special case of `test_seq`)

  .. code-block:: nim
    let t = test_string("abc")
    echo t
    echo t.parse("abc")
    # <Parser: string abc>
    # @["abc"]
  ]##
  
  let description = &"string \"{test}\""
  proc test_string_parser(stream: Stream[char]; index: var int): Result[string] =
    if index+test.len<=stream.len:
      if stream[index..<index+test.len]==test:
        result = success[string](index, index+test.len, test, description)
        index+=test.len
        return
    
    failure[string](index, index+test.len, test, description)
  Parser[char, string](fn: test_string_parser, description: description)

converter to_string_parser*(c: char): auto = test_string($c)
converter to_parser*(s: string): auto = test_string(s)

proc test_regex*(pattern: Regex, description: string = ""): Parser[char, string] =
  ##[
  tests if a regex pattern is next in a stream

  .. code-block:: nim
    let r = test_regex(re"[a-zA-Z_][a-zA-Z0-9_]*", "identifier")
    echo r
    echo r.parse("_hello1984")
    # <Parser: regex identifier>
    # @["_hello1984"]
  ]##
  proc regex_parser(stream: Stream[char]; index: var int): Result[string] =
    let match = cast[string](stream[index..<stream.len]).findBounds(pattern)
    if (match[0] == 0) and (match[1] > -1):
      let start_index = index
      index = match[1]+1+start_index
      result = success(start_index, index, @[stream[start_index..<index].map_it($it).join], description)
    else:
      result = failure[string](index, index, description)
  Parser[char, string](fn: regex_parser, description: fmt"`regex {description}`")

converter to_parser*(r: Regex): auto = test_regex(r)

proc times*[T, R](parser: Parser[T, R], min: int, max: int = -1): Parser[T, R] =
  ##[
  looks for `parser` to repeat >=min and <=max times

  .. code-block:: nim
    let t = (not test_char('c')).times(3)
    echo t
    echo t.parse("abd")
    # <Parser: not char c 3 times>
    # @['a', 'b', 'd']
  ]##
  
  let max_count = if max == -1: min else: max
  if max_count==1 and min==1: return parser
  let description = if max == -1:
    fmt"{parser.description} {min} times"
    elif min == 0 and max == int.high: fmt"{parser.description} many times"
    elif min == 0 and max == 1: fmt"optional {parser.description}"
    elif min == 0: fmt"{parser.description} at most {max_count} times"
    elif max == int.high: fmt"{parser.description} at least {min} times"
    else: fmt"{parser.description} from {min} to {max_count} times"
  proc times_parser(stream: Stream[char]; index: var int): Result[R] =
    var values: seq[R] = @[]
    var times = 0
    let start_index = index
    while times < max_count:
      result = parser.fn(stream, index)
      if result:
        values.add(result.value)
        times += 1
      elif times < min:
        result = failure[R](start_index, index, description)
        index = start_index
        return
      else:
        break
    return success(start_index, index, values, description)
  return Parser[T, R](fn: times_parser, description: description)

proc `*`*[T, R](parser: Parser[T, R], n: int): Parser[T, R] = parser.times(n)
proc `[]`*[T, R](parser: Parser[T, R], n: Slice[int]): Parser[T, R] = parser.times(n.a, n.b)

proc many*(parser: Parser): Parser =
  ##[
  finds `parser` until it doesn't

  .. code-block:: nim
    let t = (test_char('c')).many().then(test_char('p'))
    echo t
    echo t.parse("ccccp")
    # <Parser: char c many times then char p>
    # @['c', 'c', 'c', 'c', 'p']
  ]##
  parser.times(0, int.high)

proc at_most*(parser: Parser, n: int): auto =
  ## finds `parser` up to n times
  parser.times(0, n)

proc at_least*(parser: Parser, n: int): auto =
  ## finds `parser` at least n times
  parser.times(n, int.high)

proc optional*(parser: Parser): auto =
  ## tries to find `parser` and if not continues
  parser.times(0, 1)

proc `or`*[T, R](parser, other: Parser[T, R]): Parser[T, R] =
  ##[
  `parser` or `other` must match

  .. code-block:: nim
    let p = test_char('c') or test_char('p')
    echo p
    echo p.parse("p")
    # <Parser: char c or char p>
    # @['p']
  ]##
  let description = fmt"{parser.description} or {other.description}"
  proc or_parser(stream: Stream[char]; index: var int): Result[R] =
    let start_index = index
    result = parser.fn(stream, index)
    if result:
      return
    let fail_end = result.end_index
    result = other.fn(stream, index)
    if result:
      return
    result = failure[R](start_index, max(fail_end, result.end_index), description)
  Parser[T, R](fn: or_parser, description: description)


proc `and`*[T, R](parser, other: Parser[T, R]): Parser[T, R] =
  ##[
  `parser` and `other` must both match at the current position in the stream

  .. code-block:: nim
    let p = test_proc(proc(c: char): auto = parseInt($c)>2, "greater than 2") and test_proc(proc(c: char): auto = parseInt($c)<= 10, "less than or equal to 10")
    echo p
    echo p.parse("4")
    # <Parser: greater than 2 and less than or equal to 10>
    # @['4']
    p.parse("1")
    # Error: unhandled exception: failed to parse with error: Expected `greater than 2` got @['1'] @ 0:1 [ParseError]
  ]##
  let description = fmt"{parser.description} and {other.description}"
  proc and_parser(stream: Stream[char]; index: var int): Result[R] =
    let start_index = index
    result = parser.fn(stream, index)
    if not result:
      return
    index = start_index
    let first_value = result.value
    let end_index = result.end_index
    result = other.fn(stream, index)
    if not result:
      return
    result = success[R](start_index, max(end_index, result.end_index),
        result.value, description)
  Parser[T, R](fn: and_parser, description: description)

proc `not`*[T, R](parser: Parser[T, R]): Parser[T, R] =
  ##[
  inverts a `Result` so that a failure is instead a match and vice-versa

  .. code-block:: nim
    let p = not test_char('c')
    echo p
    echo p.parse("p")
    # <Parser: not char c>
    # @['p']
  ]##
  let description = fmt"not {parser.description}"
  proc not_parser(stream: Stream[T]; index: var int): Result[R] =
    var res = parser.fn(stream, index)
    let description = fmt"not {res.description}"
    if res.kind == ResultType.Match:
      result = failure(res.start_index, res.end_index, res.value, description)
    else:
      result = success(res.start_index, res.end_index, stream[res.start_index..<res.end_index], description)
    index = res.end_index
  Parser[T, R](fn: not_parser, description: description)

template step*(input: untyped): untyped =
  ##[
  for use in `generate`
  creates a parse step on a stream and index within a `generate` parser
  ]##
  (`input`).parse_partial(stream, index)

template generate*[T, R](desc: string; body: untyped): untyped =
  ##[
    creates a parser based on a codeblock of `Parser`s and `Result` manipulation
    .. code-block:: nim
      let lparen = "("
      let rparen = ")"


      let t = generate[char, string] "unwrap number":
        discard step lparen
        result = step re"[0-9]+"
        discard step rparen

      echo t
      echo t.parse("(7)")
      # <Parser: unwrap number>
      # @["7"]
  ]##
  block:
    let temp = proc(stream{.inject.}: Stream[T], index{.inject.}: var int): Result[R] =
      `body`
    
    Parser[T, R](fn: temp, description: desc)
