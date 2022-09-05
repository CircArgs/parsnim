import std/[sequtils, strformat, strutils, re, options]
import sugar
type
  ParseError* = object of ValueError

  ResultType* = enum
    Match, Failure

  Result*[T] = object of RootObj ## Result objects are returned by all parsers
    kind*: ResultType             ## states whether the result is a `Match` or `Failure`
    value*: Stream[T] ## `Parser`s translate from one stream to another hence result values are `Stream`s
    start_index*, end_index*: int  ## start and end indices for the value/expected
    tag*: seq[string] ## an optional seq of strings that can be used to mark results for further processing
    description*: string ## describes what the value would represent. used when erroring on failed parsing
    expected*: Stream[T] ## for failures, what value wanted to be. used when erroring on failed parsing

  Stream*[T] = seq[T] ## Stream is an alias for seq[T]

  State*[T] = object ## object passed to all parsers to track position and the stream being parsed
    index*: int
    stream*: Stream[T]

  ParseFn*[T, R] = proc(s: var State[T]): Result[R] ## function defined to manipulate `State` for the next parser and return a result
  Parser*[T, R] = object ## central object to wrap all `ParseFn`s in
    fn*: ParseFn[T, R]
    description*: string

proc success*[T](start_index, end_index: int, value: Stream[T],
    description: string): Result[T] =
  ## function to generate `Match` `Results`
  Result[T](kind: ResultType.Match, value: value, start_index: start_index,
      end_index: end_index, description: description)

proc failure*[T](start_index, end_index: int, description: string): Result[T] =
  ## function to generate `Failure` `Results` without an expected value
  Result[T](kind: ResultType.Failure, start_index: start_index,
      end_index: end_index, description: description)

proc failure*[T](start_index, end_index: int, expected: Stream[T],
    description: string): Result[T] =
  ## function to generate `Failure` `Results` with an expected value
  Result[T](kind: ResultType.Failure, expected: expected,
      start_index: start_index, end_index: end_index, description: description)

converter to_bool*(res: Result): bool = ## Results are truthy/falsey
  res.kind == ResultType.Match

proc describe*(parser: var Parser, desc: string): Parser =
  ## describe a parser
  parser.description = desc
  parser

proc map*[T, R, NR](parser: Parser[T, R], map_fn: proc(x: seq[R]): seq[NR]): Parser[T, NR] =
  ## takes a parser that goes from Stream[T]->Stream[R] and transforms it to Stream[T]->Stream[NR]
  proc map_parser(state: var State[T]): Result[NR] =
    let old = parser.fn(state)
    Result[NR](kind: old.kind, value: map_fn(old.value), expected: map_fn(
        old.expected), start_index: old.start_index, end_index: old.end_index,
            tag: old.tag,
        description: old.description)
  Parser[T, NR](fn: map_parser, description: parser.description)

proc map*[T, R, NR](parser: Parser[T, R], map_fn: proc(x: R): NR): Parser[T, NR] =
  ## takes a parser that goes from Stream[T]->Stream[R] and transforms it to Stream[T]->Stream[NR]
  map[T, R, NR](parser, proc(s: seq[R]): seq[NR] = s.map_it(map_fn(it)))

proc map*[T, R, NR](parser: Parser[T, R], item: NR): Parser[T, NR] =
  ## takes a parser that goes from Stream[T]->Stream[R] and transforms it to Stream[T]->Stream[NR]
  map[T, R, NR](parser, proc(s: seq[R]): seq[NR] = s.map_it(item))

proc tag*[T, R](parser: Parser[T, R], tag: string): Parser[T, R] =
  ## creates a parser that tags `parser` results
  proc tag_parser(state: var State[T]): Result[R] =
    let old = parser.fn(state)
    Result(kind: old.kind, value: old.value,
        start_index: old.start_index, end_index: old.end_index, tag: tag,
        description: old.description)
  Parser(fn: tag_parser, description: parser.description)

proc `$`*(parser: Parser): string =
  fmt"<Parser: {parser.description}>"

proc `$`*(state: State): string =
  fmt"<State: {state.index}; {state.stream}>"

proc parse_partial*[T, R](parser: Parser[T, R], state: var State[T]): Result[R] =
  result = parser.fn(state)
  if not result:
    var expected = ""
    try:
      $result.expected
      expected = if result.expected.len > 0: fmt" {result.expected}" else: ""
    except:
      discard
    let got = try: fmt"got {state.stream[result.start_index..<result.end_index]}" except IndexDefect: "got `out of stream`" 
    raise newException(ParseError, fmt"failed to parse with error: Expected{expected} Description: `{result.description}` {got} @ {result.start_index}:{result.end_index}")

proc parse_partial*[T, R](parser: Parser[T, R], stream: Stream[T]): Result[R] =
  var state = State[T](stream: stream)
  parse_partial(parser, state)

proc eos_parser[T, R](state: var State[T]): Result[R] =
  if state.index >= state.stream.len:
    return success[R](state.index, state.index, @[], "`end of stream`")
  failure[R](state.index, state.index, "`end of stream`")

proc eos[T, R]: auto =
  ## parser spots the end of a stream
  Parser[T, R](fn: eos_parser[T, R],
    description: "`end of stream`")

proc parse*[T, R](parser: Parser[T, R], stream: Stream[T]): Stream[R] =
  ## run a parser on a stream
  result = parser.skip(eos[T, R]()).parse_partial(stream).value

proc parse*[char, R](parser: Parser[char, R], stream: string): Stream[R] =
  ## run a parser on a stream that is a string
  result = parser.skip(eos[char, R]()).parse_partial(stream.toSeq).value

proc skip*[T, R](parser, other: Parser[T, R]): Parser[T, R] =
  ## parses `parser` then `other` but ignores `other` result
  proc skip_parser(state: var State[T]): Result[R] =
    let first = parser.fn(state)
    if not first:
      return first
    let to_skip = other.fn(state)
    if not to_skip:
      return to_skip
    first
  Parser[T, R](fn: skip_parser, description: fmt"{parser.description} skip {other.description}")

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
  proc then_parser(state: var State[T]): Result[R] =
    result = parser.fn(state)
    if not result:
      return
    let other_result = other.fn(state)
    if not other_result:
      return other_result
    result = success(result.start_index, other_result.end_index, concat(
        result.value, other_result.value), description)
  Parser[T, R](fn: then_parser, description: description)

proc test_proc[T](test_proc_var: proc(x: T): bool, description: string,
    expected: Option[T]): Parser[T, T] =
  ## tests a procedure against the next value in the stream
  let exp = if expected.isSome: @[expected.get] else: @[]
  proc test_proc_parser(state: var State[T]): Result[T] =
    if state.index < state.stream.len:
      if test_proc_var(state.stream[state.index]):
        result = success[T](state.index, state.index+1, @[state.stream[
            state.index]], description)
        state.index+=1
        return
    result = failure[T](state.index, state.index+1, exp, description)
  Parser[T, T](fn: test_proc_parser, description: description)

proc test_proc*[T](test_proc_var: proc(x: T): bool,
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

proc test_proc*[T](test_proc_var: proc(x: T): bool, description: string,
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

proc test_seq*[T](test_seq: Stream[T], description: string): Parser[T, T] =
  ##[
    tests if the `test_seq` is the next sequence of the state's stream

  .. code-block:: nim
    let t = test_seq("abc".to_seq, "alphabet")
    echo t
    echo t.parse("abc")
    # <Parser: alphabet>
    # @['a', 'b', 'c']
  ]##
  proc test_seq_parser(state: var State[T]): Result[T] =
    var i = 1
    let start_index = state.index
    var values: Stream[T] = @[]
    if state.index < state.stream.len and test_seq[0] == state.stream[state.index]:
      values.add state.stream[state.index]
      state.index+=1
    while i < test_seq.len:
      if state.index < state.stream.len:
        if test_seq[i] == state.stream[state.index]:
          values.add state.stream[state.index]
          state.index+=1
      else:
        result = failure[T](start_index, state.index, test_seq, description)
        state.index = start_index
        return
      i+=1
    return success[T](start_index, state.index, values, description)

  Parser[T, T](fn: test_seq_parser, description: description)

proc test_item*[T](test: T, description: string): auto =
  ## test if a single `test` item is next in the stream
  test_proc(proc(x: T): auto = test == x, description, test)

proc test_char*(test: char): Parser[char, char] =
  ## test if a char is next in the stream (usually a string for char test)
  test_item(test, fmt"`char {test}`")

proc str*[T](parser: Parser[T, char]): Parser[T, string] =
  map[T, char, string](parser, proc(x: seq[char]): seq[string] = @[x.map_it($it).join])

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
  test_seq(test.to_seq, fmt"string {test}").str




proc test_regex*(pattern: Regex, description: string): Parser[char, string] =
  ##[
  tests if a regex pattern is next in a stream

  .. code-block:: nim
    let r = test_regex(re"[a-zA-Z_][a-zA-Z0-9_]*", "identifier")
    echo r
    echo r.parse("_hello1984")
    # <Parser: regex identifier>
    # @["_hello1984"]
  ]##
  proc regex_parser(state: var State[char]): Result[string] =
    let match = cast[string](state.stream[
        state.index..<state.stream.len]).findBounds(pattern)
    if match[0] > -1:
      let start_index = state.index
      state.index = match[1]+1+state.index
      return success(start_index, state.index, @[state.stream[match[0]..match[
          1]].map_it($it).join], description)
    failure[string](state.index, -1, description)
  Parser[char, string](fn: regex_parser,
      description: fmt"`regex {description}`")

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
  let description = if max == -1:
    fmt"{parser.description} {min} times"
    elif min == 0 and max == int.high: fmt"{parser.description} many times"
    elif min == 0 and max == 1: fmt"optional {parser.description}"
    elif min == 0: fmt"{parser.description} at most {max_count} times"
    elif max == int.high: fmt"{parser.description} at least {min} times"
    else: fmt"{parser.description} from {min} to {max_count} times"
  proc times_parser(state: var State[T]): Result[R] =
    var values: seq[R] = @[]
    var times = 0
    let start_index = state.index
    while times < max_count:
      result = parser.fn(state)
      if result:
        values.add(result.value)
        state.index = result.end_index
        times += 1
      elif times < min:
        result = failure[R](start_index, state.index, description)
        state.index = start_index
        return
      else:
        break
    return success(start_index, state.index, values, description)
  return Parser[T, R](fn: times_parser, description: description)

proc until*[T, R](self, parser: Parser[T, R]): Parser[T, R] =
  ##[
  looks for the current parser `self` until `parser` matches

  .. code-block:: nim
    let p = (test_char('c')).until(test_char('p'))
    echo p
    echo p.parse("ccccp")
    # <Parser: char c until not char c>
    # @['c', 'c', 'c', 'c', 'p']
  ]##
  let description = fmt"{self.description} until {parser.description}"
  proc until_parser(state: var State[T]): Result[R] =
    var values: seq[R] = @[]
    let start_index = state.index
    var last_index = start_index
    var until = parser.fn(state)
    while not until:
      state.index = last_index
      result = self.fn(state)
      if result:
        values.add(result.value)
        state.index = result.end_index
      else:
        result = failure[R](start_index, state.index, description)
        state.index = last_index
        return
      last_index = state.index
      until = parser.fn(state)
    return success(start_index, state.index, concat(values, until.value), description)
  return Parser[T, R](fn: until_parser, description: description)

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
  proc or_parser(state: var State[T]): Result[R] =
    let start_index = state.index
    result = parser.fn(state)
    if result:
      return
    let fail_end = result.end_index
    result = other.fn(state)
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
  proc and_parser(state: var State[T]): Result[R] =
    let start_index = state.index
    result = parser.fn(state)
    if not result:
      return
    state.index = start_index
    let first_value = result.value
    let end_index = result.end_index
    result = other.fn(state)
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
  proc not_parser(state: var State[T]): Result[R] =
    result = parser.fn(state)
    if result.kind == ResultType.Match:
      result.kind = ResultType.Failure
      swap(result.value, result.expected)
    else:
      result.kind = ResultType.Match
      result.value = state.stream[result.start_index..<result.end_index]
      result.expected = @[]
    result.description = fmt"not {result.description}"
    state.index = result.end_index
  Parser[T, R](fn: not_parser, description: description)

proc pad*(parser: Parser[char, string]): Parser[char, string] =
  ##[
  optional whitespace

  .. code-block:: nim
    
    echo test_string("hello").pad.parse("hello    ")
    # @["hello"]
  ]##
  parser.skip(test_regex(re"\s*", "padding").optional)

proc pad*(parser: Parser[char, char]): Parser[char, char] =
  ##[
  optional whitespace

  .. code-block:: nim
    
    echo test_char('c').pad.parse("c    ")
    # @['c']
  ]##
  parser.skip(test_regex(re"\s*", "padding").optional.map(' '))

proc space*(parser: Parser[char, string]): Parser[char, string] =
  ##[
  optional whitespace

  .. code-block:: nim
    
    let p = test_string("9").space
    echo p.parse("9")
    # @["9"]
  ]##
  parser.skip(test_char(' ').map(" "))

proc space*(parser: Parser[char, char]): Parser[char, char] =
  ##[
  optional whitespace

  .. code-block:: nim
    
    echo test_char('c').space.parse("c ")
    # @['c']
  ]##
  parser.skip(test_char(' '))
