import std/[sequtils, strformat, strutils]

type
  ParseError* = object of ValueError

  ResultType* = enum
    Match, Failure

  Result*[T] = object of RootObj
    kind: ResultType
    value: seq[T]
    start_index, end_index: int
    tag: seq[string]
    description: string
    expected: seq[T]

  Stream*[T] = seq[T]

  State*[T] = ref object
    index*: int
    stream*: Stream[T]

  ParseFn*[T, R] = proc(s: var State[T]): Result[R]
  Parser*[T, R] = object
    fn*: ParseFn[T, R]
    description*: string

proc success*[T](start_index, end_index: int, value: seq[T],
    description: string): Result[T] =
  Result[T](kind: ResultType.Match, value: value, start_index: start_index,
      end_index: end_index, description: description)

proc failure*[T](start_index, end_index: int, description: string): Result[T] =
  Result[T](kind: ResultType.Failure, start_index: start_index,
      end_index: end_index, description: description)

proc failure*[T](start_index, end_index: int, expected: seq[T], description: string): Result[T] =
  Result[T](kind: ResultType.Failure, expected: expected, start_index: start_index,
      end_index: end_index, description: description)

proc combine*(self, other: Result): Result =
  var (left, right) = (self, other)
  if self.start_index > other.start_index:
    swap(left, right)
  elif self.start_index == other.start_index:
    if not self and other:
      swap(left, right)

  if left and right:
    return Result(kind: ResultType.Match, value: concat(left.value,
        right.value), start_index: min(left.start_index, right.start_index),
            end_index: max(left.end_index, right.end_index), tag: concat(
        left.tag,
        right.tag), description: fmt"{left.description}, {right.description}")
  if not left:
    return left
  if not right:
    return right

converter to_bool*(res: Result): bool = res.kind == ResultType.Match

proc describe*(parser: var Parser, desc: string): Parser =
  parser.description = desc
  parser

proc map*[T, R, NR](parser: Parser[T, R], map_fn: proc(x: seq[R]): seq[
    NR]): Parser[T, NR] =
  proc map_parser(state: var State[T]): Result[NR] =
    let old = parser.fn(state)
    Result[NR](kind: old.kind, value: map_fn(old.value),
        start_index: old.start_index, end_index: old.end_index, tag: old.tag,
        description: old.description)
  Parser[T, NR](fn: map_parser, description: parser.description)

proc tag*[T, R](parser: Parser[T, R], tag: string): Parser[T, R] =
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
    let expected = if result.expected.len>0: fmt" {result.expected} " else: ""
    raise newException(ParseError, fmt"failed to parse with error: Expected {expected} {result.description} @ {result.start_index}:{result.end_index}")

proc parse_partial*[T, R](parser: Parser[T, R], stream: Stream[T]): Result[R] =
  var state = State[T](stream: stream)
  parse_partial(parser, state)

proc eos_parser*[T, R](state: var State[T]): Result[R] =
  if state.index >= state.stream.len:
    return success[R](state.index, state.index, @[], "end of stream")
  failure[R](state.index, state.index, "end of stream")

proc eos*[T, R]: auto = Parser[T, R](fn: eos_parser[T, R],
    description: "end of stream")

proc parse*[T, R](parser: Parser[T, R], stream: Stream[T]): seq[R] =
  result = parser.skip(eos[T, R]()).parse_partial(stream).value

proc parse*[T, R](parser: Parser[T, R], stream: string): seq[R] =
  result = parser.skip(eos[T, R]()).parse_partial(stream.toSeq).value

proc skip*[T, R](parser, other: Parser[T, R]): Parser[T, R] =
  proc skip_parser(state: var State[T]): Result[R] =
    let first = parser.fn(state)
    let to_skip = other.fn(state)
    if not to_skip:
      return to_skip.combine(first)
    first
  Parser[T, R](fn: skip_parser, description: fmt"{parser.description} skip {other.description}")

proc then*[T, R](parser, other: Parser[T, R]): Parser[T, R] =
  proc then_parser(state: var State[T]): Result[R] =
    result = parser.fn(state).combine(other.fn(state))
  Parser[T, R](fn: then_parser, description: fmt"{parser.description} then {other.description}")

proc test_proc*[T](test_proc_var: proc(x: T): bool, description: string, expected: T): Parser[T, T] =
  proc test_proc_parser(state: var State[T]): Result[T] =
    if state.index < state.stream.len:
      if test_proc_var(state.stream[state.index]):
        result = success[T](state.index, state.index+1, @[state.stream[
            state.index]], description)
        state.index+=1
        return
    result = failure[T](state.index, state.index+1, @[expected], description)
  Parser[T, T](fn: test_proc_parser, description: description)

proc test_seq*[T](test_seq: seq[T], description: string): Parser[T, T] =
  proc test_seq_parser(state: var State[T]): Result[T] =
    var i = 1
    let start_index = state.index
    var values: seq[T] = @[]
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

proc test_item*[T](test: T, description: string): auto = test_proc(proc(
    x: T): auto = test == x, description, test)

proc test_char*(test: char): auto = test_item(test, fmt"char {test}")

proc test_string*(test: string): auto = test_seq(test.to_seq,
    fmt"string {test}").map(proc(x: seq[char]): auto = @[x.map_it($it).join])

proc times*[T, R](parser: Parser[T, R], min: int, max: int = -1): Parser[T, R] =
  let max_count = if max == -1: min else: max
  let description = if max == -1: fmt"{parser.description} {min} times" else: fmt"{parser.description} from {min} to {max_count} times"
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

proc many*(parser: Parser): Parser = parser.times(0, int.high)

proc at_most*(parser: Parser, n: int): auto = parser.times(0, n)

proc at_least*(parser: Parser, n: int): auto = parser.times(n, int.high)

proc optional*(parser: Parser): auto = parser.times(0, 1)

proc `or`*[T, R](parser, other: Parser[T, R]): Parser[T, R] =
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
  let description = fmt"{parser.description} and {other.description}"
  proc and_parser(state: var State[T]): Result[R] =
    let start_index = state.index
    result = parser.fn(state)
    if not result:
      return
    state.index=start_index
    let first_value = result.value
    let end_index = result.end_index
    result = other.fn(state)
    if not result:
      return
    result = success[R](start_index, max(end_index, result.end_index), concat(first_value, result.value), description)
  Parser[T, R](fn: and_parser, description: description)

proc `not`*[T, R](parser: Parser[T, R]): Parser[T, R] =
  let description = fmt"not {parser.description}" 
  proc not_parser(state: var State[T]): Result[R] =
    result = parser.fn(state)
    echo result
    if result.kind == ResultType.Match:
      result.kind = ResultType.Failure 
    else:
      result.kind = ResultType.Match
    result.description = fmt"not {result.description}"
    let temp = concat(result.expected, result.value)
    result.value = concat(result.value, result.expected)
    result.expected = temp
    echo result
  Parser[T, R](fn: not_parser, description: description)

# let p = (not test_char('c')).until(test_char('p'))
# echo p.parse("ccccp".to_seq)
let p = not test_char('c')
echo p.parse("p")

# TODO generate template
# iterator wrapper in proc with final return