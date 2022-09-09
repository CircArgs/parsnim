import parsnim
import std/re

let year = test_regex(re"[0-9]{4}", "year")
echo year.parse("2017")
# @["2017"]

# echo year.parse("abc")
# Error: unhandled exception: failed to parse with error: Expected Description: `year` got @['a', 'b', 'c'] @ 0:... [ParseError]

let month = test_regex(re"[0-9]{2}", "month").pad
let day =   test_regex(re"[0-9]{2}", "day").pad
let dash =  test_string("-").pad

let fulldate = year.skip(dash.optional).then(month).skip(dash.optional).then(day)

# echo fulldate.parse("2017-xx")
# Error: unhandled exception: failed to parse with error: Expected Description: `month` got @['x', 'x'] @ 5:... [ParseError]
# echo fulldate.parse("2017-01")
# Error: unhandled exception: failed to parse with error: Expected Description: `day` got @[] @ 7:... [ParseError]
echo fulldate.parse("2017-02-01")
# @["2017", "02", "01"]


echo fulldate.parse("201702-01")
# @["2017", "02", "01"]


echo fulldate.parse("2017-02- 01")
# @["2017", "02", "01"]

echo fulldate.parse("20170201")
# @["2017", "02", "01"]

echo fulldate.parse("2017   02-01")
# @["2017", "02", "01"]
