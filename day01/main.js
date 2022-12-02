const _ = require('../javascript/lodash.min')
require('../javascript/lodash.fp.min')(_)

const file = readFile("./input.txt")
const example = `1000
2000
3000

4000

5000
6000

7000
8000
9000

10000
`

const inputToElves = _.flow([
    _.split('\n'),
    lines => _.reduce((result, n) => {
            if (n == '') {
                result.elves.push(result.current)
                result.current = []
            } else {
                result.current.push(parseInt(n))
            }
            return result
        }, {current:[], elves:[]}, lines),
    _.property('elves')
])

const part1 = _.flow([
    inputToElves,
    _.map(_.sum),
    _.max
])

const part2 = _.flow([
    inputToElves,
    _.map(_.sum),
    _.sortBy(_.identity),
    _.reverse,
    _.take(3),
    _.sum
])

print(JSON.stringify({
    part1Ex: part1(example),
    part1: part1(file),
    part2Ex: part2(example),
    part2: part2(file),
}, null, 2));

1; debugger;
