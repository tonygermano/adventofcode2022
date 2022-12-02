const _ = require('../javascript/lodash.min')
require('../javascript/lodash.fp.min')(_)

const file = readFile("./input.txt")
const example = `A Y
B X
C Z
`
const inputToGames = _.flow([
    _.split('\n'),
    _.filter('length'),
    _.map(_.split(' '))
])

const strengthMap = {
    A: 1,
    B: 2,
    C: 3,
    X: 1,
    Y: 2,
    Z: 3
}

const gameToStrength = ([l,r]) => [strengthMap[l], strengthMap[r]]

/* 0 = loss; 1 = draw; 2 = win */
const getOutcome = ([l,r]) => (r - l + 4) % 3

const scoreMatch = ([l,r]) => getOutcome([l,r]) * 3 + r

// const part1 = () => 'TODO'

const part1 = _.flow([
    inputToGames,
    _.map(gameToStrength),
    _.map(scoreMatch),
    _.sum
])

const gameToStrength2 = ([l,r]) => {
    l = strengthMap[l]
    r = r == 'X' ? (l - 1) :
        r == 'Y' ? l :
        l + 1
    r = r == 0 ? 3 :
        r == 4 ? 1 :
        r
    return [l,r]
}

//const part2 = () => 'TODO'

const part2 = _.flow([
    inputToGames,
    _.map(gameToStrength2),
    _.map(scoreMatch),
    _.sum
])

print(JSON.stringify({
    part1Ex: part1(example),
    part1: part1(file),
    part2Ex: part2(example),
    part2: part2(file),
}, null, 2));

1; debugger;
