import os

name = "-CBGP" ## Initial benchmarking of Code Building GP on Hamilton's HPC

problems = [
            ### PSB1
    # "checksum",
    "collatz-numbers",
    "compare-string-lengths",
    "count-odds",
    "digits",
    "double-letters",
    "even-squares",
    "for-loop-index",
    "grade",
    "last-index-of-zero",
    "median",
    "mirror-image",
    "negative-to-zero",
    "number-io",
    "pig-latin",
    "replace-space-with-newline",
    "scrabble-score",
    "small-or-large",
    "smallest",
    "string-differences",
    "string-lengths-backwards",
    "sum-of-squares",
    "super-anagrams",
    "syllables",
    "vector-average",
    "vectors-summed",
    "x-word-lines",

            ### PSB2
    "basement",
    "bouncing-balls",
    "bowling",
    "camel-case",
    "dice-game",
    "fizz-buzz",
    "fuel-cost",
    "gcd",
    "indices-of-substring",
    "leaders",
    "luhn",
    "middle-character",
    "paired-digits",
    "shopping-list",
    "snow-day",
    "solve-boolean",
    "spin-words",
    "square-digits",
    "substitution-cipher",
    "twitter",
    "vector-distance"
]

with open('hpc_launcher.template', 'r') as hpc_template:
    hpc_launcher_template = hpc_template.read()


for problem in problems:
    hpc_launcher = hpc_launcher_template.replace("#qsub-name#", problem + name)
    hpc_launcher = hpc_launcher.replace("#namespace#", problem)


    temp_filename = "temp_launcher.run"
    with open(temp_filename, 'w') as temp_launcher:
        temp_launcher.write(hpc_launcher)

    os.system("qsub " + temp_filename)
    os.remove(temp_filename)
