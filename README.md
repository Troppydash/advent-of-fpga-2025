Advent of FPGA submission
===========================

Problems solved
- `src/code_breaker.ml`, solution to the 2025 aoc day 1 part 1 and 2
    - build using `bin/generate.exe code-breaker 1` for part 1
    - build using `bin/generate.exe code-breaker 2` for part 2
- `src/joltage.ml`, solution to the 2025 aoc day 3 part 1 and 2
    - build using `bin/generate.exe code-breaker 2` for part 1
    - build using `bin/generate.exe code-breaker 12` for part 2


## Building
For development, run
```
dune build --watch --terminal-persistence=clear-on-rebuild-and-flush-history bin/generate.exe @runtest
```

For a complete build with expect tests, run
```
dune build bin/generate.exe @runtest
```

To generate RTL, run
```
bin/generate.exe <name> <arg>
```

## Input formats
- `src/code_breaker.ml` (part 1)
    - inputs are integers fed into the input wire `data_in`, with positive integers indicating a right rotation and negative integers indicate a left rotation (the magnitude is the amount of rotation)
    - output is an integer of the wire `times`, denoting the solution to the problem
    - testbench is located in `test/test_code_breaker.ml`, with one expect test verifying the correctness on the full aoc input
    - algorithm description
        - the `position` register stores the dial position (mod 100) after every rotation
        - for each input rotation, compute new position, use fixed point division to compute the remainder of the new position mod 100, and update position using remainder
        - if new position is 0, increment `count` register
        - after `finish` wire is vdd, output `count` by setting `count_valid` to vdd
        - the fixed point div 100 quotient is computed by
            ```
            x / 100
            => x * 0.01 / 1
            => x * 0.01 * 2^37 / 2^37
            => (x * 1374389534.72) << 37
            => (x * 1374389535) << 37
            # which could overshoot if x is negative, so a remainder check of 100 is performed after
            ```
- `src/code_breaker.ml` (part 2)
    - same inputs as part 1
    - same outputs as part 2, meaning changed to the number of zeros passed
    - algorithm description
        - the `position` register stores the dial position (mod 100) after every rotation
        - for each input rotation, compute new position, use fixed point division to compute the remainder of the new position mod 100, and update position using remainder
        - to compute the number of zeros passed, accumulate the (fixed) absolute quotient (during /100) if turning in the positive directly. For negative turns, accumulate the absolute quotient with the following exceptions
            - if `position` non-zero and new position zero, additionally add one
            - if `position` zero and new position non-zero, additionally subtract one
        - The accumulator is in the register `count` which is also the output value
        
- `src/joltage.ml` (part 1 and 2)
    - inputs are 8 bit integers denoting the digits for a subproblem (one line) in wire `data_in`
    - output is a single 64 bit integer `solution` denoting the problem solution
    - algorithm description
        - let `n` be the number of digits to choose (part 1 has `n=2`, part 2 has `n=12`)
        - create `n+1` 64-bit registers as dp memory, with `dp[i]` denoting the longest `i` digit number so far. Initialize all to zero
        - for each new digit, update dp using recurrence `dp[i] = max(dp[i], dp[i-1]*10+d)` where `d` is the new digit
        - answer is the last register in the dp array, or `dp[n]`
        