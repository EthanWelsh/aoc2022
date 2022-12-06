# Advent of Code 2022

Example usage:
- `stack run -- -d 9`: Runs Day 9's solutions.
- `stack run -- --day 14 --input "wibble.txt"`: Runs Day 14's solutions, using the input file "wibble.txt".
- `stack run -- -d 1 -i "alex.txt" --timings`: Runs Day 1's solutions, using the input file "alex.txt". Also prints timing information for each solution.
- `stack run -- --all-days`: Runs the solutions from all days.

This template can be used with `ghcid` to compile and run your code every time you save your files. Consider putting the following in your `.bashrc` (or equivalent):

```bash
function day { ghcid --run="Main.performDay (Options (OneDay $1 Nothing) Timings)" }
```
