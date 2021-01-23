cd "$(echo $(git rev-parse --show-toplevel))"
git diff HEAD -U0 -- $1 | rg -o -P "(?<=\+)(\d+)(?=(,\d+)?\s@@)" | head -n 1
