cd "$(echo $(git rev-parse --show-toplevel))"

regex="(?<=\+)(\d+)(?=(,\d+)?\s@@)"

if git status --porcelain | grep --silent $1; then
  git diff HEAD -U0 -- $1 | rg -o -P $regex | head -n 1
else
  git log --follow -p -U0 -- $1 | rg -o -P $regex | head -n 1
fi
