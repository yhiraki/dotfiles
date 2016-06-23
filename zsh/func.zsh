# repo - cd to repogitory dir
repo() {
  local dir
  dir=$(ghq list > /dev/null | fzf-tmux -q "$*") &&
    cd $(ghq root)/$dir
}

# fbr - checkout git branch
branch() {
  local branches branch
  branches=$(git branch --all -vv) &&
  branch=$(echo "$branches" | fzf-tmux +m -q "$*") &&
  git checkout $(echo "$branch" | awk '{print $1}' | sed "s/.* //")
}

# fshow - git commit browser
gitshow() {
  git log --graph --color=always \
      --format="%C(auto)%h%d %s %C(black)%C(bold)%cr" "$@" |
  fzf --ansi --no-sort --reverse --tiebreak=index --bind=ctrl-s:toggle-sort \
      --bind "ctrl-m:execute:
                (grep -o '[a-f0-9]\{7\}' | head -1 |
                xargs -I % sh -c 'git show --color=always % | less -R') << 'FZF-EOF'
                {}
FZF-EOF"
}

# v - open files in neomru
v() {
  local files
    files=$(tail +2 $XDG_CACHE_HOME/neomru/file \
      | fzf-tmux -d -m -q "$*" -1) && $EDITOR ${files}
}

# fd - cd to selected directory
fd() {
  local dir
  dir=$(find ${1:-*} -path '*/\.*' -prune \
                  -o -type d -print 2> /dev/null | fzf-tmux +m) &&
  cd "$dir"
}

# fda - including hidden directories
fda() {
  local dir
  dir=$(find ${1:-.} -type d 2> /dev/null | fzf-tmux +m) && cd "$dir"
}

_fzf-select-file() {
  IFS='
'
  local -a declare files
  files=($(cat - | fzf-tmux --query="$1" --select-1 --exit-0))
  [[ -n "$files" ]] && ${EDITOR:-vim} "${files[@]}"
  unset IFS
}

# fe [FUZZY PATTERN] - Open the selected file with the default editor
#   - Bypass fuzzy finder if there's only one match (--select-1)
#   - Exit if there's no match (--exit-u
fe() {
  ls -a | grep -v -e "\.$" | _fzf-select-file $1
}

fea() {
  find . | sed -e 's/\.\///g' | _fzf-select-file $1
}

fsh() {
  ssh $(cat ~/.ssh/config | grep -e '^host' | sed -e 's/host //' | fzf-tmux -q "$*")
}
