_fzf-select-repo-dir(){
  local dir
  dir=$(ghq list > /dev/null | fzf-tmux -q "$*") &&
    echo $(ghq root)/$dir
}

# repo - cd to repogitory dir
repo() {
  cd $(_fzf-select-repo-dir "$*")
}

_fzf-select-branch(){
  local branches branch
  branches=$(git branch --all -vv) &&
  branch=$(echo "$branches" | fzf-tmux +m -q "$*") &&
  echo $(basename $(echo "$branch" | awk '{print $1}' | sed "s/.* //"))
}

# fbr - checkout git branch
checkout() {
  git checkout $(_fzf-select-branch "$*")
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

gitroot(){
  cd $(git rev-parse --show-toplevel)
}

# v - open files in neomru
v() {
  local files
    files=$(tail -n +2 $XDG_CACHE_HOME/neomru/file \
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

_fzf-select-files() {
  IFS='
'
  local -a declare files
  files=($(cat - | fzf-tmux --query="$1" -m --select-1 --exit-0))
  # [[ -n "$files" ]] && ${EDITOR:-vim} "${files[@]}"
  [[ -n "$files" ]] && echo "${files[@]}"
  unset IFS
}

_find-current-dir(){
  find . | sed -e 's/\.\///g'
}

# fe [FUZZY PATTERN] - Open the selected file with the default editor
#   - Bypass fuzzy finder if there's only one match (--select-1)
#   - Exit if there's no match (--exit-u
fe() {
  $EDITOR $(ls -a | grep -v -e "\.$" | _fzf-select-files $1)
}

fea() {
  $EDITOR $(_find-current-dir | _fzf-select-files $1)
}

fsh() {
  ssh $(cat ~/.ssh/config | grep -i -e '^host' | sed -e 's/host //i' | fzf-tmux -q "$*")
}

_select-files-in-dir(){
  find $1 -type f | _fzf-select-files
}

_select-dirs-in-dir(){
  find $1 -type d | _fzf-select-files
}

_select-dirs-in-repo(){
  _select-dirs-in-dir $(_fzf-select-repo-dir)
}

_select-files-in-repo(){
  _select-files-in-dir $(_fzf-select-repo-dir)
}


alias -g dlf='$(_select-files-in-dir ~/Downloads)'
alias -g dld='$(_select-dirs-in-dir ~/Downloads)'
alias -g junkf='$(_select-files-in-dir ~/.cache/junkfile/)'
alias -g repod='$(_select-dirs-in-repo)'
alias -g repof='$(_select-files-in-repo)'
alias -g bra='$(_fzf-select-branch)'
