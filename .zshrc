# 設定はここを参考にした
# http://qiita.com/uasi/items/c4288dd835a65eb9d70

# pure
autoload -U promptinit && promptinit

# 固有設定の読込
[ -f ~/.zshrc.local ] && source ~/.zshrc.local

# zplug
export ZPLUG_HOME=$HOME/.zplug
if [ ! -d $ZPLUG_HOME ]; then
  git clone https://github.com/zplug/zplug $ZPLUG_HOME
fi
source $ZPLUG_HOME/init.zsh

zplug "junegunn/fzf", as:command, use:bin/fzf-tmux
zplug "zsh-users/zsh-syntax-highlighting", nice:10
zplug "mollifier/anyframe"
zplug "zsh-users/zsh-completions"
zplug "mafredri/zsh-async"
zplug "sindresorhus/pure"

# check installed
if ! zplug check --verbose; then
    # printf "Install? [y/N]: "
    # if read -q; then
        echo; zplug install
    # fi
fi

# load plugins
zplug load

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# 分割されたzshrc
ZSHHOME="${ZDOTDIR}/.zsh.d"

case ${OSTYPE} in
  darwin*)
    source $ZSHHOME/.zshrc.darwin
    ;;
  linux*)
    source $ZSHHOME/.zshrc.linux
    ;;
  msys*)
    source $ZSHHOME/.zshrc.msys
    ;;
esac

if which anyframe-widget-select-widget > /dev/null; then
  source $ZSHHOME/.zshrc.anyframe
fi

# cal 今日の日付に色を付ける
alias cal='cal | grep -C6 --color $(date +%d)'

repo() {
  local dir
  dir=$(ghq list > /dev/null | fzf-tmux) &&
    cd $(ghq root)/$dir
}

# fbr - checkout git branch
branch() {
  local branches branch
  branches=$(git branch --all -vv) &&
  branch=$(echo "$branches" | fzf-tmux +m) &&
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

# v - open files in ~/.viminfo
v() {
  local files
  files=$(grep '^>' ~/.viminfo | cut -c3- |
          while read line; do
            [ -f "${line/\~/$HOME}" ] && echo "$line"
          done | fzf-tmux -d -m -q "$*" -1) && $EDITOR ${files//\~/$HOME}
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
