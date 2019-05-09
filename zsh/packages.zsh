# ln -s ~/.config/zsh/packages.zsh $ZPLUG_LOAD_FILE

zplug 'zplug/zplug', hook-build:'zplug --self-manage'
# zplug "aws/aws-cli", use:bin/aws_zsh_completer.sh, on:"zsh-users/zsh-completions", defer:2
zplug "b4b4r07/zsh-vimode-visual", defer:3
zplug "felixr/docker-zsh-completion"
zplug "hchbaw/opp.zsh", lazy:true
zplug "junegunn/fzf", as:command, use:bin/fzf-tmux, lazy:true
zplug "mollifier/anyframe"
zplug "paulirish/git-open", as:plugin
zplug "plugins/git", from:oh-my-zsh, if:"(( $+commands[git] ))", defer:2
zplug "plugins/docker", from:oh-my-zsh, if:"(( $+commands[docker] ))", defer:2
zplug "plugins/docker-compose", from:oh-my-zsh, if:"(( $+commands[docker-compose] ))", defer:2
zplug "mafredri/zsh-async"
zplug "sindresorhus/pure"
zplug "zsh-users/zsh-autosuggestions", defer:2
zplug "zsh-users/zsh-completions", lazy:true
zplug "zsh-users/zsh-history-substring-search", defer:2
zplug "zsh-users/zsh-syntax-highlighting", defer:2

zplug "$ZDOTDIR", from:local, use:"{alias,bind,func}.zsh"
