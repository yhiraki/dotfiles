# \e → ESC

# vi bind でも emacs bind を使用する
# https://qiita.com/b4b4r07/items/8db0257d2e6f6b19ecb9
bindkey -M viins '^?' backward-delete-char
bindkey -M viins '^A' beginning-of-line
bindkey -M viins '^B' backward-char
#                '^C' SIGINT
bindkey -M viins '^D' delete-char-or-list
bindkey -M viins '^E' end-of-line
bindkey -M viins '^F' forward-char
bindkey -M viins '^G' send-break
bindkey -M viins '^H' backward-delete-char
#                '^I' expand-and-complete (TAB)
#                '^J' accept-line (RETURN)
bindkey -M viins '^K' kill-line
#                '^L' clear-screen
#                '^M' accept-line (RETURN)
bindkey -M viins '^N' down-line-or-history
#                '^O' nil
bindkey -M viins '^P' up-line-or-history
#                '^Q' vi-quoted-insert
bindkey -M viins '^R' widget-search-history-incremental
#                '^S' nil
#                '^T' nil
bindkey -M viins '^U' backward-kill-line
#                '^V' vi-quoted-insert
bindkey -M viins '^W' backward-kill-word
#                '^X' prefix
bindkey -M viins '^Y' yank
#                '^X' prefix
#                '^Z' suspend

# zsh-autosuggestions
bindkey '^ ' autosuggest-accept

bindkey -M viins '\efd' widget-find-download-file
bindkey -M viins '\eff' widget-find-file
bindkey -M viins '\efj' widget-find-junkfile
bindkey -M viins '\egb' widget-branch-name
bindkey -M viins '\egf' widget-find-current-repo-file
bindkey -M viins '\egg' widget-find-repo-file

bindkey '^X^R' widget-find-snippet
