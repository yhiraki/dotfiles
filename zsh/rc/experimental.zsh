# https://dev.classmethod.jp/tool/fzf-original-app-for-git-add/
function gadd() {
    local selected
    selected=$(git status -s | fzf -m | awk '{print $2}')
    if [[ -n "$selected" ]]
    then
        echo $selected | xargs git add
    fi
}
