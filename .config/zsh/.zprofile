# anyenv
# https://qiita.com/luckypool/items/f1e756e9d3e9786ad9ea
if [ -d ${HOME}/.anyenv ]
then
    export PATH="$HOME/.anyenv/bin:$PATH"
    eval "$(anyenv init -  --no-rehash)"
    for D in `ls $HOME/.anyenv/envs`
    do
        export PATH="$HOME/.anyenv/envs/$D/shims:$PATH"
    done
fi
