if [ -f ~/.zshrc ]; then
  . ~/.zshrc
fi

# pyenv
export PYENV_ROOT=$HOME/.pyenv
if [ ! -d $PYENV_ROOT ]; then
  git clone https://github.com/yyuu/pyenv.git $PYENV_ROOT
fi
export PATH=$PYENV_ROOT/bin:$PATH
eval "$(pyenv init -)"

# pyenv-virtualenv
if [ ! -d $PYENV_ROOT/plugins/pyenv-virtualenv ]; then
  git clone https://github.com/yyuu/pyenv-virtualenv.git $PYENV_ROOT/plugins/pyenv-virtualenv
fi
eval "$(pyenv virtualenv-init -)"
