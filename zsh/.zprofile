export SSH_AGENT_RC=/tmp/ssh-agent-rc

if [ ! -f $SSH_AGENT_RC ]; then
  ssh-agent >$SSH_AGENT_RC
  ssh-add
fi

source "$SSH_AGENT_RC"

if ! kill -s 0 "$SSH_AGENT_PID"; then
  ssh-agent >$SSH_AGENT_RC
  ssh-add
  source "$SSH_AGENT_RC"
fi

# iterm transparent title bar
# https://codematters.blog/custom-iterm2-titlebar-background-colors-a088c6f2ec60
echo -e "\033]6;1;bg;red;brightness;34\a"
echo -e "\033]6;1;bg;green;brightness;34\a"
echo -e "\033]6;1;bg;blue;brightness;34\a"

echo '.zprofile loaded'
