# oh-my-bash stuff
export OSH=/home/hydra/.oh-my-bash
OSH_THEME="candy"
completions=(
  git
  ssh
)
aliases=(
  general
)
plugins=(
  git
  bashmarks
)
source $OSH/oh-my-bash.sh

# calibre
export CALIBRE_USE_DARK_PALETTE=1

# pyenv
# export PATH="$HOME/.pyenv/bin:$PATH"
# eval "$(pyenv virtualenv-init -)"

# thefuck
eval "$(thefuck --alias)"

# aliases
alias clock='tty-clock -c -u -t'
alias reboot='sudo reboot'
