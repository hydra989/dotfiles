# oh-my-bash stuff
export OSH=/home/hydra/.oh-my-bash
OSH_THEME="candy"
completions=(
  git
  gh
  hub
)
aliases=(
  general
)
plugins=(
  git
)
source $OSH/oh-my-bash.sh

# calibre
export CALIBRE_USE_DARK_PALETTE=1

# emacs
# export LSP_USE_PLISTS=true

# thefuck
eval "$(thefuck --alias)"

# aliases
alias clock='tty-clock -c -u -t'
alias reboot='sudo reboot'
