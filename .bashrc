export EDITOR="vim"
export VISUAL="emacs"

# oh-my-bash stuff
export OSH=/home/hydra/.oh-my-bash
OSH_THEME="90210"
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

# thefuck
eval "$(thefuck --alias)"

# aliases
alias clock='tty-clock -c -u -t'
alias reboot='sudo reboot'
alias unixporn='clear && neofetch'
