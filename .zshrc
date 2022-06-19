# The following lines were added by compinstall

zstyle ':completion:*' completer _complete _ignored
zstyle :compinstall filename '/home/hydra/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall
# Lines configured by zsh-newuser-install
HISTFILE=~/.cache/.histfile
HISTSIZE=1000
SAVEHIST=1000
unsetopt beep
bindkey -v
# End of lines configured by zsh-newuser-install


zstyle ':omz:update' mode disabled

export EDITOR="vim"
export VISUAL="emacs"

# calibre
export CALIBRE_USE_DARK_PALETTE=1

# thefuck
eval "$(thefuck --alias)"

# aliases
alias clock='tty-clock -c -u -t'
alias reboot='sudo reboot'
alias unixporn='clear && neofetch'
alias rescan='nmcli device wifi rescan'
alias nixrebuild='sudo nixos-rebuild switch -I nixos-config=/home/hydra/s/dotfiles/nix/configuration.nix'
alias cleantrash='sudo sh /home/hydra/s/dotfiles/empty_trash.sh'
