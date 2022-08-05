zstyle ':completion:*' completer _complete _ignored
zstyle :compinstall filename '/home/hydra/.zshrc'

autoload -Uz compinit
if [[ -n ${ZDOTDIR}/.zcompdump(#qN.mh+24) ]]; then
	compinit;
else
	compinit -C;
fi;
HISTFILE=~/.cache/.histfile
HISTSIZE=1000
SAVEHIST=1000
unsetopt beep
bindkey -v


zstyle ':omz:update' mode disabled

# aliases
alias clock='tty-clock -c -t'
alias reboot='sudo reboot'
alias unixporn='clear && neofetch'
alias rescan='nmcli device wifi rescan'
alias nixrebuild='sudo nixos-rebuild switch -I nixos-config=/home/hydra/s/dotfiles/nix/configuration.nix'
alias cleantrash='sudo sh /home/hydra/s/dotfiles/sh/empty_trash.sh'
alias quit='exit'
alias fountainpdf='find . -type f \( -name "*.fountain" \) -exec wrap pdf {} \;'
alias resemacs='systemctl --user restart emacs'
