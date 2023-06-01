{ config, inputs, ... }:
{
  programs.zsh = {
    enable = true;

    dotDir = ".config/zsh";

    initExtra = ''
      SOURCEDIR=$HOME/s
      (cat ${config.xdg.cacheHome}/wal/sequences &)
    '';

    history = {
      save = 1000;
      size = 1000;
      path = "$HOME/.cache/zsh_history";
    };

    enableAutosuggestions = true;
    enableSyntaxHighlighting = true;
    oh-my-zsh = {
      enable = true;
      plugins = [ "git" "tmux" ];
      theme = "ys";
      extraConfig = ''
        ZSH_TMUX_AUTOSTART=true
      '';
    };

    shellAliases = {
      clock = "tty-clock -c -t";
      reboot = "sudo reboot";
      unixporn = "clear && neofetch";
      rescan = "nmcli device wifi rescan";
      quit = "exit";
      resemacs = "systemctl --user restart emacs";
      dots = "cd ~/s/dotfiles";
      fountainpdf = "find . -type f \( -name \"*.fountain\" \) -exec wrap pdf {} \;";
			rebuild = "sudo nixos-rebuild switch";
    };
  };
}
