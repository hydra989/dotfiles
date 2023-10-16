{ pkgs, ... }:
let
  tmux-super-fingers = pkgs.tmuxPlugins.mkTmuxPlugin
    {
      pluginName = "tmux-super-fingers";
      version = "unstable-2023-01-06";
      src = pkgs.fetchFromGitHub {
        owner = "artemave";
        repo = "tmux_super_fingers";
        rev = "2c12044984124e74e21a5a87d00f844083e4bdf7";
        sha256 = "sha256-cPZCV8xk9QpU49/7H8iGhQYK6JwWjviL29eWabuqruc=";
      };
    };
in
{
    programs.tmux = {
        enable = true;

        disableConfirmationPrompt = true;
        keyMode = "vi";
        prefix = "C-a";
        terminal = "alacritty";
        shell = "\{pkgs.zsh}/bin/zsh";

        plugins = [ 
            {
                plugin = tmux-super-fingers;
                extraConfig = "set -g @super-fingers-key f";
            }
        ];

        extraConfig = ''
            bind | split-window -h
            bind - split-wnindow -v

            bind -n M-Left select-pane -L
            bind -n M-Right select-pane -R
            bind -n M-Up select-pane -U
            bind -n M-Down select-pane -D

            bind b set-option status
        '';
    };
}
