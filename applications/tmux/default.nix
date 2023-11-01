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
        mouse = true;
        shell = "${pkgs.zsh}/bin/zsh";

        plugins = [ 
            {
                plugin = tmux-super-fingers;
                extraConfig = "set -g @super-fingers-key f";
            }
        ];

        extraConfig = ''
            set -a terminal-overrides ",alacritty:RGB"

            bind | split-window -h
            bind - split-window -v
            unbind '"'
            unbind %

            bind -n M-Left select-pane -L
            bind -n M-Right select-pane -R
            bind -n M-Up select-pane -U
            bind -n M-Down select-pane -D

            # statusbar
            set-option -g status-style bg=default,fg=default
            #set-option -g status-justify centre
            set-option -g status-left '#[bg=default,fg=default,bold]#{?client_prefix,,  tmux  }#[bg=#698DDA,fg=black,bold]#{?client_prefix,  tmux  ,}'
            #set-option -g status-right '#S'
            set-option -g window-status-format ' #I:#W '
            set-option -g window-status-current-format '#[bg=#698DDA,fg=default] #I:#W#{?window_zoomed_flag,  , }'
        '';
    };
}
