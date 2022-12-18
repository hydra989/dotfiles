{ inputs, ... }:
{
  home.file."/.config/sxhkd/sxhkdrc" = {
    text = ''
      #
      # wm independent hotkeys
      #

      # terminal emulator
      alt + Return
          	alacritty

      # program launcher
      alt + space
          	rofi -show drun -show-icons

      # make sxhkd reload its configuration files:
      alt + Escape
          	pkill -USR1 -x sxhkd

      #
      # bspwm hotkeys
      #

      # quit/restart bspwm
      alt + shift + {q,r}
          	bspc {quit,wm -r}

      # close and kill
      alt + {_,shift + }w
          	bspc node -{c,k}

      # alternate between the tiled and monocle layout
      alt + m
          	bspc desktop -l next

      # send the newest marked node to the newest preselected node
      alt + y
            bspc node newest.marked.local -n newest.!automatic.local

      # swap the current node and the biggest window
      alt + g
          	bspc node -s biggest.window

      #
      # state/flags
      #

      # set the window state
      alt + {t,shift + t,s,f}
          	bspc node -t {tiled,pseudo_tiled,floating,fullscreen}

      # set the node flags
      alt + ctrl + {m,x,y,z}
          	bspc node -g {marked,locked,sticky,private}

      #
      # focus/swap
      #

      # focus the node in the given direction
      alt + {_,shift} + {{h,j,k,l}
          	bspc node -{f,s} {west,south,north,east}

      # focus the node for the given path jump
      alt + {p,b,comma,period}
          	bspc node -f @{parent,brother,first,second}

      # focus the next/previous window in the current desktop
      alt + {_,shift + }c
          	bspc node -f {next,prev}.local.!hidden.window

      # focus the next/previous desktop in the current monitor
      alt + bracket{left,right}
          	bspc desktop -f {prev,next}.local

      # focus the last node/desktop
      alt + {grave,Tab}
          	bspc {node,desktop} -f last

      # focus the older or newer node in the focus history
      alt + {o,i}
          	bspc wm -h off; \
          	bspc node {older,newer} -f; \
          	bspc wm -h on

      # focus or send to the given desktop
      alt + {_,shift + }{1-9,0}
          	bspc {desktop -f,node -d} '^{1-9,10}'

      #
      # preselect
      #

      # preselect the direction
      super + alt + {h,j,k,l}
          	bspc node -p {west,south,north,east}

      # preselect the ratio
      super + alt + {1-9}
          	bspc node -o 0.{1-9}

      # cancel the preselection for the focused node
      super + alt + space
          	bspc node -p cancel

      # cancel the preselection for the focused desktop
      super + alt + shift + space
          	bspc query -N -d | xargs -I id -n 1 bspc node id -p cancel

      #
      # move/resize
      #

      # expand a window by moving one of its side outward
      alt + {h,j,k,l}
          	bspc node -z {left -20 0,bottom 0 20,top 0 -20,right 20 0}

      # contract a window by moving one of its side inward
      alt + shift + {h,j,k,l}
          	bspc node -z {right -20 0,top 0 20,bottom 0 -20,left 20 0}

      # move a floating window
      super + {Left,Down,Up,Right}
          	bspc node -v {-20 0,0 20,0 -20,20 0}
    '';
  };
}
