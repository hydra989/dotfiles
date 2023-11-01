{ ... }: {
    programs.foot = {
        enable = true;
        settings = {
            main = {
                font = "JetBrains Mono:size=11";
            };

            # oxocarbon
            colors = {
                foreground = "ffffff";
                background = "000000";
                regular0 = "000000"; # black
                regular1 = "ee5396"; # red
                regular2 = "42be65"; # green
                regular3 = "ffe97b"; # yellow
                regular4 = "33b1ff"; # blue
                regular5 = "ff7eb6"; # magenta
                regular6 = "3ddbd9"; # cyan
                regular7 = "dde1e6"; # white
                bright0 = "393939"; # bright black
                bright1 = "ee5396"; # bright red
                bright2 = "42be65"; # bright green
                bright3 = "ffe97b"; # bright yellow
                bright4 = "33b1ff"; # bright blue
                bright5 = "ff7eb6"; # bright magenta
                bright6 = "3ddbd9"; # bright cyan
                bright7 = "ffffff"; # bright white
            };
        };
    };
}
