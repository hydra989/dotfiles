{ pkgs, ... }: {
    programs.firefox = {
        enable = true;
        package = pkgs.firefox-wayland;
        profiles.default = {
            id = 0;
            name = "Default";

            extensions = with pkgs.nur.repos.rycee.firefox-addons; [
                decentraleyes
                vimium
                ublock-origin
            ];

            settings = {
                # (partially) taken from: 
                # https://shen.hong.io/nixos-for-philosophy-installing-firefox-latex-vscodium/
                "dom.security.https_only_mode" = true;
                "dom.security.https_only_mode_ever_enabled" = true;

                "privacy.donottrackheader.enabled" = true;
                "privacy.trackingprotection.enabled" = true;
                "privacy.trackingprotection.socialtracking.enabled" = true;
                "privacy.partition.network_state.oscp_cache" = true;

                "experiments.activeExperiment" = false;
                "experiments.enabled" = false;
                "experiments.supported" = false;
                "network.allow-experiments" = false;

                "browser.newtabpage.activity-stream.feeds.telemetry" = false;
                "browser.newtabpage.activity-stream.telemetry" = false;
                "browser.ping-centre.telemetry" = false;
                "toolkit.telemetry.archive.enabled" = false;
                "toolkit.telemetry.bhrPing.enabled" = false;
                "toolkit.telemetry.enabled" = false;
                "toolkit.telemetry.firstShutdownPing.enabled" = false;
                "toolkit.telemetry.hybridContent.enabled" = false;
                "toolkit.newProfilePing.enabled" = false;
                "toolkit.telemetry.reportingpolicy.firstRun" = false;
                "toolkit.telemetry.shutdownPingSender.enabled" = false;
                "toolkit.telemetry.unified" = false;
                "toolkit.telemetry.updatePing.enabled" = false;

                "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
                "browser.tabs.searchclipboardfor.middleclick" = false;
                "toolkit.tabbox.switchByScrolling" = true;

                "browser.urlbar.suggest.pocket" = false;
                "extensions.pocket.showHome" = false;
                "browser.newtabpage.activity-stream.section.highlights.includePocket" = false;

                # get rid of "sponsored" links as best as possible
                "browser.newtabpage.activity-stream.showSponsored" = false;
                "browser.newtabpage.activity-stream.showSponsoredTopSites" = false;
                "browser.urlbar.suggest.quicksuggest.sponsored" = false;
            };

            # user chrome copied from:
            # https://github.com/Bali10050/FirefoxCSS

            userChrome = ''
                :root{--WindowDraggingFromNewtabButton: no-drag}  /*  For windows  *//*
                :root{--WindowDraggingFromNewtabButton: drag !important}  /* For linux */

                /*  #reload-button, #stop-button{display:none !important}  /*  Removes the reload/stop button  */

                /*  #back-button, #forward-button{display: none !important}  /*  Removes the forward/back button  */

                /*  .tab-close-button{display: none !important}  /*  Removes the x-button from the tabs  */

                /*  #tabs-newtab-button, #new-tab-button{opacity: 0 !important}  /*  Hide newtab button  */

                #navigator-toolbox{background-image: url("./Background.png"), url("./NavbarBackground.png")}  /*  Custom background -- Also check userContent.css  */

                :root{
                --tab-border-radius: 3px !important; /*  Tab border radius -- Changes the tabs rounding  *//*  Default: 3px  */
                --NavbarWidth: 43; /*  Default values: 36 - 43  */
                --TabsHeight: 36; /*  Minimum: 30  *//*  Default: 36  */
                --TabsBorder: 8; /*  Doesnt do anything on small layout  *//*  Default: 8  */
                --NavbarHeightSmall: calc(var(--TabsHeight) + var(--TabsBorder))  /*  Only on small layout  *//*  Default: calc(var(--TabsHeight) + var(--TabsBorder))  *//*  Default as a number: 44  */}

                @media screen and (min-width:1325px)    /*  Only the tabs space will grow from here  */
                {:root #nav-bar{margin-top: calc(var(--TabsHeight) * -1px - var(--TabsBorder) * 1px)!important; height: calc(var(--TabsHeight) * 1px + var(--TabsBorder) * 1px)} #TabsToolbar{margin-left: calc(1325px / 100 * var(--NavbarWidth)) !important} #nav-bar{margin-right: calc(100vw - calc(1325px / 100 * var(--NavbarWidth))) !important; vertical-align: center !important} #urlbar-container{min-width: 0px !important;  flex: auto !important} toolbarspring{display: none !important}}

                @media screen and (min-width:850px) and (max-width:1324px)    /*  Both the tabs space and the navbar will grow  */
                {:root #nav-bar{margin-top: calc(var(--TabsHeight) * -1px - var(--TabsBorder) * 1px) !important; height: calc(var(--TabsHeight) * 1px + var(--TabsBorder) * 1px)} #TabsToolbar{margin-left: calc(var(--NavbarWidth) * 1vw) !important} #nav-bar{margin-right: calc(100vw - calc(var(--NavbarWidth) * 1vw)) !important; vertical-align: center !important} #urlbar-container{min-width: 0px !important;  flex: auto !important} toolbarspring{display: none !important} #TabsToolbar, #nav-bar{transition: margin-top .25s !important}}

                @media screen and (max-width:849px)    /*  The window is not enough wide for a one line layout  */
                {:root #nav-bar{padding: 0 5px 0 5px!important; height: calc(var(--NavbarHeightSmall) * 1px) !important} toolbarspring{display: none !important;} #TabsToolbar, #nav-bar{transition: margin-top .25s !important}}
                #nav-bar, #PersonalToolbar{background-color: #0000 !important;background-image: none !important; box-shadow: none !important} #nav-bar{margin-left: 3px;} .tab-background, .tab-stack { min-height: calc(var(--TabsHeight) * 1px) !important}

                /*  Removes rounding from the top corners,   */
                #navigator-toolbox{appearance: none !important; border-radius: 0 !important}
                #navigator-toolbox-background{background-color: Field !important; background-image: none !important;  appearance: none !important; border-radius: 0 !important}

                #navigator-toolbox-background:-moz-window-inactive{filter: contrast(90%);}  /*  Fades window while not in focus */
                :root{--tabpanel-background-color: var(--toolbar-field-background-color) !important}  /*  Removes flash before loading pages  */

                /*  Removes annoying buttons and spaces */
                #firefox-view-button, #save-to-pocket-button, #tracking-protection-icon-container, .titlebar-spacer[type="pre-tabs"], .titlebar-spacer[type="post-tabs"]{display: none !important}
                #tabbrowser-tabs{border-inline-start-width: 0!important}

                /*  Makes some buttons nicer  */
                #PanelUI-menu-button, #unified-extensions-button, #reload-button, #stop-button {padding: 2px !important}
                #reload-button, #stop-button{margin: 1px !important;}

                /* Newtab hack */
                #tabs-newtab-button, #new-tab-button{transition: .3s !important;color: transparent !important; fill: FieldText !important; background: none !important; opacity: 20%; padding-right: 20px  !important; padding-left: 4px !important; -moz-window-dragging: var(--WindowDraggingFromNewtabButton)}
                #tabs-newtab-button:hover, #new-tab-button:hover{transition: .3s !important; color: transparent !important; fill: FieldText !important; opacity: 90%; padding-right: 20px !important;  background: none !important; padding-left: 4px !important; -moz-window-dragging: var(--WindowDraggingFromNewtabButton)}
                #tabs-newtab-button > .toolbarbutton-icon , #new-tab-button > .toolbarbutton-icon{ background: none !important;}

                /*  Removes urlbar border/background  */
                #urlbar-background{border: none !important; outline: none !important; transition: .15s !important;}

                /*  Cool animation on tab/bookmark icons  */
                .tabbrowser-tab:not([pinned]):not([selected]) .tab-icon-image ,.bookmark-item .toolbarbutton-icon{opacity: 0!important; transition: .15s !important; width: 0!important; padding-left: 16px!important}
                .tabbrowser-tab:not([pinned]):hover .tab-icon-image,.bookmark-item:hover .toolbarbutton-icon{opacity: 100!important; transition: .15s !important; display: inline-block!important; width: 16px!important; padding-left: 0!important}
                .tabbrowser-tab:not([hover]) .tab-icon-image,.bookmark-item:not([hover]) .toolbarbutton-icon{padding-left: 0!important}

                /*  Removes space after pinned tabs  */
                #tabbrowser-tabs[haspinnedtabs]:not([positionpinnedtabs])>#tabbrowser-arrowscrollbox>.tabbrowser-tab[first-visible-unpinned-tab] {margin-inline-start: 0!important}

                /*  X-button on the tabs  */
                .tabbrowser-tab:not(:hover) .tab-close-button{opacity: 0% !important; transition: .3s !important; display: -moz-box !important;}
                .tab-close-button[selected]:not(:hover){opacity: 45% !important; transition: .3s !important; display: -moz-box !important;}
                .tabbrowser-tab:hover .tab-close-button{opacity: 50%; transition: .3s !important; background: none !important; cursor: pointer; display: -moz-box !important;}
                .tab-close-button:hover{opacity: 100% !important; transition: .3s !important; background: none !important; cursor: pointer; display: -moz-box !important;}
                .tab-close-button[selected]:hover{opacity: 100% !important; transition: .3s !important; background: none !important; cursor: pointer; display: -moz-box !important;}

                /*  Removes annoying border  */
                #navigator-toolbox{border:none !important;}

                /*  Titlebar button fix#6322  */
                .titlebar-buttonbox-container{-moz-box-ordinal-group: auto;}
                .titlebar-button > .toolbarbutton-icon {display: flex !important; height:unset !important; width: unset !important;}
                .titlebar-buttonbox{appearance: none !important}

                /*  Titlebar buttons  -  Minimize button  */
                .titlebar-min {list-style-image: url("data:image/svg+xml,%3Csvg width='40' height='30' version='1.1' viewBox='0 0 40 30' xmlns='http://www.w3.org/2000/svg'%3E%3Ctitle%3EVetro%3C/title%3E%3Crect transform='matrix(3.7795 0 0 3.7795 -39.998 -533.51)' x='14.552' y='145.13' width='2.6458' height='.26458' ry='3.1658e-6' fill='currentColor' style='paint-order:stroke fill markers'/%3E%3C/svg%3E%0A")!important;  border-radius: 0!important; margin: 0!important; padding: 0!important; border: 0!important; height: 100%!important; align-content: center!important; width: 56px!important; appearance: none!important}.titlebar-min:hover {background-color: color-mix(in srgb,currentColor 17%,transparent)!important}.titlebar-min:active {background-color: color-mix(in srgb,currentColor 30%,transparent)!important}

                /*  Titlebar buttons  -  Maximize button  */
                .titlebar-max {list-style-image: url("data:image/svg+xml,%3Csvg width='40' height='30' version='1.1' viewBox='0 0 40 30' xmlns='http://www.w3.org/2000/svg'%3E%3Ctitle%3EVetro%3C/title%3E%3Cpath transform='matrix(3.7795 0 0 3.7795 -101 -533.51)' d='m30.692 143.81v2.6458h2.6458v-2.6458zm0.26458 0.26459h2.1167v2.1167h-2.1167z' fill='currentColor' style='paint-order:stroke fill markers'/%3E%3C/svg%3E")!important;  border-radius: 0!important; margin: 0!important; padding: 0!important; border: 0!important; height: 100%!important; align-content: center!important; width: 56px!important; appearance: none!important}.titlebar-max:hover {background-color: color-mix(in srgb,currentColor 17%,transparent)!important}.titlebar-max:active {background-color: color-mix(in srgb,currentColor 30%,transparent)!important}

                /*  Titlebar buttons  -  Restore button  */
                .titlebar-restore {list-style-image: url("data:image/svg+xml,%3Csvg width='40' height='30' version='1.1' viewBox='0 0 40 30' xmlns='http://www.w3.org/2000/svg'%3E%3Ctitle%3EVetro%3C/title%3E%3Cpath d='m17.001 10.016v2h-2v8.0001h8.0001v-2h2v-8.0001zm0.99998 1h6v6h-0.99998v-4.9999h-4.9999zm-2 2h6v6h-6z' fill='currentColor' stroke-width='3.7795' style='paint-order:stroke fill markers'/%3E%3C/svg%3E%0A")!important;  border-radius: 0!important; margin: 0!important; padding: 0!important; border: 0!important; height: 100%!important; width: 56px!important; appearance: none!important}.titlebar-restore:hover {background-color: color-mix(in srgb,currentColor 17%,transparent)!important}.titlebar-restore:active {background-color: color-mix(in srgb,currentColor 30%,transparent)!important}

                /*  Titlebar buttons  -  Close button  */
                .titlebar-close:not(:hover, :active) {list-style-image: url("data:image/svg+xml,%3Csvg width='40' height='30' version='1.1' viewBox='0 0 40 30' xmlns='http://www.w3.org/2000/svg'%3E%3Ctitle%3EVetro%3C/title%3E%3Cpath transform='matrix(3.7796 0 0 3.7795 -165 -533.51)' d='m47.718 143.71-0.18707 0.18706 1.2294 1.2294-1.2294 1.2289 0.18707 0.18707 1.2294-1.2289 1.2289 1.2289 0.18707-0.18707-1.2289-1.2289 1.2289-1.2294-0.18707-0.18706-1.2289 1.2294z' fill='currentColor' style='paint-order:stroke fill markers'/%3E%3C/svg%3E%0A")!important; margin: 0 !important; padding: 0!important; border: 0!important; border-radius: 0!important; height: 100%!important; width: 56px!important; appearance: none!important}
                .titlebar-close:hover {background-color: #f44!important; list-style-image: url("data:image/svg+xml,%3Csvg width='40' height='30' version='1.1' viewBox='0 0 40 30' xmlns='http://www.w3.org/2000/svg'%3E%3Ctitle%3EVetro%3C/title%3E%3Cpath transform='matrix(3.7796 0 0 3.7795 -165 -533.51)' d='m47.718 143.71-0.18707 0.18706 1.2294 1.2294-1.2294 1.2289 0.18707 0.18707 1.2294-1.2289 1.2289 1.2289 0.18707-0.18707-1.2289-1.2289 1.2289-1.2294-0.18707-0.18706-1.2289 1.2294z' fill='currentColor' style='paint-order:stroke fill markers;fill:%23ffffff' /%3E%3C/svg%3E%0A")!important;  margin: 0 !important; padding: 0!important; border: 0!important; border-radius: 0!important; height: 100%!important; width: 56px!important; appearance: none!important}
                .titlebar-close:active {background-color: #f33!important;  list-style-image: url("data:image/svg+xml,%3Csvg width='40' height='30' version='1.1' viewBox='0 0 40 30' xmlns='http://www.w3.org/2000/svg'%3E%3Ctitle%3EVetro%3C/title%3E%3Cpath transform='matrix(3.7796 0 0 3.7795 -165 -533.51)' d='m47.718 143.71-0.18707 0.18706 1.2294 1.2294-1.2294 1.2289 0.18707 0.18707 1.2294-1.2289 1.2289 1.2289 0.18707-0.18707-1.2289-1.2289 1.2289-1.2294-0.18707-0.18706-1.2289 1.2294z' fill='currentColor' style='paint-order:stroke fill markers;fill:%23ffffff' /%3E%3C/svg%3E%0A")!important;  margin: 0 !important; padding: 0!important; border: 0!important; border-radius: 0!important; height: 100%!important; width: 56px!important; appearance: none!important}

                /*  Alltabs button  */
                #TabsToolbar-customization-target {counter-reset: tabCount}.tabbrowser-tab {counter-increment: tabCount}
                #alltabs-button>.toolbarbutton-badge-stack>.toolbarbutton-icon {list-style-image: url("data:image/svg+xml,%3Csvg width='40' height='30' version='1.1' viewBox='0 0 40 30' xmlns='http://www.w3.org/2000/svg'%3E%3Ctitle%3EVetro%3C/title%3E%3Cpath transform='translate(49,-60)' d='m-29 78.888-7.0703-7.0703 0.70703-0.70703 6.3633 6.3633 6.3633-6.3633 0.70703 0.70703-6.3633 6.3633z' fill='currentColor' style='paint-order:stroke fill markers'/%3E%3C/svg%3E"); overflow: hidden!important; padding: 0!important; border: 0!important; width: 40px!important; height: calc(100% + 1px)!important; margin: 0 -2px 0 0!important; transform: translate(20%,15%); padding: 0 3px}
                #alltabs-button>.toolbarbutton-badge-stack {position: relative!important; border-radius: 0!important; padding: 0!important; border: 0!important; height: calc(100% + 1px)!important; width: 56px!important; margin: 0-2px 0 0!important}
                #alltabs-button>.toolbarbutton-badge-stack::before {content: counter(tabCount); filter:contrast(500%)grayscale(100%); color: -moz-DialogText !important; position: absolute; bottom: 25%; left: 50%; transform: translate(-50%,-30%); padding: 0 3px}

                /*  Removes the background from the urlbar while not in use  */
                #urlbar:not(:hover):not([breakout][breakout-extend])>#urlbar-background {box-shadow: none!important; background: #0000 !important}

                /*  Urlbar popup thing  */
                .urlbarView-row{display: none !important}
                .urlbarView-row:nth-child(1), .urlbarView-row:nth-child(2){display: flex !important}
                .search-panel-one-offs-header.search-panel-header{display: none !important}
                #urlbar-anon-search-settings{margin-inline-start: 0px !important}

                /*  Makes the space between the reload button and the urlbar less akwardly large  */
                #urlbar-container{margin-left: 1px !important}

                /*  Makes the sidebar a little nicer  */
                #sidebar-splitter{display: none}
                #sidebar-header{padding-bottom: 0!important; border-bottom: 0!important}

                /*  The thing in the bottom with links  */
                #statuspanel-label{background-color: var(--toolbar-field-background-color)!important; border-color: transparent!important; border-radius: 5px !important; color: currentColor!important; margin: 0 0 2.5px 2px!important; padding: 5px!important; opacity: 100%;}

                /*  Removes the annoying rainbow thing from the hamburger  */
                #appMenu-fxa-separator{border-image:none !important;}

                /*  Navbar buttons order  */
                #nav-bar{direction: rtl;} #urlbar-container, #customizationui-widget-panel{direction: ltr !important;}
                #forward-button{order: -1 !important;}
                #back-button{padding-right: 2px !important;} #forward-button{padding-right: 8px !important;}

                /*  Rounded corner in pages  */
                #navigator-toolbox{padding-bottom: 10px;}
                #navigator-toolbox[style*="margin-top: -54px;"]{margin-top: -44px !important}
                .browserStack>browser{margin: -10px 0 0 0!important; border-radius: 10px 10px 0 0; transition: border-radius .2s !important}
                .browserStack:nth-last-child(3n)>browser, .browserStack:nth-last-child(4n)>browser{margin: -10px 0 !important; border-radius: 10px !important; transition: border-radius .2s !important}
                findbar{border-radius: 10px 10px 0 0 !important; margin-top: -10px; z-index: 2; padding-bottom: -10px;}
                [inFullscreen] .browserStack>browser{margin: -10px 0 0 0!important; border-radius: 0 !important; transition: border-radius .7s !important}
                [inFullscreen] .browserStack:nth-last-child(3n)>browser, .browserStack:nth-last-child(4n)>browser{margin: -10px !important; transition: border-radius .7s !important}

                /*  Devtools  */
                .devtools-horizontal-splitter{margin-top: 7px !important;margin-bottom:-12px !important;opacity: 0;}
                .devtools-side-splitter{opacity: 0;}
                .devtools-toolbox-side-iframe{margin-top: -10px; border-radius: 10px;}

                #customization-panelWrapper {max-width: 30em!important}  /*  Fixes the customization page  */
                #customization-container > *{margin-top: -10px; background-color: var(--toolbar-field-background-color) !important}  /*  Customizes the customization page  */

                #urlbar:not([open]){color: currentColor!important} #urlbar[open]{color: var(--toolbar-field-focus-color)}  /*  Fixes the urlbar color on some themes  */

                /*  Makes sidebars nicer  */
                #sidebar-box{margin-top: -10px !important; border-radius: 0 10px 0 0!important; z-index: 1}
                #sidebar-box[positionend="true"]{border-radius: 10px 0 0 0!important}
                #sidebar-box > *{background-image:none;}

                /*  Drop menus  */
                [position="bottomright topright"]{--panel-border-radius: 0 5px 5px 5px!important}
                [position="bottomleft topleft"]{--panel-border-radius: 5px 0 5px 5px!important}
                [part="content"]{border: none !important;}
                .toolbarbutton-1[open]{--toolbarbutton-border-radius: 4px 4px 0 0!important;}

                [class*="identity-color"] > * > * .tab-context-line{border-radius: 0 0 5px 5px !important}  /*  Identity-color  */


                /*  Private tabs  */
                [privatebrowsingnewindicator=""] #PanelUI-menu-button > .toolbarbutton-badge-stack{
                list-style-image: url("data:image/svg+xml,%3C%3Fxml version='1.0' encoding='UTF-8' standalone='no'%3F%3E%3C!-- This Source Code Form is subject to the terms of the Mozilla Public - License, v. 2.0. If a copy of the MPL was not distributed with this - file, You can obtain one at http://mozilla.org/MPL/2.0/. --%3E%3Csvg viewBox='0 0 20 20' width='20' height='20' version='1.1' id='svg10' xmlns='http://www.w3.org/2000/svg' xmlns:svg='http://www.w3.org/2000/svg'%3E%3Cdefs id='defs14' /%3E%3Cpath id='path4' d='M 4.4607598,4.7745347 C 2.8049552,4.7515627 1.1522018,5.4691193 0.08042091,6.8761599 -0.16625285,7.8508546 0.21338341,11.450255 0.42938959,12.016939 0.86273544,14.303671 2.732398,16.02487 4.9737961,16.02487 c 1.1186988,0 2.1336268,-0.446805 2.9349832,-1.161493 L 8.395773,14.444094 a 2.4174054,2.4174054 0 0 1 3.138115,-0.02344 l 0.734397,0.606788 v -0.0026 c 0.772021,0.621352 1.717989,1.002634 2.752686,1.002634 2.240064,0 4.109726,-1.721199 4.544406,-4.007931 C 19.78005,11.450192 20.174354,7.8668551 19.914346,6.8761599 18.008957,4.3747545 14.266945,4.0537862 11.937545,6.1938479 L 10.635423,7.3918003 H 9.3593434 L 8.0572214,6.1938479 C 7.0375252,5.2575709 5.7486078,4.7924021 4.4607598,4.7745347 Z m 1.0390935,3.6954225 c 0.9293602,0 1.7557821,0.389533 2.2891309,0.992217 a 0.86669252,0.86669252 0 0 1 0,1.1302418 c -0.5333488,0.602685 -1.3597707,0.989614 -2.2891309,0.989614 -0.9293601,0 -1.7557821,-0.386929 -2.2891308,-0.989614 a 0.86669252,0.86669252 0 0 1 0,-1.1302418 c 0.5333487,-0.602684 1.3597707,-0.992217 2.2891308,-0.992217 z m 9.0002687,0 c 0.929359,0 1.755781,0.389533 2.289131,0.992217 a 0.86669252,0.86669252 0 0 1 0,1.1302418 c -0.53335,0.602685 -1.359772,0.989614 -2.289131,0.989614 -0.92936,0 -1.755782,-0.386929 -2.289131,-0.989614 a 0.86669252,0.86669252 0 0 1 0,-1.1302418 c 0.533349,-0.602684 1.359771,-0.992217 2.289131,-0.992217 z' style='fill:currentColor;stroke-width:1.33337' /%3E%3C/svg%3E%0A");}
                #private-browsing-indicator-with-label{display: none}

                /*  Notification  */
                #tab-notification-deck .container.infobar{background: -moz-Dialog !important}
                #tab-notification-deck .notificationbox-stack{background: transparent !important}

                /*  Customization navbar fix  */
                #wrapper-urlbar-container{width: 100px !important}
                [title="Flexible Space"]{display: none !important}

                /*  Fullscreen thing  */
                #fullscreen-warning{border: none !important; background: -moz-Dialog !important}
            '';

            userContent = ''
                @-moz-document url("about:newtab"), url("about:home"){.outer-wrapper.ds-outer-wrapper-breakpoint-override.only-search.visible-logo{background-image: url("./Background.png"), url("./NewtabBackground.png"); background-position: 0 -44px, 0 0 !important;}}/*  Custom background -- Also check userChrome.css  */

                @-moz-document url-prefix(about:){
                /*  Colors  */
                :root{--newtab-background-color: -moz-Dialog !important;
                --in-content-background-color: -moz-Dialog !important;
                --in-content-page-background: -moz-Dialog !important;
                --in-content-table-background: -moz-Dialog !important;
                --in-content-box-background-odd: -moz-Dialog !important;
                --card-background-color: color-mix(in srgb, currentColor 5%, -moz-Dialog) !important;
                --in-content-box-background: color-mix(in srgb, currentColor 5%, -moz-Dialog) !important;
                --in-content-button-background: color-mix(in srgb, currentColor 5%, -moz-Dialog) !important;
                --in-content-box-background: color-mix(in srgb, currentColor 13%, -moz-Dialog) !important;
                --in-content-box-info-background: color-mix(in srgb, currentColor 13%, -moz-Dialog) !important;
                --message-bar-text-color: color-mix(in srgb, currentColor 13%, -moz-Dialog) !important;
                --newtab-background-color-secondary: color-mix(in srgb, currentColor 13%, -moz-Dialog) !important;
                --in-content-button-background-hover: color-mix(in srgb, currentColor 13%, -moz-Dialog) !important;
                --box-background: color-mix(in srgb, currentColor 13%, -moz-Dialog) !important;
                --button-background-color: color-mix(in srgb, currentColor 13%, -moz-Dialog) !important;
                --button-hover-background-color: color-mix(in srgb, currentColor 15%, -moz-Dialog) !important;}
                body{background-color: -moz-Dialog !important} @-moz-document url(about:blank){body{background-color: #eee !important} @media (prefers-color-scheme: dark){body{background-color: #222 !important}}}

                /*  Removes the scrollbar on some places  */
                body,html{overflow-y: auto}

                /*  Newtab  */
                @-moz-document url("about:home"), url("about:newtab"){
                .search-wrapper .search-handoff-button .fake-caret {top: 13px !important; inset-inline-start: 48px !important}
                .search-wrapper .logo-and-wordmark{opacity: 0.9 !important; order: 1 !important; margin-bottom: 0 !important; flex: 1 !important; flex-basis: 20% !important}
                .search-wrapper .search-handoff-button .fake-caret{top: 13px !important; inset-inline-start: 48px !important}
                .search-wrapper .logo-and-wordmark{opacity: 0.9 !important; order: 1 !important; margin-bottom: 0 !important; flex: 1 !important; flex-basis: 20% !important}
                .outer-wrapper .search-wrapper{padding: 0px !important; display: flex !important; flex-direction: row !important; flex-wrap: wrap !important; justify-content: center !important; align-items: center !important; align-content: space-around !important; gap: 20px 10px !important}
                .search-wrapper .logo-and-wordmark .logo{background-size: 60px !important; height: 60px !important; width: 60px !important}
                .search-wrapper .search-inner-wrapper{min-height: 42px !important; order: 2 !important; flex: 3 !important; flex-basis: 60% !important; top: 4px !important}
                .search-wrapper .search-inner-wrapper{min-height: 42px !important; order: 2 !important; flex: 3 !important; flex-basis: 60% !important; top: 4px !important}
                .outer-wrapper.ds-outer-wrapper-breakpoint-override.only-search.visible-logo{display: flex !important; padding-top: 0px !important;vertical-align: middle; }
                .customize-menu{border-radius: 10px 0 0 10px !important}
                #root > div{align-items: center; display: flex}}}

                @-moz-document url("about:privatebrowsing"){
                #search-handoff-button{border-radius: 10px !important; background-color: color-mix(in srgb, FieldText 13%, -moz-Dialog) !important; border: none !important; box-shadow: 0 2px 6px rgba(0, 0, 0, 0.15) !important;}
                .fake-textbox{color: FieldText !important;}
                .search-inner-wrapper{min-width: 0 !important; flex-grow: 2 !important; max-width: 720px; height: 42px !important;}
                .showPrivate.showSearch.container{padding: 0px !important; display: flex !important; flex-direction: row !important; flex-wrap: wrap !important; justify-content: center !important; align-items: center !important; align-content: space-around !important}
                .logo-and-wordmark .logo{background-size: 60px !important; height: 60px !important; width: 60px !important}
                .logo-and-wordmark{max-width: fit-content !important; max-height: 82px !important; opacity: 0.9 !important; margin-bottom: 0 !important; flex: 1 !important; flex-basis: 20% !important}
                .showPrivate.showSearch.container{display: flex !important; padding-top: 0px !important;vertical-align: middle; flex-direction: row; align-items: center; max-width: unset !important; width: -moz-available !important; gap: 20px 0px !important}
                .customize-menu{border-radius: 10px 0 0 10px !important}
                #root > div{align-items: center; display: flex}
                .info{display: none}
                .wordmark{background-size: 134px !important; margin-inline-start: 0 !important; fill: FieldText !important;}
                }


                @-moz-document url-prefix(about:devtools), url-prefix(chrome:){
                /*  Devtools tabs  */
                .devtools-tab-line{border-radius: 0 0 5px 5px !important; height: 2px !important}
                #toolbox-container{margin-top: 10px !important; background: -moz-Dialog !important}  /*  Makes rounded corners with userchrome look better  */
                .devtools-tabbar{background: -moz-Dialog !important; border: none !important}
                #toolbox-container .customize-animate-enter-done, #toolbox-container .customize-menu, #toolbox-container .top-site-outer:hover, #toolbox-container button{background-color: -moz-Dialog!important}
                .devtools-toolbar{height: 35px !important; border: none !important; background-color: -moz-Dialog !important}  /*  Has effect on all toolbars  */
                .toolbox-panel-iframe{border-radius: 8px 8px 0 0 !important}
                #toolbox-toolbar-mount{ background: -moz-Dialog !important;}
                .toolbox-panel.theme-toolbar{ background: -moz-Dialog !important;}
                /*  Debugger profile  */
                .chrome-debug-toolbar{ margin: -5px 5px 5px 5px !important; border: none !important; border-radius: 10px !important; background: color-mix(in srgb, currentColor 5%, -moz-Dialog) !important; padding-right: 5px !important; }
                .chrome-debug-toolbar__modes > label{ border: none !important;  background: color-mix(in srgb, currentColor 5%, -moz-Dialog) !important; }
                .chrome-debug-toolbar__modes input, .chrome-debug-toolbar__modes .mode__sublabel{display: none !important}
                .chrome-debug-toolbar__modes .selected{background: color-mix(in srgb, currentColor 3%, -moz-Dialog) !important}


                .tooltip-panel{ border-radius: 10px !important; border: none; background: color-mix(in srgb, currentColor 13%, -moz-Dialog) !important; }
                .tooltip-arrow{display: none !important}
                .command{  border-radius: 8px !important; margin: 0 10px; width: 90%;}}

                @-moz-document url-prefix(chrome:){




                /*  Notification fix*/
                #toolbox-notificationbox  .notification{ background: color-mix(in srgb, #000 20%, -moz-Dialog) !important; border: none !important; padding: 3px !important; }

                /*  Checkbox background fix  */
                .accordion input:not([type="checkbox"]) {border: none !important}
                #browser-style-checkbox:not(:checked):hover, #devtools-cache-checkbox:not(:checked):hover, #record-allocation-stacks-checkbox:not(:checked):hover, .accordion input[type="checkbox"]:not(:checked):hover, #devtools-display-tabbing-order-checkbox:not(:checked):hover{appearance: Dialog !important; border: 2px solid currentColor !important; border-radius: 2px !important;background-color: color-mix(in srgb, currentColor 13%, -moz-Dialog) !important}
                #browser-style-checkbox:not(:checked), #devtools-cache-checkbox:not(:checked), #record-allocation-stacks-checkbox:not(:checked), .accordion input[type="checkbox"]:not(:checked), #devtools-display-tabbing-order-checkbox:not(:checked){appearance: Dialog !important; border: 2px solid currentColor !important; border-radius: 2px !important}

                :root{scrollbar-color: color-mix(in srgb, FieldText 35%, -moz-Dialog) transparent !important;}  /*  Removes scrollbar backround  */

                /*  Inspector  */
                #markup-box{background-color: -moz-Dialog !important}
                #markup-box iframe{border-radius: 15px !important;background-color: color-mix(in srgb, currentColor 5%, -moz-Dialog) !important; margin-left: 10px !important; width: 98.65% !important}
                .tag-state{border-radius: 6px !important; width: auto !important; left: -16px !important; right: 10px !important;}
                .tag-hover{background: color-mix(in srgb, #000 20%, -moz-Dialog) !important; z-index: 0 !important; opacity: 30%}
                html body.theme-body.devtools-monospace div#root-wrapper div#root{ padding: 6px 0 6px 10px !important; }
                #inspector-breadcrumbs{top: auto; !important}
                #inspector-breadcrumbs-toolbar{height: 24px !important;}
                #inspector-eyedropper-toggle{margin-right: 2px !important;}
                .theme-body{background: transparent !important}
                #ruleview-toolbar{ flex-wrap: nowrap !important; }
                #sidebar-panel-changes{ border-radius: 10px !important; background: color-mix(in srgb, #000 20%, -moz-Dialog) !important; }
                .computed-property-view{ background: transparent !important;}
                .computed-property-view.row-striped{background-color: color-mix(in srgb, currentColor 5%, -moz-Dialog) !important;}
                #computed-property-container{background-color: color-mix(in srgb, #000 20%, -moz-Dialog) !important}
                #computed-container-focusable, #sidebar-panel-computedview{background: -moz-Dialog !important}
                #computed-container{border-radius: 10px !important;}

                /*  Console  */
                .webconsole-filteringbar-wrapper.wide{margin-bottom: 8px !important;}
                .tabs-menu-item{background-color: transparent !important}
                .message{margin: 0 1px 2px 1px !important; border-radius: 10px !important; padding: 5px !important; border-bottom: -6px !important; width: 98% !important; left: 50%; transform: translate(-50%, 0);}
                .jsterm-input-container{border-radius: 8px 8px 0 0 !important; border: none !important}
                .webconsole-app{background-color: -moz-Dialog !important}
                #mount .sidebar-item{border-radius: 10px !important; padding: 10px 0 10px 5px !important; margin-bottom: 4px !important;}
                #mount .js-sidebar{padding: 0 5px 0px 7px !important; background: -moz-Dialog !important; border: none !important}
                #mount > main{background: -moz-Dialog;}
                #mount .app-page{border-radius: 10px !important; background-color: color-mix(in srgb, currentColor 5%, -moz-Dialog) !important;}
                html#devtools-webconsole body.theme-sidebar main#app-wrapper.theme-body div.webconsole-app.jsterm-editor.eager-evaluation div.flexible-output-input div.jsterm-input-container.devtools-input{ margin-top: 8px !important; }

                /*  Input/searchboxes  */
                .devtools-filterinput{ background-color: -moz-Dialog !important; vertical-align: center !important; border-radius: 8px !important; }
                .sources-panel .devtools-filterinput{background: color-mix(in srgb, currentColor 5%, -moz-Dialog)  !important;}
                #font-preview-input-container { border: none !important; background: -moz-Dialog !important;  border-radius: 8px !important;  height: auto !important; }
                #font-preview-input-container .devtools-searchinput{ background: none !important; }
                .font-value-input{ border-radius: 10px !important; border: none !important; padding: 3px 10px 3px 3px !important; }
                #font-preview-input-container .devtools-searchinput{ height: 31px !important; border-radius: 10px !important; margin: -8px 0 -8px 0; }
                .input-expression, .xhr-input-url, .event-search-input{background:  color-mix(in srgb, currentColor 3%, -moz-Dialog) !important; appearance: none !important;  margin: 5px 2px 5px 8px !important; border-radius: 8px !important; width: 97% !important;}
                .exclude-patterns-field.small input{ border-radius: 8px !important; border: none !important; padding: 18px 5px 18px 10px !important; background-color: color-mix(in srgb, currentColor 5%, -moz-Dialog) !important; appearance: inherit !important; }
                .devtools-searchbox{height: auto !important; border-radius: 8px !important;}

                /*  Font editor  */
                #font-editor{ background: -moz-Dialog !important; }
                #font-container > .accordion{border: none !important}
                .font-value-select{ background: auto !important; padding: 4px !important; border-radius: 3px !important; border: none !important; text-align: center; background-image: none !important }
                #font-container{ background: -moz-Dialog !important; }

                /*  Debugger  */
                .source-outline-panel .tree-node{border-radius: 8px !important; padding: 5px !important; width: 96% !important; margin: 0 0 2px 2px}
                .perf-onboarding{display: none !important}
                .source-outline-tabs .tab::before, .source-tab::before{border-radius: 0 0 2px 2px !important; background-image: none !important}
                .source-outline-tabs .tab:not(.active){background: color-mix(in srgb, currentColor 5%, -moz-Dialog)  !important; width: 90%; color: inherit !important}
                .source-tab:hover{background: transparent !important;}
                .source-tab:not(.active) > .img{display: none !important}
                .exclude-patterns-field.small{background: -moz-Dialog !important; border: none !important}
                .exclude-patterns-field.small > *{background-color: color-mix(in srgb, currentColor 5%, -moz-Dialog) !important;}
                .project-text-search{margin-top: 9px !important;}
                .project-text-search .search-outline{ width: 94%; margin-left: 8px; border: none !important; }
                .project-text-search .search-outline .search-field.small{ background: color-mix(in srgb, currentColor 5%, -moz-Dialog) !important; padding: 5px; border-radius: 8px; border: none !important; margin-bottom: 5px !important; margin-top: 1px !important; }
                .exclude-patterns-field.small > label{display: contents;}
                .search-modifiers{background: color-mix(in srgb, currentColor 5%, -moz-Dialog) !important;}
                .source-outline-tabs{width: 98% !important}
                .source-outline-tabs > .tab{background-color:  color-mix(in srgb, currentColor 3%, -moz-Dialog) !important;}
                .source-outline-tabs > .tab:first-child{border-radius: 8px 0 0 8px !important}
                .source-outline-tabs > .tab:last-child{border-radius: 0 8px 8px 0 !important}
                .tab.sources-tab.active{background-color:  color-mix(in srgb, currentColor 3%, -moz-Dialog) !important;}
                .source-footer, .source-header{background: -moz-Dialog !important; border: none !important}
                .editor-wrapper > *{background: -moz-Dialog !important; border: none !important}
                .editor-wrapper > * > *, .welcomebox{border-radius: 8px !important;background-color: color-mix(in srgb, currentColor 5%, -moz-Dialog) !important; z-index: 2 !important;}
                .outline-footer{background-color: color-mix(in srgb, currentColor 5%, -moz-Dialog) !important; border-radius: 8px !important; border: none !important;}
                .outline-footer > button {width: -moz-available !important; border-radius: 8px !important; }
                .outline-footer > button:hover {width: -moz-available !important; border-radius: 8px !important; background: color-mix(in srgb, currentColor 13%, -moz-Dialog) !important;}
                .secondary-panes-wrapper > *{background: -moz-Dialog !important;}
                .command-bar{border: none !important;}
                .source-outline-panel{margin-top: -8px !important;padding: 8px !important;}

                /*  Memory  */
                .snapshot-list-item{ border-radius: 10px !important; border: none !important; margin-bottom: 5px !important; }
                .snapshot-list-item:not(.selected){background-color: color-mix(in srgb, currentColor 5%, -moz-Dialog) !important;}
                #shortest-paths > div{border-radius: 10px !important; background: -moz-Dialog !important;}
                #shortest-paths{background: -moz-Dialog !important;}
                #heap-view .h-split-box{border-radius: 10px !important;}
                .heap-view-panel > .tree{ border-radius: 10px 10px 0 0!important; }
                .heap-view-panel > .header{background: -moz-Dialog !important; border: none !important}
                .heap-view-panel{background: -moz-Dialog !important}
                #memory-tool-container > ul{ padding: 5px !important; padding-top: 5px; background: -moz-Dialog !important; border: none !important; padding-top: 2px !important; }
                .tree-map-container{ border-radius: 10px !important; }
                .toolbar-group, #record-allocation-stacks-label{border-inline-end: none !important;}
                #record-allocation-stacks-label{margin-inline-end: 10px !important;}

                /*  Stylesheet editor/Console  */
                .devtools-invisible-splitter, .splitview-nav{background-color: -moz-Dialog !important}
                .splitview-controller{padding: 0 15px 0 5px !important; background-color: -moz-Dialog !important;}
                .theme-body.devtools-monospace .CodeMirror-gutters{background: color-mix(in srgb, #000 20%, -moz-Dialog) !important}
                .theme-body.devtools-monospace .CodeMirror-scroll{background-color: color-mix(in srgb, currentColor 5%, -moz-Dialog) !important;}

                /*  Tabs  */
                .outline-filter{border: none !important;}
                .tabs, .sources-panel{background-color: -moz-Dialog !important}
                .tabs-navigation, .source-outline-tabs{background-color: color-mix(in srgb, currentColor 3%, -moz-Dialog) !important; border: none !important; padding: 0 10px 0 5px; border-radius: 10px !important; margin: 3px !important}
                .all-tabs-menu{right: 10px !important; background-color: transparent !important; border: none !important;}
                .panels{background-color: -moz-Dialog !important}
                pre{margin-right: 20px;}

                /*  Network  */
                .request-list-container{background: -moz-Dialog !important;}
                .request-list-empty-notice, .requests-list-scroll{border-radius: 10px !important; background: color-mix(in srgb, currentColor 5%, -moz-Dialog) !important}

                /*  Probably the console  */
                #mount .sidebar-item{border-radius: 10px !important; padding: 10px 0 10px 5px !important; margin-bottom: 4px !important;}
                #mount .js-sidebar{padding: 0 5px 0px 7px !important; background: -moz-Dialog !important; border: none !important}
                #mount > main{background: -moz-Dialog;}
                #mount .app-page{border-radius: 10px !important; background-color: color-mix(in srgb, currentColor 5%, -moz-Dialog) !important;}

                /*  Storage  */
                #storage-table{background: -moz-Dialog !important;}
                #storage-tree{background: -moz-Dialog !important;border-radius: 10px !important; padding-top: 3px !important}

                /*  Fixes for sidebar version  */
                .horz{background-color: -moz-Dialog !important}
                .horz .splitter{flex: 0 0 10px !important; margin: auto 2px auto 2px !important; height: 80%; justify-content: center; opacity: 100%; border-radius: 4px !important;}
                .horz #ruleview-container{  margin-top: 15px;}

                /*  Fix for light mode  */
                .theme-light .tree-widget-item.theme-selected{color: #000 !important;}

                /*  ?  */
                .devtools-toolbar-select{ background-color: color-mix(in srgb, currentColor 5%, -moz-Dialog) !important; border-radius: 4px !important; border: none !important; padding: 4px !important; appearance: dialog !important; }
                #toolbox-deck{border-radius: 10px !important;}
                .theme-dark{background-color: color-mix(in srgb, currentColor 5%, -moz-Dialog) !important}
                 body{background-color: color-mix(in srgb, currentColor 7%, -moz-Dialog) !important}
                #ruleview-container, .layout-container, .devtools-sidebar-tabs .panels{border-radius: 10px !important; background-color: -moz-Dialog; padding: 0 5px 0 5px;}
                .splitter{flex: 0 0 10px !important; margin: 30px -9px 30px -6px !important; justify-content: center; opacity: 0;}
                .devtools-inspector-tab-panel{background-color: -moz-Dialog !important}
                .compatibility-app__container{background-color: -moz-Dialog !important}
                .compatibility-footer{border: none !important;}
                .devtools-separator{display: none !important;}

                /*  Acessability  */
                window box.devtools-responsive-container.theme-body vbox#storage-tree ul.tree-widget-container li ul.tree-widget-children li div.tree-widget-item{background-color: color-mix(in srgb, currentColor 5%, -moz-Dialog) !important; border-radius: 8px !important; margin-bottom: 2px !important; padding: 5px !important; width: 85% !important; margin-left: 12px !important;  margin-right: 5px !important}
                .tree-widget-item:not(window box.devtools-responsive-container.theme-body vbox#storage-tree ul.tree-widget-container li ul.tree-widget-children li div.tree-widget-item){ border-radius: 8px !important; margin-bottom: 2px !important; padding: 5px !important; width: 90% !important; margin-left: 2px !important; margin-right: 5px !important}
                .devtools-side-splitter{opacity: 0% !important}
                .devtools-responsive-container.theme-body{background: -moz-Dialog !important;}
                .devtools-toolbar{position: sticky; top: 0px}

                /*  Stylesheet editor  */
                #style-editor-chrome .devtools-invisible-splitter{border-radius: 10px 0 0 10px !important; background: color-mix(in srgb, #000 20%, -moz-Dialog) !important; !important; width: 10px; margin-left: -10px !important; opacity: 100% !important}
                .editor-pane .CodeMirror-gutters{background: color-mix(in srgb, #000 20%, -moz-Dialog) !important}
                .editor-pane .CodeMirror-scroll{background-color: color-mix(in srgb, currentColor 5%, -moz-Dialog) !important;}
                .at-rule-label{ border: none !important; padding: 5px !important; background: aliceblue; border-radius: 8px !important; margin: 0 0 3px 5px !important; width: 92% !important; background-color: color-mix(in srgb, currentColor 5%, -moz-Dialog) !important; }
                window#style-editor-chrome-window box#style-editor-chrome.devtools-responsive-container.theme-body box.splitview-side-details.devtools-main-content box.splitview-details.splitview-active hbox.stylesheet-details-container vbox.stylesheet-sidebar.theme-sidebar toolbar.devtools-toolbar, .stylesheet-at-rules-container{background: color-mix(in srgb, #000 20%, -moz-Dialog) !important}
                .splitview-nav{border-radius: 10px !important;}
                .splitview-nav > *{border-radius: 8px !important; border: none !important; margin-bottom: 5px !important; padding: 3px !important; background-image: none !important}
                .splitview-nav > *:not(:hover, .splitview-active){background: color-mix(in srgb, currentColor 5%, -moz-Dialog) !important}
                #splitview-resizer-target{background-color: -moz-Dialog !important}
                .splitview-main > toolbar, .loading .splitview-nav-container{border-inline-end: none !important} /*  Removes line while loading  */
                .splitview-side-details.devtools-main-content{background: color-mix(in srgb, #000 20%, -moz-Dialog) !important;}
                window#style-editor-chrome-window box#style-editor-chrome.devtools-responsive-container.theme-body box.splitview-side-details.devtools-main-content box.splitview-details.splitview-active hbox.stylesheet-details-container box.stylesheet-editor-input.textbox iframe{ border-radius: 10px !important; }

                /*  Accordions  */
                body main#mount.theme-body div.debugger div.A11y-mouse div.split-box.vert div.controlled div.secondary-panes-wrapper div.secondary-panes div ul.accordion *:not([type="checkbox"]) {border: none !important}
                body.theme-body.devtools-monospace div#content div.mainFrame span div.split-box.vert div.uncontrolled{border-radius: 10px !important}
                body.theme-body.devtools-monospace div#content div.mainFrame span div.split-box.vert{background: -moz-Dialog !important}
                body.theme-body.devtools-monospace div#content div.mainFrame span div.split-box.vert div.controlled div.right-sidebar{margin-left: 10px !important;}
                body.theme-body.devtools-monospace div#content div.mainFrame span div.split-box.vert div.controlled div.right-sidebar ul.accordion{width: 95% !important; padding-right: 10px !important;}
                #sidebar-panel-ruleview, .accordion{background-color: -moz-Dialog !important}
                .accordion li h2:not(.accordion-header){background-color: color-mix(in srgb, currentColor 5%, -moz-Dialog) !important; margin: 5px 2px 5px 8px !important; border-radius: 8px !important; width: 97% !important}
                .ruleview-rule, .accordion-content{border-radius: 8px !important; padding: 5px !important; background-color: color-mix(in srgb, currentColor 5%, -moz-Dialog) !important; border-bottom: none !important; margin-bottom: 4px !important;}
                .ruleview-header, .accordion-header{background-color: -moz-Dialog !important; border-bottom: none !important}

                /*  Cahnges tab  */
                #sidebar-panel-changes .source .href{background: -moz-Dialog !important; border: none !important}
                #sidebar-panel-changes .source{background: -moz-Dialog !important}
                #sidebar-panel-changes .changes__toolbar{ padding: 6px !important; border: none !important; background: -moz-Dialog !important; padding: 0 !important; }
                #sidebar-panel-changes .changes__rule {background: color-mix(in srgb, currentColor 5%, -moz-Dialog) !important; border-radius: 8px !important; margin-bottom: 2px !important}
                #sidebar-panel-changes{background: -moz-Dialog !important; border: none !important}

                .changes__copy-all-changes-button{width: -moz-available; padding: 6px !important; background-color: color-mix(in srgb, currentColor 5%, -moz-Dialog) !important; border-radius: 10px !important; margin: 6px !important; background-position: 13px 9.5px !important; height: 35px !important;}
                .changes__copy-all-changes-button:hover{background-color: color-mix(in srgb, currentColor 13%, -moz-Dialog) !important}



                /*  Accessability  */
                .accessibility-check{background-color: color-mix(in srgb, currentColor 5%, -moz-Dialog) !important;}
                .right-sidebar{background: -moz-Dialog !important;}
                .checks .list > *{padding: 0 !important}}
            '';
        };
    };
}