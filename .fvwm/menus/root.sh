# File:     root.sh
# Author:   Mads N Noe <mntnoe (@) gmail.com>
# Licence:  as-is
# Modified: 2008-10-12

cat << eos
DestroyMenu recreate MenuRoot
AddToMenu MenuRoot      

#+ %24x24-shortcut.png%"&Shortcuts" \
                                                Popup MenuShortcuts
+ "%24x24-services.png%&Services"               Menu MenuServices Root c c
+ "%24x24-halt.png%E&xit"                       Menu MenuSession Root c c

# note: bash interp. FVWM_USERDIR, so no []!

# No key accelerators here; shouldn't be too easy to hit...
DestroyMenu MenuSession
AddToMenu   MenuSession
+ "Session"                                     Title
+ "%24x24-restart-fvwm.png%Restart FVWM"        Restart
+ ""                                            Nop
+ "%24x24-lock.png%Lock"                        Exec exec shalock
+ "%24x24-logout.png%Logout"                    Quit
eos
if [[ "$HOSTNAME" == "mntnoe-laptop" ]]; then
    cat << eos
+ ""                                            Nop
+ "%24x24-suspend.png%Suspend"                  Exec exec ramlock
+ "%24x24-hibernate.png%Hibernate"              Exec exec hiblock
eos
fi
cat << eos
+ ""                                            Nop
+ "%24x24-reboot.png%Reboot"                    Exec exec reboot_wrapper.sh
+ "%24x24-halt.png%Halt"                        Exec exec halt_wrapper.sh

DestroyMenu MenuServices
AddToMenu MenuServices
+ DynamicPopupAction PipeRead "$FVWM_USERDIR/menus/services.sh"

# vim: set ft=fvwm:
