# File:     services.sh
# Author:   Mads N Noe <mntnoe (@) gmail.com>
# Licence:  as-is
# Modified: 2008-08-14

cat << eos
DestroyMenu recreate MenuServices
AddToMenu MenuServices
eos

echo '+ "Services"      Title'
SERVICES=`sudo -l | grep /etc/init.d | sed 's/.*\/\(.*\).*/\1/'`

SEP=0
for I in $SERVICES
do
    ICON="24x24-services.png"
    if [[ "$I" == "net.eth0" ]] ; then
        ICON="24x24-network-wired.png"
    fi
    if [[ "$I" == "net.wlan0" ]] ; then
        ICON="24x24-network-wireless.png"
    fi

    if [[ "$SEP" -eq 1 ]]; then
        # Only show seperators between services
        echo "+ ''                          Nop"
    fi
    SEP=1
    echo "+ '%$ICON%$I'  Exec exec urxvt -e sleepdo 1 sudo /etc/init.d/$I restart"
    echo "+ 'stop'                      Exec exec urxvt -e sleepdo 1 sudo /etc/init.d/$I stop"
done

# vim: set ft=sh:
