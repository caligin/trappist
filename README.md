# trappist

stuff to receive snmp traps. no idea about how will be declined but original goal was
to listen to config change traps to trigger a fetch.

`make shell` must be run as root as the app requires opening privileged port 162,
run with `application:start(trappist).`
