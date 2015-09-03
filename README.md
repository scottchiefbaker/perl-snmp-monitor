# perl-snmp-monitor
Perl script to monitor bandwidth via SNMP

##### Sample usage: #####
```
perl snmp_int_watch.pl community@hostname [filter]

    --invert          Invert your filter
    --bytes           Output bandwidth per secondin bytes
    --delay           Delay between updates (in seconds)
    --int_num 1,5,9   Only show specific SNMP interfaces
    --32bit           Force 32bit SNMP counters
    --debug           Enable debug mode
```
