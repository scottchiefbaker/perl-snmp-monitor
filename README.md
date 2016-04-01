# perl-snmp-monitor
Perl script to monitor interface bandwidth via SNMP

##### Sample usage: #####
```
Usage: snmp_int_watch.pl community@hostname [filter]

    --invert          Invert your filter
    --bytes           Output bandwidth per second in bytes
    --delay           Delay between updates (in seconds)
    --int_num 1,5,9   Only show specific SNMP interfaces
    --32bit           Force 32bit SNMP counters
    --desc            Use interface aliases/descriptions instead of port names
    --nocolor         Output data without any colors
    --csv             Output data in CSV format
    --ping            Include average ping in CSV output
    --discover        Print out a list of interface IDs
    --debug           Enable debug mode
    --help            Print usage
```
