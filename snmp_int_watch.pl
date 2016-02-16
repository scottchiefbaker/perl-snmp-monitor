#!/usr/bin/perl

use strict;
use warnings;
use Getopt::Long;
use Time::HiRes qw(time sleep);
use Net::SNMP;
use English;
#use Data::Dump::Color;

# Disable output buffering
$OUTPUT_AUTOFLUSH = 1;

my ($bits,$bytes,$invert,$thirty_two,$use_alias,$no_color,$if_str,$csv,$ping);
my $debug = 0;
my $delay = 3; # Default delay is 3 seconds

my $ok = GetOptions(
	'debug+'      => \$debug,
	'bytes|b'     => \$bytes,
	'bits'        => \$bits,
	'delay|d=i'   => \$delay,
	'int_num|i=s' => \$if_str,
	'invert|v'    => \$invert,
	'alias|desc'  => \$use_alias,
	'32bit'       => \$thirty_two,
	'nocolor'     => \$no_color,
	'csv'         => \$csv,
	'ping'        => \$ping,
);

# Default to bits if nothing is specified
if (!$bits && !$bytes) {
	$bits = 1;
# In case they try and set both bits and bytes
} elsif ($bits) {
	$bytes = 0;
}

# Check to see if there is a list of interface numbers
my @ifs;
if ($if_str) {
	@ifs = split(/,/,$if_str);
	@ifs = grep { $_ > 0 } @ifs;
}

# If there is nothing passed in via ARGV or ARGV does't have a '@' in it bail out
if (!$ARGV[0] || $ARGV[0] !~ /@/) {
	die(usage());
}

# Break appart the connect string
my @p         = split(/@/,$ARGV[0]);
my $community = $p[0];
my $host      = $p[1];
my $filter    = $ARGV[1] || "";

if (!$community || !$host) {
	die(usage());
}

my $s = snmp_connect($host,$community);

# If they force 32 bit use that, otherwise check if the device supports 64bit
my $sixtyfour = 0;
if ($thirty_two) {
	$sixtyfour = 0;
} else {
	$sixtyfour = has_64bit_counters($s);
}

my $ints      = get_interface_names($s);
my $int_count = scalar(keys(%$ints));

my $last  = {};
my $first = 1;
while(1) {
	my $start = time();

	# Grab the interface stats
	my $cur = get_interface_bandwidth($s);

	# If we have previous data, output the bw
	if (%$last) {
		output_data($cur,$last);
	}

	# Calculate how much time to sleep between interations
	my $elapsed = time() - $start;
	my $remain  = $delay - $elapsed;

	# The first time we only sleep one second, this gets
	# something on the screen as quickly as possible
	if ($first) {
		if ($csv && $ping) {
			print "#Time,InterfaceName,BytesOut,BytesIn,PingMs\n";
		} elsif ($csv) {
			print "#Time,InterfaceName,BytesOut,BytesIn\n";
		}

		$remain = 1 - $elapsed;
		$first = 0;
	}

	# If the request took more than the delay, we don't delay at all
	if ($remain < 0) {
		$remain = 0;
	}

	if ($debug) {
		printf("Sleeping for %0.2f seconds\n",$remain);
	}

	# Wait X seconds and grab the data again
	sleep($remain);

	# Store the data to compare it
	$last = $cur;
}

#############################################################

sub snmp_connect {
	my ($host,$community) = @_;
	my ($session, $error) = Net::SNMP->session(
		-hostname  => $host,
		-community => $community,
		-timeout   => 5,
		-retries   => 0,
		-version   => 2,
		-port      => 161,
	);

	if (!defined($session)) {
		printf("ERROR: %s.\n", $error);
		return undef;
	}

	return $session;
}

sub get_interface_bandwidth {
	my $session = shift();

	my $start = time();

	# IF-MIB::ifInOctets / IF-MIB::ifOutOctets
	my $out_oid = ".1.3.6.1.2.1.2.2.1.16";
	my $in_oid  = ".1.3.6.1.2.1.2.2.1.10";

	# If the device supports 64 bit values, get those instead because they're more accurate
	if ($sixtyfour) {
		# IF-MIB::ifHCInOctets / IF-MIB::ifHCOutOctets
		$in_oid  = ".1.3.6.1.2.1.31.1.1.1.6";
		$out_oid = ".1.3.6.1.2.1.31.1.1.1.10";
	}

	# Get the output bytes
	my ($response,$err,$ret) = ({},{},{});
	if (@ifs) {
		foreach my $num (@ifs) {
			my $int_oid = $out_oid . ".$num";

			my $rsp = $session->get_request($int_oid);
			my $err = $session->error;

			%$response = (%$response,%$rsp);
		}
	} else {
		$response = $session->get_table($out_oid);
		$err      = $session->error;
	}

	# Store each interface in a hash by name
	foreach my $key(keys(%$response)) {
		my $bw       = $response->{$key};
		my $int_num  = int_num($key);
		my $int_name = $ints->{$int_num};

		$ret->{$int_name}->{$int_num}->{out} = $bw;
	}

	if ($debug) {
		my $total = sprintf("%0.2f",time() - $start);
		print "Fetch \"out\" bandwidth took $total seconds\n";
	}

	##########################################################

	$start = time();

	$response = {};

	# Get the input bytes
	if (@ifs) {
		foreach my $num (@ifs) {
			my $int_oid = $in_oid . ".$num";

			my $rsp = $session->get_request($int_oid);
			my $err = $session->error;

			%$response = (%$response,%$rsp);
		}
	} else {
		$response = $session->get_table($in_oid);
		$err      = $session->error;
	}

	# Store each interface in a hash by name
	foreach my $key(keys(%$response)) {
		my $bw = $response->{$key};
		my $int_num = int_num($key);
		my $int_name = $ints->{$int_num};

		$ret->{$int_name}->{$int_num}->{in} = $bw;
	}

	if ($debug) {
		my $total = sprintf("%0.2f",time() - $start);
		print "Fetch \"in\" bandwidth took $total seconds\n";
	}

	return $ret;
}

sub get_interface_names {
	my $session = shift();

	my $ret = {};

	my $oid = "";
	if ($use_alias) {
		# IF-MIB::ifAlias
		$oid = ".1.3.6.1.2.1.31.1.1.1.18";
	} else {
		# IF-MIB::ifDescr
		$oid = ".1.3.6.1.2.1.2.2.1.2";
	}

	my $start = time();

	my ($response,$err) = ({},{});
	if (@ifs) {
		foreach my $num (@ifs) {
			my $int_oid = $oid . ".$num";

			my $rsp = $session->get_request($int_oid);
			my $err = $session->error;

			%$response = (%$response,%$rsp);
		}
	} else {
		$response = $session->get_table($oid);
		$err      = $session->error;
	}

	# We're maping the IDs to the name of the interface
	foreach my $key(nsort(keys(%$response))) {
		my $value    = $response->{$key};
		my $int_num  = int_num($key);

		if ($use_alias && !$value) {
			$value = "No alias";
		}

		if ($debug > 1) {
			printf("%2d => %s\n",$int_num,$value);
		}

		$ret->{$int_num} = $value;
	}

	if ($debug) {
		my $total = sprintf("%0.2f",time() - $start);
		my $count = scalar(keys(%$ret));
		print "Fetch interface names took $total seconds ($count found)\n";
	}

	return $ret;
}

# Convert an OID to an interface number
sub int_num {
	my $str = shift();
	# Remove everything up to the LAST period
	$str =~ s/.+\.//;

	return $str;
}

sub if_count {
	my $data = shift();
	my @ifs  = @_;

	my $ret = 0;
	foreach my $if_name (@ifs) {
		foreach my $int_num (keys %{$data->{$if_name}}) {
			$ret++;
		}
	}

	return $ret;
}

sub output_data {
	my ($cur,$last) = @_;

	my @ints = nsort(keys(%$cur));

	if ($filter && $invert) {
		@ints = grep(!/$filter/,@ints);
	} elsif ($filter) {
		@ints = grep(/$filter/,@ints);
	}

	my $if_count = if_count($cur,@ints);

	# Find the length of the longest interface name
	my $max_len = 0;
	foreach (@ints) {
		my $len = length($_);
		if ($len > $max_len) {
			$max_len = $len;
		}
	}

	my $date = mysql_date(1);

	if ($csv) {
		# No header, we do it down below
	# If there is only one interface we output the data on one line instead of a table
	} elsif ($if_count == 1) {
		print color("15bold");
		printf("$date: ");
		print color();
	# More than one interface, table mode
	} else {
		# Print the date header
		print color("15bold");
		printf("$date\n");
		print "-" x length($date) . "\n";

		print color();
	}

	my $ping_str = "";
	if ($ping) {
		$ping_str = "," . ping_host($host);
	}

	# Loop through each interface, calculate the bytes between the
	# previous data and now
	foreach my $name (@ints) {
		foreach my $int_num (keys %{$cur->{$name}}) {
			my $prev      = $last->{$name}->{$int_num}->{out};
			my $now       = $cur->{$name}->{$int_num}->{out};
			my $out_total = $now - $prev;


			# Check if the counters rolled and act appropriately
			if (!$sixtyfour && ($out_total < 0)) {
				$out_total = (2**32 + $now) - $prev;

				if ($debug > 1) {
					print "\n ** 32bit output counter roll detected... compensating\n";
				}
			}

			my $iprev    = $last->{$name}->{$int_num}->{in};
			my $inow     = $cur->{$name}->{$int_num}->{in};
			my $in_total = $inow - $iprev;

			# Check if the counters rolled and act appropriately
			if (!$sixtyfour && ($in_total < 0)) {
				$in_total = (2**32 + $inow) - $iprev;

				if ($debug > 1) {
					print "\n ** 32bit input counter roll detected... compensating\n";
				}
			}

			if ($bits) {
				$out_total *= 8;
				$in_total  *= 8;
			}

			my $out_str = human_size(int($out_total / $delay));
			my $in_str  = human_size(int($in_total / $delay));

			if ($csv) {
				my $out_str = int($out_total / $delay);
				my $in_str  = int($in_total / $delay);
				printf("$date,$name,$out_str,$in_str$ping_str\n");
			} else {
				my $open_color  = color(14);
				my $reset_color = color();

				printf("$open_color%-${max_len}s$reset_color = $out_str/$in_str\n",$name);
				print color();
			}

			if ($debug > 1) {
				printf(" * In : $inow - $iprev ($in_total)\n * Out: $now - $prev ($out_total)\n");
			}
		}
	}

	# If it's not oneline mode, we output an extra \n
	if (!$csv && $if_count > 1) {
		print "\n";
	}
}

sub mysql_date {
	my ($inc_time,$epoch) = @_;

	$epoch ||= time();
	my @date = localtime($epoch);

	my $ret = sprintf("%04d-%02d-%02d", $date[5] + 1900, $date[4] + 1, $date[3]);
	if ($inc_time) { $ret .= sprintf(" %02d:%02d:%02d", $date[2], $date[1], $date[0]); }

	return $ret;
}

sub usage {
	my $ret  = "Usage: $0 community\@hostname [filter]\n";
	$ret    .= "\n";
	$ret    .= "    --invert          Invert your filter\n";
	$ret    .= "    --bytes           Output bandwidth per second in bytes\n";
	$ret    .= "    --delay           Delay between updates (in seconds)\n";
	$ret    .= "    --int_num 1,5,9   Only show specific SNMP interfaces\n";
	$ret    .= "    --32bit           Force 32bit SNMP counters\n";
	$ret    .= "    --desc            Use interface aliases/descriptions instead of port names\n";
	$ret    .= "    --nocolor         Output data without any colors\n";
	$ret    .= "    --debug           Enable debug mode\n";

	return $ret;
}

# String format: '115', '165bold', '10_on_140', 'reset', 'on_173'
sub color {
	if ($no_color) {
		return "";
	}

	my $str = shift();

	# If we're connected to a TTY don't do color
	if (-t STDOUT == 0) { return ''; }

	# No string sent in, so we just reset
	if (!$str || $str eq 'reset') {
		return "\e[0m";
	}

	# Get foreground and bold
	my ($fc,$bold) = $str =~ /^(\d+)(b|bold)?/g;
	# Get the background color (if present)
	my ($bc)       = $str =~ /on_?(\d+)$/g;

	my $ret = '';
	if ($bold) { $ret .= "\e[1m"; }
	if ($fc)   { $ret .= "\e[38;5;${fc}m"; }
	if ($bc)   { $ret .= "\e[48;5;${bc}m"; }

	return $ret;
}

sub human_size {
	my $size      = shift();
	my $use_color = shift() // 1;

	my $color = '';
	my $reset = color();

	if ($size > 1024**3) {
		$color = color(12); # Blue
		$size = sprintf("%.1fG",$size / 1024**3);
	} elsif ($size > 1024**2) {
		$color = color(10); # Green
		$size = sprintf("%.1fM",$size / 1024**2);
	} elsif ($size > 1024) {
		$color = color(11); # Yellow
		$size = sprintf("%.1fK",$size / 1024);
	} elsif ($size >= 0) {
		$color = color(9); # Red
		$size = sprintf("%dB",$size);
	}

	if ($use_color) {
		$size = $color . $size . $reset;
	}

	return $size;
}

sub has_64bit_counters {
	my $session = shift();
	my $start   = time();
	my $ret;

	# IF-MIB::ifHCInOctets / IF-MIB::ifHCOutOctets
	# 1.3.6.1.2.1.31.1.1.1.6  = in
	# 1.3.6.1.2.1.31.1.1.1.10 = out

	my $oid  = ".1.3.6.1.2.1.31.1.1.1.6.1";
	my $resp = $session->get_request($oid);
	my $err  = $session->error;

	if (!defined($resp)) {
		die("Timeout on SNMP request\n");
	}

	if ($err || $resp->{$oid} eq "noSuchObject") {
		$ret = 0;
	} else {
		$ret = 1;
	}

	if ($debug) {
		my $total = sprintf("%0.2f",time() - $start);
		print "Checking if device understands 64 bit counters (" . boolean_to_str($ret) . ") took $total seconds\n";
	}

	return $ret;
}

# http://stackoverflow.com/questions/6282574/how-can-i-sort-a-list-of-strings-by-numbers-in-them
sub expand {
	my $file=shift;
	$file=~s{(\d+)}{sprintf "%04d", $1}eg; # expand all numbers to 4 digits
	return $file;
}

# Stolen from Sort::Naturally https://github.com/bingos/sort-naturally/
sub nsort {
	my($cmp, $lc);
	return @_ if @_ < 2;   # Just to be CLEVER.

	my($x, $i);  # scratch vars

	map
		$_->[0],

	sort {
		# Uses $i as the index variable, $x as the result.
		$x = 0;
		$i = 1;

		while($i < @$a and $i < @$b) {
			last if ($x = ($a->[$i] cmp $b->[$i])); # lexicographic
			++$i;

			last if ($x = ($a->[$i] <=> $b->[$i])); # numeric
			++$i;
		}

		$x || (@$a <=> @$b) || ($a->[0] cmp $b->[0]);
	}

	map {
		my @bit = ($x = defined($_) ? $_ : '');

		if($x =~ m/^[+-]?(?=\d|\.\d)\d*(?:\.\d*)?(?:[Ee](?:[+-]?\d+))?\z/s) {
			# It's entirely purely numeric, so treat it specially:
			push @bit, '', $x;
		} else {
			# Consume the string.
			while(length $x) {
				push @bit, ($x =~ s/^(\D+)//s) ? lc($1) : '';
				push @bit, ($x =~ s/^(\d+)//s) ?    $1  :  0;
			}
		}

		\@bit;
	}
	@_;
}

sub boolean_to_str {
	my $i = shift();

	if ($i) {
		return "yes";
	} else {
		return "no";
	}
}

sub ping_host {
	my $ip = shift();

	my $cmd = "ping $ip -c 5 -i .2";
	my @out = `$cmd`;

	my $last = scalar(@out) - 1;
	my $line = $out[$last];

	# rtt min/avg/max/mdev = 17.867/18.457/18.881/0.407 ms
	my ($nums) = $line =~ /= (.*?) ms/;
	my @parts  = split(/\//,$nums);
	my $avg    = sprintf("%0.1f",($parts[1]));

	return $avg;
}
