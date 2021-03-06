#!/usr/bin/perl

use strict;
use warnings;
use Getopt::Long;
use Time::HiRes qw(time sleep);
use Net::SNMP;
use English;
use 5.010; # Needed for 'state' variables

# Disable output buffering
$OUTPUT_AUTOFLUSH = 1;

my ($bits,$bytes,$invert,$thirty_two,$use_alias,$no_color,$if_str,$csv,$ping,$discover,$help,$timeout);
my $debug        = 0;
my $delay        = 10; # Default delay
my $script_start = time();

my $ok = GetOptions(
	'debug+'      => \$debug,
	'bytes|b'     => \$bytes,
	'bits'        => \$bits,
	'delay|n=i'   => \$delay,
	'int_num|i=s' => \$if_str,
	'invert|v'    => \$invert,
	'alias|desc'  => \$use_alias,
	'32bit'       => \$thirty_two,
	'nocolor'     => \$no_color,
	'csv'         => \$csv,
	'ping'        => \$ping,
	'discover'    => \$discover,
	'help'        => \$help,
	'timeout=i'   => \$timeout,
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
if ($help || !$ARGV[0] || $ARGV[0] !~ /@/) {
	die(usage());
}

# Break appart the connect string
my @p         = split(/@/,$ARGV[0]);
my $community = $p[0];
my $host      = $p[1];
my $filter    = $ARGV[1] || "";

# Example 123456:community@server.domain.com
if ($community =~ /(\d+?):(.+)/) {
	my $if_num = $1;
	$community = $2;

	@ifs = ($if_num);
}

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

my $ints      = get_interface_names($s,$use_alias);
my $int_count = scalar(keys(%$ints));
my $one_id    = is_one_interface($ints,$filter,$invert);

if ($one_id && !@ifs) {
	if ($debug) {
		print STDERR "'$filter' maps to a SINGLE interface: $one_id\n";
	}
	@ifs = ($one_id);
}

# Just output the interface -> id mapping
if ($discover) {
	show_discover($ints);
	exit();
}

#########################################################################
# Start of the loop that gathers and outputs stats
#########################################################################

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
		my $in_out = "";
		if ($bits) {
			$in_out = "BitsOutSec,BitsInSec";
		} else {
			$in_out = "BytesOutSec,BytesInSec";
		}

		if ($csv && $ping) {
			print "#Time,InterfaceName,$in_out,PingMs\n";
		} elsif ($csv) {
			print "#Time,InterfaceName,$in_out\n";
		}

		$remain = 1 - $elapsed;
		$first = 0;
	}

	# If the request took more than the delay, we don't delay at all
	if ($remain < 0) {
		$remain = 0;
	}

	if ($debug) {
		printf STDERR ("Sleeping for %0.2f seconds\n",$remain);
	}

	my $total = time() - $script_start;
	if ($timeout && ($total >= $timeout)) {
		print "Timeout reached\n";
		exit;
	}

	# Wait X seconds and grab the data again
	sleep($remain);

	# Store the data to compare it
	if ($cur) {
		$last = $cur;
	}
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

	my $bit_mode;
	if ($sixtyfour) {
		$bit_mode = "64 bit";
	} else {
		$bit_mode = "32 bit";
	}

	my $fetch_mode;
	if ($one_id) {
		$fetch_mode = "single OID";
	} else {
		$fetch_mode = "OID Table";
	}

	my $extra_str = "($bit_mode / $fetch_mode)";

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

			if (!$err) {
				%$response = (%$response,%$rsp);
			} else {
				return undef;
			}
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
		print STDERR "Fetch \"out\" bandwidth took $total seconds $extra_str\n";
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
		print STDERR "Fetch \"in\" bandwidth took $total seconds $extra_str\n";
	}

	return $ret;
}

sub get_interface_aliases {
	my $snmp_session = shift();

	my $ifcs = get_interface_names($snmp_session,1);

	return $ifcs;
}

sub get_interface_names {
	my $session   = shift();
	my $use_alias = shift();

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
			$value = "";
		}

		if ($debug > 1) {
			printf STDERR ("%2d => %s\n",$int_num,$value);
		}

		$ret->{$int_num} = $value;
	}

	if ($debug) {
		my $total = sprintf("%0.2f",time() - $start);
		my $count = scalar(keys(%$ret));
		print STDERR "Fetch interface names took $total seconds ($count found)\n";
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

	if (!defined($cur)) {
		print "Error fetching data\n";
		return 0;
	}

	my @ints = nsort(keys(%$cur));

	if ($filter && $invert) {
		@ints = grep(!/$filter/i,@ints);
	} elsif ($filter) {
		@ints = grep(/$filter/i,@ints);
	}

	if (!@ints) {
		die("Error: No interfaces were found to output\n");
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

	if ($if_count > 1) {
		# Print the date header
		print color("15_bold");
		print color("underline");

		my $half  = int($max_len / 2);
		my $left  = " " x $half;
		my $right = " " x ($max_len - $half - 1);

		print $left . $date . $right . "\n";
		#print "-" x length($date) . "\n";

		print color();
	}

	my $ping_str = "";
	if ($csv && $ping) {
		$ping_str = "," . ping_host($host);
	}

	# Loop through each interface, calculate the bytes between the
	# previous data and now
	foreach my $name (@ints) {
		foreach my $int_num (keys %{$cur->{$name}}) {
			my $prev      = int($last->{$name}->{$int_num}->{out});
			my $now       = int($cur->{$name}->{$int_num}->{out});
			my $out_total = $now - $prev;

			# Check if the counters rolled and act appropriately
			if (!$sixtyfour && ($out_total < 0)) {
				$out_total = (2**32 + $now) - $prev;

				if ($debug > 1) {
					print STDERR "\n ** 32bit output counter roll detected... compensating\n";
				}
			}

			my $iprev    = int($last->{$name}->{$int_num}->{in});
			my $inow     = int($cur->{$name}->{$int_num}->{in});
			my $in_total = $inow - $iprev;

			# Check if the counters rolled and act appropriately
			if (!$sixtyfour && ($in_total < 0)) {
				$in_total = (2**32 + $inow) - $iprev;

				if ($debug > 1) {
					print STDERR "\n ** 32bit input counter roll detected... compensating\n";
				}
			}

			if ($bits) {
				$out_total *= 8;
				$in_total  *= 8;
			}

			# This can sometimes happen if the remote router reboots in the middle
			# and resets the counters
			if ($out_total < 0) {
				$out_total = 0;
			}
			if ($in_total < 0) {
				$in_total = 0;
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
				my $date_str    = color("15_bold") . "$date: " . color();
				state $first    = 1;

				if ($if_count == 1) {
					if ($first) {
						printf("$date_str$open_color%-${max_len}s$reset_color =     %s |   %s\n",$name, "Up", "Down");
						$first = 0;
					}

					printf("$date_str$open_color%-${max_len}s$reset_color = %s | %s\n",$name,$out_str,$in_str);
					print color();
				} else {
					if ($first) {
						printf("%-${max_len}s$reset_color       %s |   %s\n","", "Up", "Down");
						$first = 0;
					}

					printf("$open_color%-${max_len}s$reset_color = %s | %s\n",$name,$out_str,$in_str);
					print color();
				}
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
	$ret    .= "    --csv             Output data in CSV format\n";
	$ret    .= "    --ping            Include average ping in CSV output\n";
	$ret    .= "    --discover        Print out a list of interface IDs\n";
	$ret    .= "    --debug           Enable debug mode\n";
	$ret    .= "    --help            Print usage\n";

	return $ret;
}

# String format: '115', '165_bold', '10_on_140', 'reset', 'on_173', 'red', 'white_on_blue'
sub color {
	my $str = shift();

	# If we're NOT connected to a an interactive terminal don't do color
	if (-t STDOUT == 0 || $no_color) { return ''; }

	# No string sent in, so we just reset
	if (!length($str) || $str eq 'reset') { return "\e[0m"; }

	# Some predefined colors
	my %color_map = qw(red 160 blue 21 green 34 yellow 226 orange 214 purple 93 white 15 black 0);
	$str =~ s|([A-Za-z]+)|$color_map{$1} // $1|eg;

	# Get foreground/background and any commands
	my ($fc,$cmd) = $str =~ /(\d+)?_?(\w+)?/g;
	my ($bc)      = $str =~ /on_?(\d+)/g;

	# Some predefined commands
	my %cmd_map = qw(bold 1 italic 3 underline 4 blink 5 inverse 7);
	my $cmd_num = $cmd_map{$cmd // 0};

	my $ret = '';
	if ($cmd_num)     { $ret .= "\e[${cmd_num}m"; }
	if (defined($fc)) { $ret .= "\e[38;5;${fc}m"; }
	if (defined($bc)) { $ret .= "\e[48;5;${bc}m"; }

	return $ret;
}

sub human_size {
	my $size      = shift();
	my $use_color = shift() // 1;

	my $color = '';
	my $reset = color();

	if ($size > 1024**3) {
		$color = color(12); # Blue
		$size = sprintf("%5.1fG",$size / 1024**3);
	} elsif ($size > 1024**2) {
		$color = color(10); # Green
		$size = sprintf("%5.1fM",$size / 1024**2);
	} elsif ($size > 1024) {
		$color = color(11); # Yellow
		$size = sprintf("%5.1fK",$size / 1024);
	} elsif ($size >= 0) {
		$color = color(9); # Red
		$size = sprintf("%5dB",$size);
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
		print STDERR "Checking if device understands 64 bit counters (" . boolean_to_str($ret) . ") took $total seconds\n";
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
	my $nums = "";
	if ($line =~ /= (.*?) ms/) {
		$nums = $1;
	} else {
		return -1;
	}

	my @parts  = split(/\//,$nums);
	my $avg    = sprintf("%0.1f",($parts[1]));

	return $avg;
}

# If the filter leaves only ONE interface return ID for that interface
sub is_one_interface {
	my ($ifcs,$filter,$invert) = @_;

	my @filtered;
	foreach my $id (keys(%$ifcs)) {
		my $desc = $ifcs->{$id};

		if ($filter && $invert) {
			if ($desc !~ /$filter/i) {
				push(@filtered,$id);
			}
		} elsif ($filter) {
			if ($desc =~ /$filter/i) {
				push(@filtered,$id);
			}
		}
	}

	if (@filtered == 1) {
		return $filtered[0];
	} else {
		return undef;
	}
}

sub show_discover {
	my $ints    = shift();
	my @ifcs    = sort { lc($ints->{$a}) cmp lc($ints ->{$b}) } keys %$ints;
	my $aliases = get_interface_aliases($s);

	# Find the longest interface name so we can properly line up columns
	my $longest = 0;
	foreach my $id (@ifcs) {
		my $name = $ints->{$id};
		my $len  = length($name);

		if ($len > $longest) {
			$longest = $len;
		}
	}

	my $longest_id = 0;
	foreach my $id (@ifcs) {
		my $len = length($id);

		if ($len > $longest_id) {
			$longest_id = $len;
		}
	}

	foreach my $id (@ifcs) {
		my $name  = $ints->{$id};
		my $alias = $aliases->{$id};

		if ($alias) {
			printf("%${longest_id}d = %-${longest}s (alias: '%s')\n",$id,$name,$alias);
		} else {
			printf("%${longest_id}d = %s\n",$id,$name);
		}
	}

	exit;
}

# Create a subroutine k() that either uses Data::Dump::Color or Dumper
# whichever is installed
#
# Borrowed from: http://www.perturb.org/display/1097_Perl_detect_if_a_module_is_installed_before_using_it.html
BEGIN {
	if (eval { require Data::Dump::Color }) {
		*k = sub { Data::Dump::Color::dd($_[0]) };
	} else {
		require Data::Dumper;
		*k = sub { print Data::Dumper::Dumper($_[0]) };
	}

	sub kd {
		k(@_);
		die;
	}
}
