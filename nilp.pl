#!/usr/bin/perl -n

# Needs to run multiple times for same-line occurrences.
# for file in src/*.[ch] ; do 2>/dev/null clang-check --ast-dump -p ./compile_commands.json  $file | MYFILE=$file ./nilp.pl ; done ;

use File::Basename;

BEGIN {
    $myfile = basename($ENV{MYFILE});
    $file = "";
    $line = "";
    $col = "";
}

/^[| `-]+(\S+) 0x[0-9a-f]+ <([^>]+)> (.*)/; # `
my ($what, $annots, $rest) = ($1, $2, $3);
if ($annots) {
    my @rest = split(/\s+/, $rest);
    for my $a (split(/\s*,\s*/, $annots)) {
	parse_annot($a);
	if ($file eq $myfile && $what eq 'UnaryOperator' && $rest[2] eq "'!'") {
	    to_disk($line, $col);
	}
    }
}

sub to_disk {
    my ($wline, $wcol) = @_;
    open my $in, "<", $ENV{MYFILE} or die;
    open my $out, '>', "$ENV{MYFILE}.new" or die;
    while (<$in>) {
	my $text = $_;
	if ($. == $wline) {
	    if (substr($text, $wcol - 1, 2) eq '! ') {
		$text = substr($text, 0, $wcol) . substr($text, $wcol + 1);
	    }
	}
	print $out $text;
    }
    close $in;
    close $out;
    rename "$ENV{MYFILE}.new", $ENV{MYFILE};
}

sub parse_annot {
    my ($annot) = @_;
    my @foo = split(/:/, $annot);
    if ($foo[0] eq 'line') {
	$line = $foo[1];
	$col = $foo[2];
    } elsif ($foo[0] eq 'col') {
	$col = $foo[1];
    } else {
	$file = $foo[0];
	$line = $foo[1];
	$col = $foo[2];
    }
}
