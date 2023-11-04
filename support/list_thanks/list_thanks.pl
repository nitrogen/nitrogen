#!/usr/bin/perl

## Just a manual list of names to ignore from the git history (usually mistakes of some time or another)

local %filter = (
	"root" => 1,
	"unknown" => 1,
	"tempaccounterl" => 1,
	"" => 1,
	" " => 1,
);

local %remap = (
	"Russell Klophaus" => "Rusty Klophaus",
	"rklophaus" => "Rusty Klophaus",
	"dkpsystem" => "Jesse Gumm",
	"Sidello Website" => "Jesse Gumm",
	"Bracketpal User" => "Jesse Gumm",
	"Nitrogen User" => "Jesse Gumm",
	"fbrau" => "Franklin Brauning",
	"andreas" => "Andreas Hasselberg",
	"stuart" => "Stuart Thackray",
	"stuart-thackray" => "Stuart Thackray",
	"JonGretar" => "Jon Gretar Borhthorsson",
	"allanstreib" => "Allan Streib",
);


## This tells perl to automatically flush output

local $pwd = `pwd`;
chomp($pwd);
local $git_cmd = "git log --shortstat --date=short | sed '/^commit/d' | sed '/^ /d' | sed '/^\$/d'";
local %authors = {};

&do_repo("");
&do_repo("nitrogen_core");
&do_repo("simple_bridge");
&do_repo("nprocreg");
&do_repo("mutagen");
&do_repo("nitro_cache");
&do_repo("canister");
&do_repo("rekt");
&do_repo("NitrogenProject.com");

foreach $key (sort compare_date (keys(%authors))) {
	my $name = $authors{$key}->{author};
	#my $commits = $authors{$key}->{commits};
	#my $date = $authors{$key}->{date};
	print "$name\n" if not exists($filter{$name});
}


sub compare_date {
	return $authors{$a}->{date} cmp $authors{$b}->{date}
}

sub compare_commits {
	return $authors{$a}->{commits} cmp $authors{$b}->{commits};
}

sub normalize_author_key {
	my($author) = @_;
	$author =~ s/[^\w]//gi;
	return $author;
}

## It's nasty, but this just relies on the %authors hash
sub do_repo {
	my ($repo) = @_;
	chdir "$pwd/$repo";
 	my @lines = `$git_cmd`;
	my $author_key = "";
	my $author_name = "";
	for(@lines) {
		if(/^Author:\s*(.*?)\s<.*>$/) {
			$author_name=$1;
			if(exists($remap{$author_name})) {
				$author_name = $remap{$author_name}
			}
			$author_key = &normalize_author_key($author_name);
		}
		elsif(/^Date:\s*(.*)$/) {
			my $date = $1;
			
			if(exists($authors{$author_key})) {
				if($date lt $authors{$author_key}->{date}) {
					$authors{$author_key}->{date} = $date
				}
				$authors{$author_key}->{commits}++;
			}else{
				$authors{$author_key} = { author=>$author_name, date=>$date, commits => 1};
			}
		}
	}
}		


