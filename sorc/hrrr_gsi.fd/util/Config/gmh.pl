#!/usr/bin/perl
#
# Simple utility to parse gmake output and print error summary;
# loosely inspired on Perl's Harness module.
#
#----------------------------------------------------------------------------


use Env;                 # make env vars readily available
use Getopt::Std;         # command line options
use File::Basename

# Command line options
# --------------------
  getopts('Ahv');
  usage() if ( $opt_h || $#ARGV < 0 );

%DIR = {};

LINE:
    while ( <> ) {
      
	chomp;

        s/\'//g; s/\`//g;

        @tokens = split;

	if ( /Assert.pl/ && ( /WARNING/ || /ERROR/ || /MESSAGE/ )) {
	    $assertLine{$_} ++;
	}

        if ( /^gmake/ &&  /: Entering/ ) {

            $lev = substr($tokens[0],6,1); # assume 1-digit for now
            $dirn  = $tokens[3];
            $name = basename("$dirn");       

            $Dir{$dirn} = { name  => "$name", done  => 0, };

            unless ( $Lev{$dirn} ) { $Lev{$dirn} = $lev; } 

            push @seq, "$dirn";

	}
        elsif ( /^gmake/ && "$tokens[2]" eq "Error" ) {
            $name  = $tokens[1];
            $name =~ s/\[//g;
            $name =~ s/\]//g;
            if ("$tokens[4]" !~ "(ignored)" ) {
                 $Err{$dirn}++;
                 $Failed{$name}++;
	    } else {
                 $iErr{$dirn}++;
                 $Ignored{$name}++;
            }
        }
        elsif ( /^gmake/ && "$tokens[1]" eq '***' ) {
            $name  = $tokens[2];
            $name =~ s/\[//g;
            $name =~ s/\]//g;
            $Err{$dirn}++;
            $Failed{$name}++;
        }
        elsif ( /^gmake/ &&  /: Leaving/ ) {
            $dirn  = $tokens[3];
            $Dir{$dirn}->{done} = 1;
	}

    }


# Assert messages
# ---------------
  if ( $opt_A && %assertLine ) {
    print <<EOF;

                       ---------------
                       Assert Messages
                       ---------------

EOF
    foreach $val ( keys %assertLine ) {
	print " [$assertLine{$val}] $val\n";
    }
    print "\n";

  }

# Details
# -------
if ( $opt_v && @seq ) {
    print <<EOF;

                          --------
                          Packages
                          --------

         >>>> Fatal Error           .... Ignored Error

EOF
    foreach $dirn ( @seq ) {
        $n = $Lev{$dirn} - 1;
        $name = $Dir{$dirn}->{name};
        $nerr = $Err{$dirn};
        $nierr = $iErr{$dirn};
        unless ( $gotit{$dirn} ) {
            if ( $nerr ) {
                printf( " [%2d] >>>> ",$nerr);             
		for $i (1..$n) { print ">>>> "; }            
		print "$name\n";
            } elsif ( $nierr ) {
                printf( " [%2d] .... ",$nierr);             
		for $i (1..$n) { print ".... "; }            
		print "$name\n";
            } else {
                print " [ok]      ";             
		for $i (1..$n) {   print "|    "; }            
		print "$name\n";
            }
	}
        $gotit{$dirn} = 1;
    }

    print <<EOF;

                          -------
                          Summary
                          -------

EOF

}  

# Print summary
# -------------
  Summary();

exit 0;


#......................................................................

sub Summary {

#   Summary
#   -------
$n_error = 0; $n_ierror = 0; $n_total = 0;
foreach $dirn ( keys %Dir ) {
    $n_total++;
    if ( $Err{$dirn} ) {
	$n_error++;
    }
    if ( $iErr{$dirn} ) {
	$n_ierror++;
    }
}

if ($n_total > 0) {
    $percent = 100 * ( 1 - ($n_error / $n_total) );
} else {
    $percent = 100;
}

if ( $n_error+$n_ierror > 0 ) {
    print "Specify -v for additional details.\n" unless ( $opt_v );
}

if ( $n_error > 0 ) {
    print "FAILED";
    $nf = 0;
    foreach $name ( keys %Failed ) {
	print " $name";
        $nf++;
    }
    print " ($nf files in $n_error packages)\n";
}

if ( $n_ierror > 0 ) {
    print "IGNORED";
    $nf = 0;
    foreach $name ( keys %Ignored ) {
	print " $name";
        $nf++;
    }
    print " ($nf files in $n_ierror packages)\n";
}

if ( $n_error == 0 ) {
    print "All $n_total packages compiled successfully.\n";
} else {
    printf("Failed to compile %d/%d packages, %.2f%% okay.\n",
           $n_error, $n_total, $percent );
}

}

#......................................................................

sub usage {

   print <<"EOF";

NAME
     gmh.pl - Harness GNU Make Output
          
SYNOPSIS

     gmh.pl [-Ahv] filename(s)
          
DESCRIPTION

     Reads output of 'gmake install' and summarizes the results, e.g.

     FAILED  geopk.d m_gsiGuess.d m_gsiCheck.d
     Failed to compile 2/77 packages, 97.40% okay.

     The option -A give a list of Assert messages printed during compilation.
     The option -v gives details of what failed or has been ignored

EOF

  exit(1)

 }

