use 5.010;
use strict;
use warnings;
use Data::Dumper qw(Dumper);
use File::Find::Rule;
 
my %snippets;
my $snipCount = 0;
my $currentSnippetName = "";
my @snippetLines;
my @tokens;

my @files = File::Find::Rule->file()
                            ->name( '*.purs' )
                            ->in( './src/' );

for my $file (@files) {
    print "file: $file\n";
}


while (<@files>) {
  if (/Snippet_Start/../Snippet_End/) {   # snippets are between these delimiters
    next if /Snippet_Start/;              # we don't include the START delimiter
    if (/Snippet_End/ || eof) {           # if end of file and in middle of snippet, finish current snippet
      $snippets{$currentSnippetName} = [ @snippetLines ];
      @snippetLines = ();             # reset the accumulator to empty array
      next;                           # we don't include the END delimiters
    }   
    if (/Name: /) {                   # get name of snippet from line following /SNIPPET/
      $snipCount++;                   # start a new snippet
      @tokens = split(' ', $_);       # tokenize name line
      $currentSnippetName = $tokens[2]; # name is the third item
    } 
    else {                     
      push (@snippetLines, $_);       # we're in snippet, push this line onto accumulator;
    }
  }
}
 
# print Dumper \%snippets;

my $name;
my $i;
foreach $name ( keys %snippets ) {
    print "snippet: $name\n";
    open(FH, '>', $name.purs) or die $!;
    foreach $i ( 0 .. $snippets{$name}->$#* ) {
        print FH "$snippets{$name}[$i]";
    }
    close(FH);
}
