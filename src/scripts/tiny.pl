@ARGV = qw(.) unless @ARGV;
use File::Find;

find sub { print $File::Find::name, -d && '/', "\n" }, @ARGV;

# use File::Find::Rule;

# my @files = File::Find::Rule->file()
#                             ->name( '*.purs' )
#                             ->in( './src/' );
