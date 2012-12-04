#! /usr/bin/perl -w
use ExtUtils::MakeMaker;
WriteMakefile(
    'VERSION' => 0.1,
    'NAME'    => 'Geo::Fimex',
    'INC'     => '-I../Fimex/include',
    'LIBS'    => ['-L../Fimex/src/.libs -lfimex'],
    'OBJECT'  => 'fimex_wrap.o'  # Object files
);


