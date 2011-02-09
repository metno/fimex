AM_CONDITIONAL([HAVE_LIBPQ], [test "x$with_pq" != xno])
if HAVE_LIBPQ
    USE_GRIDEXER = gridexer
endif
SUBDIRS = fimex kdtree++ felt $(USE_GRIDEXER)
