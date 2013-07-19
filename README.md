Signalarium
===========

Signalarium is a system for programmatically exploring empirical information
about signaling pathways in biology.  The system encodes guidelines and
heuristics for semi-mechanically translating pathway models (such as those from
KEGG) into Kappa models, augmented by information acquired from across the
bioinformatica.  The system generates local databases for protein sets of
interest on the fly from authoritative public data services, and analyzes this
data programmatically to create instances of model families consistent with
published empirical consensus.

[globe]: https://raw.github.com/mnewberry/signalarium/master/data/misc/signalarium_sm.png

The software is distributed as an OCaml library, and the primary user interface
is the OCaml REPL as:

    bin/ocrun src/test.ml
    rlwrap ocaml -I ``find . -name '_build' -type d | head -1`` $@

with a `~/.ocamlinit` such as

    #use "topfind" ;
    #require "batteries" ;;
    #require "netclient" ;;
    #require "pxp" ;;
    #install_printer Pxp_document.print_node;;
    #install_printer Pxp_document.print_doc;;
    #require "parmap" ;;
    #require "sqlite3" ;;
    #load "util.cmo" ;;
    #load "xml.cmo" ;;
    #load "protein.cmo" ;;
    #load "db.cmo" ;;
    #load "db_sql.cmo" ;;
    #load "pdb.cmo" ;;
    #load "interval.cmo" ;;

Most immediately-useful functions are to be found in the `Query` module.  The
`Db_sql` module is required to instantiate a local database to query.  In most
cases, there is no need to populate the database.  You may readily query for
entries that are not in the database, such as 

    uniprot db "Q01617"

and all the required information will be retrieved from relevant web services.
