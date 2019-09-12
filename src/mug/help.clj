(ns mug.help
  (:gen-class))

(def t-help
"
 .h            this help message
 .doc          open Mud documentation
 .sl <cname>   case sensitive symbol lookup by company name
 .eu           edit universe
 .su           show universe
 .noisy        (state: show external data-usage alerts)
 .quiet        (state: hide external data-usage alerts: default)
 .w low high   create a fresh set, window of mkt caps
 .b            bag, create a set (bag) by listing tickers
 .a            add a ticker or tickers to the set (bag)
 .d            delete a ticker from the set (bag)
 <ticker> <cmd>  evaluates command (cmd) of name
 .l            list named sets and pairs
 .p <t> <t>    create un-named pair (not yet implimented)
 <name>        load symbol or named set or pair
 .q            to quit.\n\n")

(def u-help 
"
 .h            this help message
 .ai abc       add industry abc to the universe
 .li           list all industries
 .si           search industries
 .q            quit Mug!
 .u            up to top level
 .cl           clear universe\n\n")

(def s-help
"\n <name>        load symbol or named set or pair 
 .h            this help message
 .n            list named sets and pairs
 .l            list members of loaded set or pair
 .q            to quit.
 .cname company name
 .bas   basket
 .pr     prices 1D
 .pr x   prices 15m 5m 1m
 .fpr    fresh prices 1D
 .fpr x  fresh prices 15m 5m 1m
 .ngm   normalized gap moves
 .i     industry
 .web   website
 .oweb  open website
 .desc  description
 .ceo   ceo
 .s     sector
 .emp   employees
 .so    sharesoutstanding
 .pf    publicfloat
 .v     avg30Volume
 .b     beta
 .mkt   marketcap
 .c     cash
 .d     debt
 .r     revenue
 .g     gross
 .e     ebitda
 .d2e   dbtoeqty
 .ir    insider roster
 .db    deep book
 .sp    splits
 .fo    fund ownership
 .cff   cash flow financing
 .io    institutional-ownership
 .zc    zipcode
 .cc    ceo-compensation\n")

(def b-help
"
 <name>        load symbol or named set or pair 
 <name> <cmd>  evaluates command (cmd) of name
 .h            this help message
 .doc          open Mud documentation
 .cmds         list of commands of the type (cmd ticker)
 .sl <cname>   symbol lookup by company name
 .w low high   refresh un-named set, window of mkt caps
 .b            bag, create a bag (set) by listing tickers
 .a            add a ticker or tickers to the bag
 .d            delete a ticker from the bag
 .u            up to top
 .doc          Mug documentation
 .count        # of companies in bag
 .m            maps functions onto the bag (table creation)
 .srt          sorts (reverse) a .m-generated table by first data column
 .p <t> <t>    create un-named pair (not yet implimented)
 .noisy        (state: show data usage points)
 .quiet        (state: hide data usage points)
 .n            list named sets and pairs
 .l            list members of loaded set or pair
 .s <name>     name the current set or pair
 .q            to quit.\n\n")