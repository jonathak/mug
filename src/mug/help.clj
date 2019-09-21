(ns mug.help
  (:gen-class))

(def t-help
"
 .h              this help message
 .doc            open Mud documentation
 .q              to quit.
 .w low high     create a fresh set, window of mkt caps
 .b              bag, create a bag by listing tickers
 .l              list named bags and pairs
 <name>          load symbol or named set or pair 
 .noisy          (state: show external data-usage alerts)
 .quiet          (state: hide external data-usage alerts: default)
 .sl <cname>     case sensitive symbol lookup by company name
 .eu             edit universe
 .su             show universe
 <ticker> <cmd>  evaluates command (cmd) of name
 .p <t> <t>      create un-named pair (not yet implimented)
 .dow            create bag of dow stocks
 \n\n")

(def u-help 
"
 .h            this help message
 .su           show universe
 .cl           clear universe
 .li           list all industries
 .si           search industries
 .ai abc       add industry abc to the universe
 .q            quit Mug!
 .u            up to top level
\n\n")

(def s-help-sub
"
What do you want help with?
1 quant functions
2 web nav
3 explore
4 company info
5 historical data
6 Mug navigation
> ")

(def s-help-sub-quant
" .emp   employees
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
 .cc    ceo-compensation
 .mvs   total abs normalized gap moves
 .tvs   top volume spike
")

(def s-help-sub-web
" .oweb  open company website
 .yahoo open yahoo finance
 .sec   open company sec filings
")

(def s-help-sub-find
" .sp   splits
 .cff  cash flow financing
 .mov  top 5 normalized gap moves
 .vs   top 5 volume spikes
")

(def s-help-sub-info
" .cname company name
 .web   company website
 .desc  description
 .i     industry
 .s     sector
 .ceo   ceo
 .ir    insider roster
 .db    deep book
 .sp    splits
 .fo    fund ownership
 .cff   cash flow financing
 .io    institutional-ownership
 .zc    zipcode (sec web scrape, very slow)
 .mov   top 5 normalized gap moves
 .vs    top 5 volume spikes
 .news  company news
")

(def s-help-sub-history
" .pr     prices 1D
 .pr x   prices 15m 5m 1m
 .fpr    fresh prices 1D
 .fpr x  fresh prices 15m 5m 1m
 .sp     splits
 .cff    cash flow financing
 .mov    top 5 normalized gap moves
 .vs     volume spikes (top 5)
")

(def s-help-sub-nav
"<name> load symbol or named set or pair 
 .u    up to bag or top
 .n    list named sets and pairs
 .l    list members of loaded set or pair
 .q    to quit.
")

(def s-help s-help-sub)

(def b-help-sub
"
What do you want help with?
1 modifying bags
2 creating tables
3 Mug navigation
> ")

(def b-help-bag
" .sl <cname>   symbol lookup by company name
 .w low high   refresh un-named set, window of mkt caps
 .b            bag, create a new bag (set) by listing tickers
 .a            add a ticker or tickers to the bag
 .d            delete a ticker from the bag
 .count        # of companies in bag
 .n            list named sets and pairs
 .l            list members of loaded set or pair
 .s <name>     name the current set or pair
")

(def b-help-table
" .m            maps functions onto the bag (table creation)
 .cmds         available functions
 .count        # of companies in bag
 .srt          sorts (reverse) a .m-generated table by first data column
 .noisy        (state: show data usage points)
 .quiet        (state: hide data usage points)
 .l            list members of loaded set or pair
 .kt number    keep top number of companies (includes .srt)
 .dt number    drop top number of companies (includes .srt)
")

(def b-help-nav
" <name>        load symbol or named set or pair 
 <name> <cmd>  evaluates command (cmd) of name
 .cmds         list of commands of the type <name> <cmd>
 .h            this help message
 .doc          open Mud documentation
 .sl <cname>   symbol lookup by company name
 .u            up to top
 .p <t> <t>    create un-named pair (not yet implimented)
 .n            list named sets and pairs
 .l            list members of loaded set or pair
 .s <name>     name the current set or pair
 .q            to quit.
")

(def cmds
"
 emp   employees
 so    sharesoutstanding
 pf    publicfloat
 v     avg30Volume
 b     beta
 mkt   marketcap
 c     cash
 d     debt
 r     revenue
 g     gross
 e     ebitda
 d2e   dbtoeqty
 cc    ceo-compensation
 mov   top 5 normalized gap moves
 mvs   total abs normalized gap moves
 vs    top 5 volume spikes
 tvs   amount of top volume spike
 cname company name
 web   company website
 i     industry
 s     sector
 ceo   ceo
 ceoweb opens top google hit for ceo's name
 cff   aggregate cash flow financing
 zc    zipcode
 ctg   open clin trials gov
")