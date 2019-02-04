```console
yum install -y R openssl-devel gi curl-devel curl 
yum install -y groupinstall Development
```

https://www.r-bloggers.com/list-of-user-installed-r-packages-and-their-versions/
```console
ip <- as.data.frame(installed.packages()[,c(1,3:4)])
rownames(ip) <- NULL
ip <- ip[is.na(ip$Priority),1:2,drop=FALSE]
print(ip, row.names=FALSE)
```

```
                      Package   Version
                     askpass       1.1
                  assertthat     0.2.0
                   backports     1.1.3
                   base64enc     0.1-3
                          BH  1.69.0-1
                       bindr     0.1.1
                    bindrcpp     0.2.2
                       callr     3.1.1
                   checkmate     1.9.1
                         cli     1.0.1
                       clipr     0.5.0
                  clisymbols     1.2.0
                  colorspace     1.4-0
                   compareDF     1.7.0
                      crayon     1.3.4
                   crosstalk     1.0.0
                        curl       3.3
 DataQualityCheckEuracAlpEnv     0.1.0
                        desc     1.2.0
                    devtools     2.0.1
                      digest    0.6.18
                       dplyr     0.7.8
                          DT       0.5
                    dygraphs   1.1.1.6
                    evaluate      0.12
                       fansi     0.4.0
                          fs     1.2.6
                      getopt    1.20.2
                     ggplot2     3.1.0
                          gh     1.0.1
                       git2r    0.24.0
                        glue     1.3.0
                      gtable     0.2.0
                       highr       0.7
                   htmlTable    1.13.1
                   htmltools     0.3.6
                 htmlwidgets       1.3
                      httpuv   1.4.5.1
                        httr     1.4.0
                     hwriter     1.3.2
                         ini     0.3.1
                    jsonlite       1.6
                       knitr      1.21
                    labeling       0.3
                       later     0.7.5
                    lazyeval     0.2.1
                    magrittr       1.5
                       mailR     0.4.1
                    markdown       0.9
                     memoise     1.1.0
                        mime       0.6
                     munsell     0.5.0
                     openssl     1.2.1
                    optparse     1.6.1
                      pillar     1.3.1
                    pkgbuild     1.0.2
                   pkgconfig     2.0.2
                     pkgload     1.0.2
                       plogr     0.2.0
                        plyr     1.8.4
                 prettyunits     1.0.2
                    processx     3.2.1
                    promises     1.0.1
                          ps     1.3.0
                       purrr     0.2.5
                 R.methodsS3     1.7.1
                        R.oo    1.22.0
                     R.utils     2.7.0
                          R6     2.3.0
                   rcmdcheck     1.3.2
                RColorBrewer     1.1-2
                        Rcpp     1.0.0
                     remotes     2.0.2
                    reshape2     1.4.3
                       rJava    0.9-10
                       rlang     0.3.1
                   rmarkdown      1.11
                   rprojroot     1.3-2
                  rstudioapi     0.9.0
                      scales     1.0.0
                 sessioninfo     1.1.1
                       shiny     1.2.0
                 sourcetools     0.1.7
                     stringi     1.2.4
                     stringr     1.3.1
                         sys       2.1
                      tibble     2.0.1
                       tidyr     0.8.2
                  tidyselect     0.2.5
                     tinytex      0.10
                     usethis     1.4.0
                        utf8     1.1.4
                 viridisLite     0.3.0
                     whisker     0.3-2
                       withr     2.1.2
                        xfun       0.4
                         XML 3.98-1.16
                       xopen     1.0.0
                      xtable     1.8-3
                         xts    0.11-2
                        yaml     2.2.0
                         zoo     1.8-4
```          
