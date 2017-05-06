# Some demos related to the psychology replication crisis

### Author 
Mark Andrews. Twitter: [@xmjandrews](https://twitter.com/xmjandrews)

## Demos

1. [Why most published research findings are false](https://lawsofthought.shinyapps.io/false_discovery).
2. [How p-hacking can make almost anything significant](https://lawsofthought.shinyapps.io/p_hacking).
3. [How optional stopping can inflate Type I error rates](https://lawsofthought.shinyapps.io/optional_stopping).
4. [How lower powered studies can inflate the value of true effects](https://lawsofthought.shinyapps.io/power_failure).

## Source code

The demos above are all written in R and Shiny. The source code can be found at <https://github.com/lawsofthought/replication-crisis-demos>.

*Note*: To use shinyapps.io, I had to install package PKI and this lead to errors when I tried to install the usual way. This was fixed with

```
install.packages('PKI',,'http://www.rforge.net/')
```

