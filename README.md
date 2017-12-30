# RsparkleR

`RsparkleR` provides an R interface for launching virtual machines and deploying Sparkler as painless as possible with a few lines from your local R session.

Sparkler (contraction of Spark-Crawler) is a new web crawler that makes use of recent advancements in distributed computing and information retrieval domains by conglomerating various Apache projects like Spark, Kafka, Lucene/Solr, Tika, and Felix.

> See all documentation on the [sparkler website](https://github.com/USCDataScience/sparkler)

## Creating a Sparkler Cluster

Detailled instructions here : https://data-seo.com/2017/12/17/advanced-r-programming-seo-crawler

1. Configure a OVH Cloud Project with billing
https://api.ovh.com/createToken/index.cgi?GET=/*&POST=/*&PUT=/*&DELETE=/*

2. Create your SSH keys : sshPubKeyPath, sshPrivKeyPath

3. Put your regionVM ( SBG3,BHS3,WAW1,UK1,DE1,GRA3)

- SBG3 Datacenter is in France
- BHS3 Datacenter is in Canada
- WAW1 Datacenter is in Poland
- UK1  Datacenter is in UK
- DE1  Datacenter is in Deutch
- GRA3 Datacenter is in France

4. Put your typeVM (s1-2,s1-4,...) and SSH Key
About range of cloud servers : https://www.ovh.co.uk/public-cloud/instances/prices/

5. Run `library(RsparkleR)` 
5. `ovh <- importOvh()` 
6. `client <- loadClient(ovh,endpoint,application_key,application_secret,consumer_key)` 
7. Run `vm <- createSparkler(client,regionVM='UK1',typeVM='s1-4',sshPubKeyPath,sshPrivKeyPath)`
8. Wait for it to install and your instance is ready, you get vm object with ip and port 22 is open
9. Now you can deploy your Sparkler 
10. Deploy your Docker with Sparkler : Run `startSparkler(vm, prod=TRUE, debug=TRUE)`. Be patient for the first time.
11. Launch crawl : `crawlid <- startCrawl(vm, url="https://data-seo.com", topUrls=100, topGroups=5, maxIter=2, debug=TRUE)`
12. Get results from SolR `crawlDF <- readSolr(vm, pattern, crawlid, topUrls=100, extracted=TRUE)`


## Thanks to

* Thamme Gowda and USC Data Science ( http://irds.usc.edu ) for creating Sparkler
* Mark Edmondson for the [googleComputeEngineR]() package for providing an R interface to the Google Cloud Compute Engine API, for launching virtual machines.
* Scott Chamberlin for the [analogsea](https://github.com/sckott/analogsea) package for launching Digital Ocean VMs, which inspired the SSH connector functions for this one.
* Winston Chang for the [harbor](https://github.com/wch/harbor/) package where the docker functions come from.  If `harbor` will be published to CRAN, it will become a dependency for this one.

## Install

Github

```r
library(devtools)
install_github("voltek62/RsparkleR")
```

CRAN version:

Waiting...
