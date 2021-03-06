% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sparkler.R
\name{sparkler.crawl}
\alias{sparkler.crawl}
\title{Launch a crawl}
\usage{
sparkler.crawl(vm, url, topUrls, topGroups, maxIter, debug = FALSE,
  mode = "default")
}
\arguments{
\item{vm}{The Instance object}

\item{url}{URL website to crawl}

\item{topUrls}{Number of URLs in each website}

\item{topGroups}{Number of hosts to fetch in parallel.}

\item{maxIter}{Number of iterations to run.}

\item{debug}{If TRUE, will see debug messages.}

\item{mode}{Choose your delays (default:1000ms,fast:500ms,turbo:100ms) between two fetch requests for the same host}
}
\value{
The crawl Id
}
\description{
Launch a crawl
}
\details{
Check if Docker exists and running
- If not, we create the docker with Sparkler with the "docker run" command
- If exists, we restart it
Next, we use "sparkler crawl" to inject URL parameters in Sparkler and launch a crawl.

Very important: Sparkler developers slow down the crawl to avoid getting blocked from the websites.
Top groups = number of hosts to fetch in parallel.
Top N = number of URLs in each website.
By default, it tries for 256 groups and 1000 URLs in each group
}
\examples{

\dontrun{

library(RsparkleR)

ovh <- import_ovh()
client <- load_client(ovh, endpoint, application_key, application_secret, consumer_key)

sshPubKeyPath  <- 'C:/Users/vterrasi/.ssh/id_rsa.pub'
sshPrivKeyPath <- 'C:/Users/vterrasi/.ssh/id_rsa'

vm <- sparkler.create(client, regionVM="UK1", typeVM="s1-4", sshPubKeyPath, sshPrivKeyPath)

sparkler.start(vm, debug)
url <- "https://www.YOUR WEBSITE.com"
pattern <- "www.YOUR WEBSITE.com"

topN <- 1000
maxIter <- 100;
topGroups <- 2

crawlid <- sparkler.crawl(vm, url, topN, topGroups, maxIter, debug=FALSE, mode="fast")

}

}

