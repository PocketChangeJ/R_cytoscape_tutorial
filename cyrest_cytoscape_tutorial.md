# CyREST Cytoscape tutorial
Julia Gustavsen  
March 21, 2016  



# Installation

Any woes?

Make sure Cytoscape is open while running these commands in R!

# Make simple network in R

Load necessary libraries for the tutorial. 

```r
library(vegan)
library(Hmisc)
library(reshape2)
library(dplyr)
library(igraph)
library(RJSONIO)
library(httr)
```

Load dataset for use today

```r
data(dune)
dune
```

```
##    Achimill Agrostol Airaprae Alopgeni Anthodor Bellpere Bromhord Chenalbu
## 1         1        0        0        0        0        0        0        0
## 2         3        0        0        2        0        3        4        0
## 3         0        4        0        7        0        2        0        0
## 4         0        8        0        2        0        2        3        0
## 5         2        0        0        0        4        2        2        0
## 6         2        0        0        0        3        0        0        0
## 7         2        0        0        0        2        0        2        0
## 8         0        4        0        5        0        0        0        0
## 9         0        3        0        3        0        0        0        0
## 10        4        0        0        0        4        2        4        0
## 11        0        0        0        0        0        0        0        0
## 12        0        4        0        8        0        0        0        0
## 13        0        5        0        5        0        0        0        1
## 14        0        4        0        0        0        0        0        0
## 15        0        4        0        0        0        0        0        0
## 16        0        7        0        4        0        0        0        0
## 17        2        0        2        0        4        0        0        0
## 18        0        0        0        0        0        2        0        0
## 19        0        0        3        0        4        0        0        0
## 20        0        5        0        0        0        0        0        0
##    Cirsarve Comapalu Eleopalu Elymrepe Empenigr Hyporadi Juncarti Juncbufo
## 1         0        0        0        4        0        0        0        0
## 2         0        0        0        4        0        0        0        0
## 3         0        0        0        4        0        0        0        0
## 4         2        0        0        4        0        0        0        0
## 5         0        0        0        4        0        0        0        0
## 6         0        0        0        0        0        0        0        0
## 7         0        0        0        0        0        0        0        2
## 8         0        0        4        0        0        0        4        0
## 9         0        0        0        6        0        0        4        4
## 10        0        0        0        0        0        0        0        0
## 11        0        0        0        0        0        2        0        0
## 12        0        0        0        0        0        0        0        4
## 13        0        0        0        0        0        0        0        3
## 14        0        2        4        0        0        0        0        0
## 15        0        2        5        0        0        0        3        0
## 16        0        0        8        0        0        0        3        0
## 17        0        0        0        0        0        2        0        0
## 18        0        0        0        0        0        0        0        0
## 19        0        0        0        0        2        5        0        0
## 20        0        0        4        0        0        0        4        0
##    Lolipere Planlanc Poaprat Poatriv Ranuflam Rumeacet Sagiproc Salirepe
## 1         7        0       4       2        0        0        0        0
## 2         5        0       4       7        0        0        0        0
## 3         6        0       5       6        0        0        0        0
## 4         5        0       4       5        0        0        5        0
## 5         2        5       2       6        0        5        0        0
## 6         6        5       3       4        0        6        0        0
## 7         6        5       4       5        0        3        0        0
## 8         4        0       4       4        2        0        2        0
## 9         2        0       4       5        0        2        2        0
## 10        6        3       4       4        0        0        0        0
## 11        7        3       4       0        0        0        2        0
## 12        0        0       0       4        0        2        4        0
## 13        0        0       2       9        2        0        2        0
## 14        0        0       0       0        2        0        0        0
## 15        0        0       0       0        2        0        0        0
## 16        0        0       0       2        2        0        0        0
## 17        0        2       1       0        0        0        0        0
## 18        2        3       3       0        0        0        0        3
## 19        0        0       0       0        0        0        3        3
## 20        0        0       0       0        4        0        0        5
##    Scorautu Trifprat Trifrepe Vicilath Bracruta Callcusp
## 1         0        0        0        0        0        0
## 2         5        0        5        0        0        0
## 3         2        0        2        0        2        0
## 4         2        0        1        0        2        0
## 5         3        2        2        0        2        0
## 6         3        5        5        0        6        0
## 7         3        2        2        0        2        0
## 8         3        0        2        0        2        0
## 9         2        0        3        0        2        0
## 10        3        0        6        1        2        0
## 11        5        0        3        2        4        0
## 12        2        0        3        0        4        0
## 13        2        0        2        0        0        0
## 14        2        0        6        0        0        4
## 15        2        0        1        0        4        0
## 16        0        0        0        0        4        3
## 17        2        0        0        0        0        0
## 18        5        0        2        1        6        0
## 19        6        0        2        0        3        0
## 20        2        0        0        0        4        3
```

Make correlation network from vegan's dune dataset. How correlated are these different species across all these different sites? We will get a matrix will correlation values and we will visualize this as a network in Cytoscape. 


```r
dune_cor <- rcorr(as.matrix(dune),
                  type="spearman") 

correlations <- dune_cor$r
p_values <- dune_cor$P

## get rid of the upper section by turning to NA. 
correlations[upper.tri(correlations )] <- NA
p_values[upper.tri(p_values)] <- NA

melted_cor <- melt(correlations)
melted_p <- melt(p_values)

melted_together <- cbind(melted_p$value,
                         melted_cor)

melted_together <- na.omit(melted_together) ## gets rid of the leftover diagonals
names(melted_together)
```

```
## [1] "melted_p$value" "Var1"           "Var2"           "value"
```

```r
names(melted_together)[1] <- "p_value"
names(melted_together)[4] <- "weight"
```

Want to keep only the strong correlations for visualization in our network.

```r
#filter for correlation about 0.6 and p value less than 0.01
filtered_data <- filter(melted_together,
                        p_value <= 0.01  & abs(weight) > 0.6)

filtered_data <- subset(filtered_data,
                        select=c(Var1,
                                 Var2,
                                 weight))
filtered_data 
```

```
##        Var1     Var2     weight
## 1  Agrostol Achimill -0.6693543
## 2  Anthodor Achimill  0.6466069
## 3  Bromhord Achimill  0.6395448
## 4  Anthodor Agrostol -0.6031567
## 5  Planlanc Agrostol -0.6686553
## 6  Ranuflam Agrostol  0.6764412
## 7  Scorautu Agrostol -0.6265462
## 8  Empenigr Airaprae  0.7254763
## 9  Hyporadi Airaprae  0.8167949
## 10 Planlanc Anthodor  0.6371434
## 11 Bromhord Bellpere  0.6879885
## 12 Juncarti Eleopalu  0.7015024
## 13 Ranuflam Eleopalu  0.8699118
## 14 Callcusp Eleopalu  0.7130738
## 15 Hyporadi Empenigr  0.6085806
## 16 Ranuflam Juncarti  0.6268941
## 17  Poaprat Lolipere  0.8659608
## 18 Trifprat Planlanc  0.7274929
## 19 Callcusp Ranuflam  0.6749409
## 20 Trifprat Rumeacet  0.8167620
```


Make a network from the dataframe using igraph.

```r
graph <- graph.data.frame(filtered_data,
                          directed=FALSE)
```


We can quickly visualize the network using igraph.

```r
plot(graph) # using igraph
```

![](cyrest_cytoscape_tutorial_files/figure-html/unnamed-chunk-6-1.png)


# What is REST? 

- REST: representational state transfer
- uses http ports to send and receive data
- helpful websites: http://www.pgbovine.net/rest-web-api-basics.htm
- uses verbs used in http to receive and send data
- verbs we will use today are:
    - GET - read the resource
    - POST - send the resource
    - PUT - update the resource
    - DELETE - deletes specified resource

We will be using [cyREST](https://github.com/idekerlab/cyREST/wiki) today to send and receive data from R to Cytoscape. 


# Setting up your R script


```r
source("./cyrest_cytoscape_functions.R")
# Basic settings

## The port number can be customized in cytoscape if desired. Would need to use Cytoscape
## Preference Editor (Edit <- Preferences) and modify "rest.port" to your desired port. 
port.number = 1234

resetCytoscapeSession(port.number) # just to make sure you are using a clean Cytoscape
## removes all previous networks that were loaded in that session.

base.url = paste("http://localhost:",
                 toString(port.number),
                 "/v1", sep="")
base.url
```

```
## [1] "http://localhost:1234/v1"
```

```r
version.url = paste(base.url,
                    "version",
                    sep="/")

cytoscape.version = GET(version.url)

cy.version = fromJSON(rawToChar(cytoscape.version$content))
cy.version
```

```
##       apiVersion cytoscapeVersion 
##             "v1"          "3.3.0"
```


# Send network to Cytoscape


```r
cygraph <- toCytoscape(graph) ## sends an igraph object to Cytoscape
```

```
## [1] "Done.  To json Start..."
```

```r
network.url <-  paste(base.url,
                      "networks",
                      sep="/")
res <- POST(url=network.url,
            body=cygraph,
            encode="json")
```


# Check out network in Cytoscape

- what are the main views of the network
- where can you see the data? etc
- how can you change how it looks?


Will appear in Cytoscape. Will not have any fancy styles and might actually just look like one square since all of the nodes will be on top of each other. We will play around with the network in Cytoscape for a few minutes and look at the Node Table, the Edge Table and the Style panel. 

Could show something like Style <- Edge <- Width <- weight <- "Continuous mapping" (and can then click on "Create Legend" to get a legend that maps to the style you have created.)

# Show how this style info is stored in the REST API


```r
## look at default
default.style.url = paste(base.url,
                          "styles/default",
                          sep="/")
GET(url=default.style.url)
```

```
## Response [http://localhost:1234/v1/styles/default]
##   Date: 2016-03-21 17:55
##   Status: 200
##   Content-Type: application/json
##   Size: 9.83 kB
```

```
## No encoding supplied: defaulting to UTF-8.
```

```
## {
##   "title" : "default",
##   "defaults" : [ {
##     "visualProperty" : "COMPOUND_NODE_PADDING",
##     "value" : 10.0
##   }, {
##     "visualProperty" : "COMPOUND_NODE_SHAPE",
##     "value" : "ROUND_RECTANGLE"
##   }, {
##     "visualProperty" : "DING_RENDERING_ENGINE_ROOT",
## ...
```

```r
default.style.url
```

```
## [1] "http://localhost:1234/v1/styles/default"
```

Navigate to this URL using your web browser. 


```r
# Extract SUID of the new network
network.suid <-  unname(fromJSON(rawToChar(res$content)))
network.suid
```

```
## [1] 260
```

```r
# Apply a style
style.name <-  "MyFirstStyle"
apply.style.url <-  paste(base.url,
                          "apply/styles",
                          style.name,
                          toString(network.suid),
                          sep="/")
GET(apply.style.url)
```

```
## Response [http://localhost:1234/v1/apply/styles/MyFirstStyle/260]
##   Date: 2016-03-21 17:55
##   Status: 404
##   Content-Type: <unknown>
## <EMPTY BODY>
```

```r
# Edge Line Size Mapping
min.weight <-  min(edge.attributes(graph)$weight)
max.weight <-  max(edge.attributes(graph)$weight)

point1 <-  list(
  value = min.weight,
  lesser = "2.0",
  equal = "2.0",
  greater = "2.0"
)

point2 <-  list(
  value = max.weight,
  lesser = "20.0",
  equal = "20.0",
  greater = "20.0"
)

edge.width.continuous.points <- list(point1,
                                     point2)

edge.width <- list(
  mappingType="continuous",
  mappingColumn="weight",
  mappingColumnType="Double",
  visualProperty="EDGE_WIDTH",
  points = edge.width.continuous.points
)

mappings <- list(edge.width)

style <- list(title=style.name,
              mappings = mappings)

style.JSON <- toJSON(style)

style.url <-  paste(base.url,
                    "styles",
                    sep="/")
POST(url = style.url,
     body = style.JSON,
     encode = "json")
```

```
## Response [http://localhost:1234/v1/styles]
##   Date: 2016-03-21 17:55
##   Status: 201
##   Content-Type: application/json
##   Size: 25 B
```

```
## No encoding supplied: defaulting to UTF-8.
```

```r
apply.style.url <-  paste(base.url,
                          "apply/styles",
                          style.name,
                          toString(network.suid),
                          sep="/")
GET(apply.style.url)
```

```
## Response [http://localhost:1234/v1/apply/styles/MyFirstStyle/260]
##   Date: 2016-03-21 17:55
##   Status: 200
##   Content-Type: application/json
##   Size: 35 B
```

```
## No encoding supplied: defaulting to UTF-8.
```




# Add the metadata to the network in R


```r
data(dune.taxon)

## add the taxonomic information to the graph

V(graph)$Genus <- dune.taxon$Genus[match(V(graph)$name,
                                         row.names(dune.taxon))]

V(graph)$Family <- dune.taxon$Family[match(V(graph)$name,
                                           row.names(dune.taxon))]

V(graph)$Order <- dune.taxon$Order[match(V(graph)$name,
                                         row.names(dune.taxon))]

V(graph)$Subclass <- dune.taxon$Subclass[match(V(graph)$name,
                                               row.names(dune.taxon))]

V(graph)$Class <- dune.taxon$Class[match(V(graph)$name,
                                         row.names(dune.taxon))]

## We can also, for example, add the degree of each vertex to the graph
V(graph)$degree <- degree(graph)
```


# Send network to cytoscape again


```r
resetCytoscapeSession(port.number)
cygraph <- toCytoscape(graph) ## so what does this do?
```

```
## [1] "Done.  To json Start..."
```

```r
network.url <-  paste(base.url,
                      "networks",
                      sep="/")
res <- POST(url=network.url,
            body=cygraph,
            encode="json")

## each time you send a new network to cytoscape you need to do this
network.suid <-  unname(fromJSON(rawToChar(res$content)))
network.suid
```

```
## [1] 364
```

```r
# Apply force-directed layout
layout.params = list(
  name="unweighted",
  value=TRUE
)

layout.params.url = paste(base.url,
                          "apply/layouts/kamada-kawai/parameters",
                          sep="/")
PUT(layout.params.url,
    body=toJSON(list(layout.params)),
    encode = "json")
```

```
## Response [http://localhost:1234/v1/apply/layouts/kamada-kawai/parameters]
##   Date: 2016-03-21 17:55
##   Status: 200
##   Content-Type: application/json
## <EMPTY BODY>
```

```r
apply.layout.url = paste(base.url,
                         "apply/layouts/kamada-kawai",
                         toString(network.suid),
                         sep="/")
GET(apply.layout.url)
```

```
## Response [http://localhost:1234/v1/apply/layouts/kamada-kawai/364]
##   Date: 2016-03-21 17:55
##   Status: 200
##   Content-Type: application/json
##   Size: 30 B
```

```
## No encoding supplied: defaulting to UTF-8.
```


# Send style info to cytoscape

Node fill colour

Column <- "Class" <- Mapping Type = "Discrete Mapping"


```r
# Extract SUID of the new network
network.suid <-  unname(fromJSON(rawToChar(res$content)))
network.suid
```

```
## [1] 364
```

```r
# Apply a style
style.name <-  "new_style"
apply.style.url <-  paste(base.url,
                          "apply/styles",
                          style.name,
                          toString(network.suid),
                          sep="/")
GET(apply.style.url)
```

```
## Response [http://localhost:1234/v1/apply/styles/new_style/364]
##   Date: 2016-03-21 17:55
##   Status: 404
##   Content-Type: <unknown>
## <EMPTY BODY>
```

```r
## Could assign each Class a specific colour like this:

class.mappings <-  list()
class.mappings[[1]] <- list(key = "Monocots", value = "#33FF33")
class.mappings[[2]] <- list(key = "Bryophytes", value = "#FF3399")
class.mappings[[3]] <- list(key = "Dicots", value = "#0066FF")

node.colour <- list(
  mappingType="discrete",
  mappingColumn="Class",
  mappingColumnType="String",
  visualProperty="NODE_FILL_COLOR",
  map = class.mappings
)

mappings <- list(edge.width,
                 node.colour)

style <- list(title=style.name,
              mappings = mappings)

style.JSON <- toJSON(style)

style.url <-  paste(base.url,
                    "styles",
                    sep="/")
POST(url = style.url,
     body = style.JSON,
     encode = "json")
```

```
## Response [http://localhost:1234/v1/styles]
##   Date: 2016-03-21 17:55
##   Status: 201
##   Content-Type: application/json
##   Size: 22 B
```

```
## No encoding supplied: defaulting to UTF-8.
```

```r
apply.style.url <-  paste(base.url,
                          "apply/styles",
                          style.name,
                          toString(network.suid),
                          sep="/")
GET(apply.style.url)
```

```
## Response [http://localhost:1234/v1/apply/styles/new_style/364]
##   Date: 2016-03-21 17:55
##   Status: 200
##   Content-Type: application/json
##   Size: 35 B
```

```
## No encoding supplied: defaulting to UTF-8.
```




```r
## Or could use R to automate it a bit:
library(RColorBrewer)

unique_class_dune <- unique(dune.taxon$Class)

colour_class <- brewer.pal(length(unique_class_dune), "Accent")

class.mappings = list()

for (class in seq_along(unique_class_dune)){
  class.mappings[[class]] <- list(key = unique_class_dune[class], 
                                  value = unname(colour_class[class]))
}

node_colour_style = list(
  mappingType="discrete",
  mappingColumn="Class",
  mappingColumnType="String",
  visualProperty="NODE_FILL_COLOR",
  map = class.mappings
)

mappings = list(node_colour_style
)
```


# How can you use this to automate some of your analysis



## Save images from cytoscape

This will save your current network in Cytoscape as a png or a pdf. 

```r
network.image.url <-  paste(
  base.url,
  "networks",
  toString(network.suid),
  "views/first.png?h=1500",
  sep="/"
)

network.image.url
```

```
## [1] "http://localhost:1234/v1/networks/364/views/first.png?h=1500"
```

```r
network.image.url_pdf <-  paste(
  base.url,
  "networks",
  toString(network.suid),
  "views/first.pdf",
  sep="/"
)
network.image.url_pdf
```

```
## [1] "http://localhost:1234/v1/networks/364/views/first.pdf"
```

```r
download.file(network.image.url_pdf, "./figures/testing_out_cytoscape.pdf")
download.file(network.image.url, "./figures/testing_out_cytoscape.png")
```

![last figure](./figures/testing_out_cytoscape.png)

You can also save these sessions (if you want to come back to them or maybe you need to run some more complex exploratory analysis on them later)


```r
saveCytoscapeSession(filepath="./figures/networks_dune_cor.cys")
```

