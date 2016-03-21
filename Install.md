 Installation for running Cytoscape from R tutorial:
 
 Requirements:
 
- Java 8 (http://www.oracle.com/technetwork/java/javase/downloads/jdk8-downloads-2133151.html)
- Cytoscape 3.2.1 or later (http://www.cytoscape.org/download.php)
- CyREST (used to communicate between Cytoscape and R): 
    - Start Cytoscape
    - Apps → App Manager
    - Select cyREST
    - Click install
  
- R packages: RJSONIO, igraph, httr, dplyr, reshape2, Hmisc, vegan and RcolorBrewer
    - from R run (editing as necessary based on already installed packages): ```install.packages(c("RJSONIO", "igraph", "httr", "dplyr", "reshape2", "Hmisc", "vegan", "RColorBrewer"))```