 Installation for running Cytoscape from R tutorial:
 
 Requirements:
 
- Java 8 (http://www.oracle.com/technetwork/java/javase/downloads/jdk8-downloads-2133151.html)
- Cytoscape 3.2.1 or later (http://www.cytoscape.org/download.php)
- CyREST (used to communicate between Cytoscape and R): 
    - Start Cytoscape
    - Apps → App Manager
    - Select cyREST
    - Click install
  
- R packages: RJSONIO and igraph
    - from R run: ```install.packages(c("RJSONIO", "igraph"))```