# Build Join Graph Using The Cypher Front End Modules

## Quick Start
Navigate to `out/artifacts/front_end_9_0_2_13_jar/front-end-9.0_2.13.jar` and execute the following command to generate hypergraph from a Cypher query. (This is tested using openjdk 11.0.15 2022-04-19 LTS)

```
$ java -cp front-end-9.0_2.13.jar org.opencypher.v9_0.frontend.join_graph_analytics.Hypergraph [path_to_cypher_query_file]
```

In the same directory, you can also process the entire queries for LSQB and gMark-shop at once by running the bash scripts.

```
$ bash lsqb.sh
```

```
$ bash gMark-shop.sh
```