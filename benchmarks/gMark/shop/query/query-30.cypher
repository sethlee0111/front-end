MATCH (x0)-[:p76|p76*]->(x1), (x0)-[:p76]->()-[:p67]->()-[:p54]->()<-[:p54]-(x2), (x2)-[:p54|p76|p79*]->(x1) RETURN DISTINCT x2, x0, x1;
