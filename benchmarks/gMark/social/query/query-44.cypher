MATCH (x0)<-[:p6]-()-[:p21]->()<-[:p21]-()<-[:p2]-(x1), (x1)-[:p15|p15*]->(x2), (x2)-[:p26]->()<-[:p20]-()-[:p21]->()<-[:p16]-(x3) RETURN DISTINCT x3, x0, x1, x2;