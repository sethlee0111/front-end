MATCH (x0)-[:p16|p16|p22*]->(x1), (x1)<-[:p20]-()-[:p19]->()<-[:p20]-()-[:p15]->(x2), (x0)-[:p16|p17*]->(x3), (x3)-[:p16|p16*]->(x2) RETURN DISTINCT x0, x2 UNION ;
