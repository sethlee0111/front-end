MATCH (x0)<-[:p51]-()-[:p57]->()<-[:p57]-()-[:p57]->(x1), (x1)<-[:p76]-()-[:p76]->()-[:p72]->()-[:p78]->(x2), (x2)-[:p79|p78|p78*]->(x3) RETURN DISTINCT x3, x2, x1, x0;