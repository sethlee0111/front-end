MATCH (x0)<-[:p30]-()-[:p57]->()-[:p56]->(x1), (x1)<-[:p56]-()-[:p54]->()<-[:p54]-()-[:p63]->(x2), (x0)<-[:p22]-()-[:p54]->()<-[:p54]-()-[:p51]->(x3), (x3)<-[:p51]-()<-[:p69]-()-[:p50]->(x2) RETURN DISTINCT x2, x0, x1;
