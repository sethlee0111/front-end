package org.opencypher.v9_0.frontend.join_graph_analytics;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Scanner;

public class Hypergraph {
    public static void main(String[] args) {
        if (args.length > 0) {
            String query = args[0];
            String ppg = JoinGraph.getPrettyPrintGraphFromQuery(query);
            ArrayList<String> hypergraph = ConstructPrimalGraph.read_hypergraph_from_string(ppg);
            System.out.println(hypergraph);
            ConstructPrimalGraph.construct_primalgraph(hypergraph, "hypergraph", ".");
        }
    }
}
