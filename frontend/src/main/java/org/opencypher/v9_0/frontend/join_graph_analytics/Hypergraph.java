package org.opencypher.v9_0.frontend.join_graph_analytics;

import org.apache.commons.lang3.ArrayUtils;

import java.io.*;
import java.util.ArrayList;
import java.util.Objects;
import java.util.Scanner;

public class Hypergraph {
    public static void main(String[] args) {
        if (args.length > 0) {
            String filename = args[0];
            String[] parsed = filename.split("\\.");
            if (Objects.equals(parsed[parsed.length - 1], "cypher")) {
                String query = "";
                String pathname = parsed[parsed.length-2];
                String[] parsed_pathname = filename.split("/");
                parsed_pathname[parsed_pathname.length-2] = "primal-graph";
                String graphName = parsed_pathname[parsed_pathname.length-1].split("\\.")[0];
                parsed_pathname = ArrayUtils.remove(parsed_pathname, parsed_pathname.length-1);
                String path = String.join("/", parsed_pathname);
                try {
                    File file = new File(filename);
                    Scanner scanner = new Scanner(file);
                    while (scanner.hasNextLine()) query = query.concat(scanner.nextLine() + "\n");

                } catch (FileNotFoundException e) {
                    System.out.println("File not found");
                }
                String ppg = JoinGraph.getPrettyPrintGraphFromQuery(query);
                ArrayList<String> hypergraph = ConstructPrimalGraph.read_hypergraph_from_string(ppg);
                System.out.println(hypergraph);
                System.out.println(graphName);
                System.out.println(path);
                writeHypergraph(hypergraph, parsed_pathname, graphName);
                ConstructPrimalGraph.construct_primalgraph(hypergraph, graphName, path);
            }
        }
    }

    public static void writeHypergraph(ArrayList<String> hypergraph, String[] parsed_pathname, String graphName) {
        parsed_pathname[parsed_pathname.length-1] = "hypergraph";
        String path = String.join("/", parsed_pathname);
        String filename = path + "/" + graphName + ".graph";
        try {
            PrintWriter writer = new PrintWriter(new FileWriter(filename));
            for (int j = 0; j < hypergraph.size(); j++) {
                String[] line = hypergraph.get(j).split(" ");
                writer.printf("%s (", line[0]);
                for (int i = 1; i < line.length; i++) {
                    if (i > 1) writer.printf(", ");
                    writer.printf(line[i]);
                }
                if (j == hypergraph.size() - 1) writer.printf(").");
                else writer.printf("),\n");
            }
            writer.close();
        }
        catch (IOException e) {
            e.printStackTrace();
        }
    }
}
