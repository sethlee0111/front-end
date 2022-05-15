package org.opencypher.v9_0.frontend.join_graph_analytics;
import java.util.*;

import javax.print.DocFlavor.STRING;

import java.io.StringReader;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.FileReader;
import java.io.FileNotFoundException;

import java.util.ArrayList;

public class ConstructPrimalGraph {

    static String TPC_H_dir = "TPC-HBenchmark/";
    static String JOB_dir = "JOBBenchmark/";
    static String MLK_dir = "MLKBenchmark/";

    public static void main(String[] args) {
        // ArrayList<String> hypergraph = read_hypergraph("TPC-HBenchmark/Hypergraph/q5.txt");
        // construct_primalgraph(hypergraph, "q5.txt");

        String input = display_option_and_get_input();
        String dir = "";

        if(!(input.compareTo("tpc_h") == 0 || input.compareTo("job") == 0 || input.compareTo("mlk") == 0))
        {
            System.out.println("Invalid Benchmark! Terminate.");
            return;
        }
        else
        {
            if(input.compareTo("tpc_h") == 0)
            {
                dir = TPC_H_dir;
            }
            else if(input.compareTo("job") == 0)
            {
                dir = JOB_dir;
            }
            else
            {
                dir = MLK_dir;
            }
        }
        ArrayList<String> files = get_hypergraph(dir);
        for(String file: files)
        {
            ArrayList<String> hypergraph = read_hypergraph(dir + "/Hypergraph/" + file + ".txt");
            construct_primalgraph(hypergraph, file, dir);
        }
    }

    public static String display_option_and_get_input()
    {
        System.out.println("Benchmark Options");
        System.out.println("TPC_H");
        System.out.println("JOB");
        System.out.println("MLK");

        Scanner console = new Scanner(System.in);
        System.out.print("Enter Benchmark: ");
        String input = console.nextLine().toLowerCase();

        return input;
    }

    public static ArrayList<String> get_hypergraph(String dir)
    {
        ArrayList<String> files = new ArrayList<>();
        File folder = new File(dir + "Hypergraph");
        File[] listOfFiles = folder.listFiles();

        for (int i = 0; i < listOfFiles.length; i++) {
            File file = listOfFiles[i];
            if (file.isFile() && file.getName().endsWith(".txt")) {
                if(file.getName().compareTo("scheme.txt") != 0)
                {
                    files.add(file.getName().substring(0, file.getName().length() - ".txt".length()));
                }
            }
        }

        return files;
    }

    public static ArrayList<String> read_hypergraph_from_string(String str) {
        ArrayList<String> hypergraph = new ArrayList<>();
        Scanner scanner = new Scanner(str);
        while (scanner.hasNextLine()) {
            String data = scanner.nextLine();
            // System.out.println(data);
            data = data.substring(0, data.length() - 2).replace(" (", " ");
            hypergraph.add(data);
        }
        return hypergraph;
    }

    public static ArrayList<String> read_hypergraph(String dir)
    {
        ArrayList<String> hypergraph = new ArrayList<>();

        try
        {
            File file = new File(dir);

            Scanner console = new Scanner(file);

            while(console.hasNextLine())
            {
                String data = console.nextLine();
                // System.out.println(data);
                data = data.substring(0, data.length() - 2).replace(" (", " ");
                hypergraph.add(data);
            }
            console.close();
        }
        catch(FileNotFoundException e)
        {
            System.out.println("An error occurred.");
            e.printStackTrace();
        }

        return hypergraph;
    }

    public static void construct_primalgraph(ArrayList<String> hypergraph, String filename, String dir)
    {
        ArrayList<Integer[]> edges = new ArrayList<>();
        int max = 0;
        for(String h: hypergraph)
        {
            String[] cur = h.split(" ");
            ArrayList<String> list = new ArrayList<>();
            String h_name = cur[0];

            for(int i = 1; i < cur.length; i++)
            {
                list.add(cur[i]);
            }
            Collections.sort(list);
            for(int i = 0; i < list.size() - 1; i++)
            {
                int left = Integer.parseInt(list.get(i).substring(1));
                max = Math.max(max, left);
                for(int j = i + 1; j < list.size(); j++)
                {
                    int right = Integer.parseInt(list.get(j).substring(1));
                    edges.add(new Integer[] {left, right});
                    max = Math.max(max, right);
                }
            }
        }
        Collections.sort(edges, new Comparator<Integer[]>() {
            @Override
            public int compare(Integer[] o1, Integer[] o2) {
                if(o1[0].compareTo(o2[0]) < 0)
                {
                    return -1;
                }
                else if(o1[0].compareTo(o2[0]) > 0)
                {
                    return 1;
                }
                else
                {
                    return o1[1].compareTo(o2[1]);
                }
            }
        });

        write_primal_graph(edges, max, filename, dir);
    }

    public static void write_primal_graph(ArrayList<Integer[]> edges, int max, String filename, String dir)
    {
        try
        {
            FileWriter  writer = new FileWriter(dir + "/" + filename + ".graph");
            writer.write("p " + max + " " + edges.size() + "\n");
            for(Integer[] edge: edges)
            {
                writer.write("e " + (edge[0] - 1) + " " + (edge[1] - 1) + "\n");
            }
            System.out.println("Successfully wrote to the file.");
            writer.close();
        }
        catch(IOException e)
        {
            System.out.println("An error occurred.");
            e.printStackTrace();
        }
    }
}