package yatze;

import java.util.Arrays;
import java.util.LinkedList;

public class Main {

        public static boolean est_full(int[] ds) {
                Arrays.sort(ds);
                return (ds[0] == ds[1] && ds[1] == ds[2] && ds[3] == ds[4]);
        }

        public static boolean est_suites(int[] ds) {
                Arrays.sort(ds);
                return (ds[0] == (ds[1] - 1) && ds[1] == (ds[2] - 1) && ds[2] == (ds[3] - 1) && ds[3] == ds[4] - 1);
        }

        public static void main(String[] args) {
                LinkedList<int[]> full = new LinkedList<>();
                LinkedList<int[]> suites = new LinkedList<>();

                for (int d0 = 1; d0 <= 6; d0++) {
                        for (int d1 = 1; d1 <= 6; d1++) {
                                for (int d2 = 1; d2 <= 6; d2++) {
                                        for (int d3 = 1; d3 <= 6; d3++) {
                                                for (int d4 = 1; d4 <= 6; d4++) {
                                                        int[] ds = {d0, d1, d2, d3, d4};
                                                        if (est_full(ds.clone()))
                                                                full.add(ds);
                                                        if (est_suites(ds.clone()))
                                                                suites.add(ds);
                                                }
                                        }
                                }
                        }
                }
                System.out.println("Fulls");
                for (int[]f: full) {
                        System.out.print("\t");
                        for (int i : f)
                                System.out.print(i + ";");
                        System.out.println();
                }
                System.out.println("Suites");
                for (int[]f: suites) {
                        System.out.print("\t");
                        for (int i : f)
                                System.out.print(i + ";");
                        System.out.println();
                }

        }

}
