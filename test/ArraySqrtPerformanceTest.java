
import java.util.ArrayList;
import java.util.List;

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author heikok
 */
public class ArraySqrtPerformanceTest {
    static double offset = 0;
    
    private static double transform(double old1, double old2) {
        double new1 = old1 + offset;
        double new2 = old2 - offset;
        if (offset < 1) {
        // normalize
        double norm = Math.sqrt((old1*old1 + old2*old2)/(new1*new1 + new2*new2));
            new1 *= norm;
            new2 *= norm;
        }
        return new1;
    }

    public static void calcByTransform(double[] values1, double[] values2) {
        for (int i = 0; i < 10; i++) {
            offset = i*.2;
            for (int j = 0; j < values1.length; j++) {
                values1[j] = transform(values1[j], values2[j]);
            }
	}
    }

    public static void printStat(List<Double> stats, int flop) {
        double min = stats.get(0);
        double max = stats.get(0);
        for (double stat : stats) {
            min = (stat < min) ? stat : min;
            max = (stat > max) ? stat : max;
        }
        double sum = 0;
        int num = 0;
        for (double stat : stats) {
            if (!((stat == min) || (stat == max))) {
                sum += stat;
                num++;
            }
        }
        double avg = sum / num;
        double var = 0;
        if (num > 1) {
            for (double stat : stats) {
                if (!((stat == min) || (stat == max))) {
                    double diff = avg - stat;
                    var += (diff * diff);
                }
            }
            //cerr << sqrt(var) << " " << num << " " << min << " " << max<< endl;
            var = Math.sqrt(var) / (num - 1);
        }
        int flops = (int) (flop / avg / 1000000);
        System.out.println(avg + "+-" + var + " " + flops + "MFLOPS");
    }

    
    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        int size = 1000000;
        if (args.length > 0) {
            size = new Integer(args[0]);
        }
        System.out.println("transform: " + size);
        double[] values1 = new double[size];
        double[] values2 = new double[size];
        for (int i = 0; i < size; i++) {
            values1[i] = 1;
            values2[i] = 1;
        }
        List<Double> stats = new ArrayList<Double>();
        for (int i = 0; i < 10; i++) {
            long start = System.currentTimeMillis();
            calcByTransform(values1, values2);
            long end = System.currentTimeMillis();
            stats.add((end-start)/1000.);
            System.out.println("Done "+i+": " + ((end-start)/1000.));
        }
       	int flop = size * 12 * 10; // 12 * 10 floating point operations in loop
	printStat(stats, flop);
    }

}
