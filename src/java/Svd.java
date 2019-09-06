import Jama.*;

public class Svd {


   public static Double[] hole(Double[] data) {
    data[1] = 8.8;
    return data;
   }

   public static Double[] uuu(Double[] booger, Long by) {
     Matrix A;
     int len = booger.length;
     int iby = by.intValue();
     double[] columnwise = new double[len];
     for (int i=0; i< len; i++) {
       columnwise[i] = booger[i];
     }
     A = new Matrix(columnwise, iby);
     SingularValueDecomposition SVD = A.svd();
     columnwise = SVD.getU().getColumnPackedCopy();
     for (int i=0; i<9; i++) {
       booger[i] = columnwise[i];
     }
     return booger;
   }

   public static void test () {
     Matrix A;
     double[] columnwise = {1.,2.,-5.,1.,-2.,1.,3.,1.,-1.};
     print("\nTesting SVD...\n");
     A = new Matrix(columnwise,3);
     A.print(3,3);
     SingularValueDecomposition SVD = A.svd();
     SVD.getU().print(3,3);
     SVD.getS().print(3,3);
   }

   public static void main (String argv[]) {
     Matrix A, B;
     double[] columnwise = {1.,2.,-5.,1.,-2.,1.,3.,1.,-1.};
     double[] serialbooger = {0., 0., 0., 0., 0., 0., 0., 0., 0.};
     print("\nTesting SVD...\n");
     A = new Matrix(columnwise,3);
     B = new Matrix(3,3);
     A.print(3,3);
     SingularValueDecomposition SVD = A.svd();
     SVD.getU().print(3,3);
     SVD.getS().print(3,3);
   }

   /* private print functions */

   // shorten spelling
   private static void print(String s) 
     {System.out.print(s);}
     
   // row vector
   private static void print(double[] x, int w, int d) {
     // Use format Fw.d for all elements.
     print("\n");
     new Matrix(x,1).print(w,d);
     print("\n");
   }
}
