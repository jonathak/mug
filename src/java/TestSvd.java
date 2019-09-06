import Jama.*;

public class TestSvd {

   public static void main (String argv[]) {
     Matrix A;
     double[] columnwise = {1.,2.,-5.,1.,-2.,1.,3.,1.,-1.};
     print("\nTesting SVD...\n");
     A = new Matrix(columnwise,3);
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
