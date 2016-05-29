package ExceptionHandling;

public class ExceptionHTest {

	public static void wantOutput(boolean o, String output) {
		if(o){
			System.out.println(output);
		}
	}	
	
	/**
	 * calls all Methods.
	 * 
	 */
	public static void main(String[] args) {
		boolean wantOut = false;
		int start = 2;
		int ende = 31;
		DaikonAnalyseExceptions da = new DaikonAnalyseExceptions();
		for (int i = start; i < ende; i++) {
			da.classVar = i;
			try{
				wantOutput(wantOut, i +" : "+da.testCase1(i));
			}catch(Exception e){
				wantOutput(wantOut, i +" : testCase1 ERROR");
			}
			try{
				wantOutput(wantOut, i +" : "+da.testCase2(i));
			}catch(Exception e){
				wantOutput(wantOut, i +" : testCase2 ERROR");
			}
			try{
				wantOutput(wantOut, i +" : "+da.testCase3(i));
			}catch(Exception e){
				wantOutput(wantOut, i +" : testCase3 ERROR");
			}
			//try{
			//	wantOutput(wantOut, i +" : "+da.testCase4(i));
			//}catch(Exception e){
			//	wantOutput(wantOut, i +" : testCase4 ERROR");
			//}
			try{
				wantOutput(wantOut, i +" : "+da.testCase5(i));
			}catch(Exception e){
				wantOutput(wantOut, i +" : testCase5 ERROR");
			}
			try{
				wantOutput(wantOut, i +" : "+da.testCase6(i));
			}catch(Exception e){
				wantOutput(wantOut, i +" : testCase6 ERROR");
			}
		};
	}

}
