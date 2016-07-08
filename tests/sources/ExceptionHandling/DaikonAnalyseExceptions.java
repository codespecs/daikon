package ExceptionHandling;

public class DaikonAnalyseExceptions {
	
	public int classVar = 42;
	
	public boolean isEvan(int parameter){
		return ((parameter % 2) == 0);
	}	
	
	public boolean isFiveTimes(int parameter){
		return ((parameter % 5) == 0);
	}
	
	
	/**
     * Throws one Exception.
     */
	public int testCase1(int parameter) throws Exception{
		if((parameter % 5) == 0){
			throw new Exception();
		}
		if((parameter % 2) == 0){ //even
			return parameter;
		}else{
			return parameter*parameter;
		}
	}
	
	
	/**
     * Throws one derivated Exception.
     */
	public int testCase2(int parameter) throws Exception{
		int x = 42 / (parameter % 5);
		if((parameter % 2) == 0){ //even
			return parameter;
		}else{
			return parameter*parameter;
		}
	}	
	
	/**
     * Catches a derivated Exception.
     */
	public int testCase3(int parameter){
		try{
			return testCase1(parameter);
		}catch(Exception e){
			return 0;
		}
	}
	
	/**
     * Throws an Exception and one derivated Exception, which
     * are both be catched in the same Method. 
     * 
     * Creates an Exit-PPT and ThrowNN-PPT in the same call.
     *  --> error! dont call!
     */
	public int testCase4(int parameter){
		try{
			if((parameter % 5) == 0){
				throw new Exception();
			}
			int x = 42 / (parameter % 5);
			if((parameter % 2) == 0){ 
				return parameter;
			}else{
				return parameter*parameter;
			}
		}catch(Exception e){
			return 0;
		}
	}
	
	/**
     * Throws an Exception and one derivated Exception,
     */
	public int testCase5(int parameter) throws Exception{
		if((parameter % 5) == 0){
			throw new Exception();
		}
		int x = 42 / (parameter % 6);
		if((parameter % 2) == 0){ //even
			return parameter;
		}else{
			return parameter*parameter;
		}
	}
	
	/**
     * Throws two Exceptions.
     * Creates Two THROWNN-PPTs.
     */	
	public int testCase6(int parameter) throws Exception{
		if((parameter % 5) == 0){
			throw new Exception();
		}
		if((parameter % 6) == 0){
			throw new Exception();
		}
		if((parameter % 2) == 0){ //even
			return parameter;
		}else{
			return parameter*parameter;
		}
	}

	///**
	 //* check the Valid of the new created Stackframe Table
	 //*/
	//public int paratest(int i , int[] ia , Integer in, Integer[] inA, float flo, float[] floA, Float floa, Float[] floaA){
////		char cc = 'C';
////		int[] iii = {3,4,5};
////		int blub= 6;
////		Integer john = 45;
////		Integer[] huhu = {13,12};
////		float bu = 0.5f;
////		Float hehe = 4545.0f;
////		boolean b = true;
////		byte by = 0x4;
////		char c = 'C';
////		short s = 7;

		//for (int i1 = -2; i1 <= 2; i1++) {
			//i;
		//}
		//return  47;
	//}
}
