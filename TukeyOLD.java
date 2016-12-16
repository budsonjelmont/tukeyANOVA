import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;


import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.DocumentBuilder;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;



import java.io.*;



/**
 * 
 * @author qinqin
 * method parseXML:@author kyu; sbeik
 *        parseXML modification: @author qinqin
 *        printOutputArray: @author sbeik
 *        printOutputArray modification @author qinqin
 *        wait: @author sbeik
 *        
 */

/**
 * 	Program TukeyOLD:
 * 		This program takes an XML document, output by Filemaker, gets all of the experiment 
 *      data from the document and reformats it. Generated peakarea.csv file contains peak area
 *      for all the peptides and timecourse.csv file contains the column names corresponding to
 *      the peakarea.csv. These two files will be used for input to a JAVA-TO-R interface. R
 *      calculates tuKey, ANOVA pvalue and Q value, and all the data was put in a 
 *      TukeyAnovaOutput.csv file. Filemaker will import the data to mark the significance of 
 *      each experiment contained therein.
 * 
 *	Class TukeyOLD:
 *		This class import XML document from Filemaker, generated peakarea.csv and peakarea.csv,
 *		and instatiates the TalkToR class, which will do the JAVA-TO-R interface, generate the
 *		final output file. 
 */ 

public class TukeyOLD {
	int num_timepoints = -1;                //These are set to their default values;so that when 	
	int num_replicates = -1;                //the command line is looked through and no Argument
	int min_replicates = -1;                //is there to set a certain variable, that variable will
	double sig_level=-1;                    //keep its default value. (Default value of -1 mean that	             
	String path_to_wait;                    //this variable will not be considered, so timepoints
    String[] col_name;                      //and replicates that=-1, even after reading the command                          
    String file_name;                        //line, are errors.
    String path_to_file;
    double[][] tukeyOutput;
    double min_peak_area = 0;
    boolean _error = false;
    int lf_weight = 1;
    /**
	 * @param possible command line arguments:
	 * 
	 * 	num_timepoints = the (max) number of timepoints at which replicates were tested
	 * 	num_replicates = the (max) number of replicates tested		 
	 * 	min_replicates = the minimum number of replicates desired with which to perform the significance test
	 *  sig_level=the significance level for TukeyHSD 
	 * All possible command line arguments:
	 * 
	 * filename pathToWait timepoints=x replicates=x significance=x min_replicates=x  
	 * 
	 * -> filename, pathToWait, timepoints, replicates, significance are NECESSARY, but these and the other paremeters, except filename
	 *    and pathToWait, can be put on the command line in any order
	 * 
	 */
    
    
    public static void main(String[] args) {
		if(args.length<5||args.length>9){
			System.out.println("Error: invalid number of parameters");
			return;
		}
    	   	
		TukeyOLD p=new TukeyOLD();
        p.path_to_wait=args[1];				
		p.wait(true);                           //set the program as busy=true as it calculates the Tukey
		
		//set the instance variables to their non-default values, if they were input
		for (int i=2; i<args.length;i++){            //start the 2nd argument -> this is where"=x" args start
			String[] list = args[i].split("=");      //look through each argument for what its setting and the intended value
			                                         //if a parameter is "param=", with no input number;
			if(list.length>1){                        //donot set that parameter
				if(list[0].equals("timepoints")){
					p.num_timepoints=Integer.parseInt(list[1]);
					
				}
				if(list[0].equals("replicates")){
					p.num_replicates=Integer.parseInt(list[1]);
					
				}
				if(list[0].equals("min_replicates")){
					p.min_replicates=Integer.parseInt(list[1]);
					
				}
				if(list[0].equals("label_free")){
					p.lf_weight = Integer.parseInt(list[1]);
				}
				if(list[0].equals("min_peak_area")){
					p.min_peak_area = Double.parseDouble(list[1]);
				}
				if(list[0].equals("significance")){
					p.sig_level=Double.parseDouble(list[1]);
					
				}
			}
		}
		
						
		
	    
		String[] forPTF = args[0].split("\\\\");
 		int endLen = forPTF[forPTF.length-1].length();
 		p.path_to_file = args[0].substring(0, args[0].length()-endLen);
		 		 		
 		String name=forPTF[forPTF.length-1];
  		String[] name2=name.split("\\.");
  		p.file_name=p.path_to_file+name2[0];
 		
  
 			
		//check that timepoints, replicates, significance level were set--these are NECESSARY to run the program		
		if(p.num_timepoints==-1||p.num_replicates==-1||p.sig_level==-1){
			System.out.println("Error:Invalid parameters");
			return;
		}
		
		try{
			System.out.println("Gathering data...");
				
			//get peak array;
			XMLToDataArray2 x = new XMLToDataArray2(p.num_timepoints, p.min_peak_area);
			double[][] data= x.parseFilemakerData(args[0]);
	      
			//System.out.println("# of row: "+p.matps.length+"# of col "+p.matps[0].length);
			//p.printOutputArray(data,"_peakarea.csv");
			
			System.out.println("Generated peakareaXML...");
		
			//get col name, which will be used in R
			p.col_name=p.getColName(p.num_timepoints, p.num_replicates);
			
			
			System.out.println("Generated timecourseCSV...");
		
			//generate Tukey and ANOVA output file
		 
			System.out.println("generating tukey and anova file....");
			System.out.println("redundant println");
			TalkToROLD t=new TalkToROLD();
			System.out.println("getting data for array");
			double[][] tableForR=p.getArray(data);
		    p.printOutputArray(tableForR,"_peakarea.csv");
			
		    System.out.println("Calling getTukey now...");
			p.tukeyOutput=t.getTukey(p.file_name,tableForR, p.col_name, p.num_timepoints, p.num_replicates, p.sig_level);
			System.out.println("Finished getTukey call");
			p.printOutputArray(p.tukeyOutput,".csv");	
			p.printFilemakerInput(p.tukeyOutput,".csv");
			p.wait(false);
			System.out.println("Done !");
		}catch (Exception e) {
		    e.printStackTrace();
		  }	
		
		System.exit(0);
				    			
	}	
		
	
    
    
    
    /**
     *  getColName:
     * 	This method is to generate  timecourse.CSV file with column names for R;
     *  Also returns an array with column names;
     * column names: c1 c2 c3...c1 c2...
     * @param condition
     * @param replicates
     * @return output
     */
    String[] getColName(int timepoints, int replicates){
    	
    	String timecourse="";
    	for(int i=1;i<replicates+1;i++){
    		for(int j=1;j<timepoints+1;j++){   			
    			timecourse+="c"+j+",";    			
    		}
    	}    
    	
    	timecourse = timecourse.substring(0, timecourse.length() - 1);
    	String[] output=timecourse.split(",");    	
    	timecourse=timecourse+"\n";           
    
    	/*
    	 try{  		  
   		  String pathToFile = file_name+ "_timecourse.CSV";
   		  // Create file 
 			  FileWriter fstream = new FileWriter(pathToFile);
 			  BufferedWriter out = new BufferedWriter(fstream);
 			  out.write(timecourse);
 			  //Close the output stream
 			  out.close();
   	  }catch(Exception e){
   		  System.err.println("Error: "+e.getMessage());
   	  }
     */
    	
    	 return output;
    }

    
   double[][] getArray(double[][] array){
	   int colnum=num_timepoints*num_replicates;
	   double[][] output = new double[array.length][colnum+1];
	   
	   //System.out.println("row="+output.length);
	   //System.out.println("col="+output[0].length);
	   
	   for(int i=0; i<array.length;i++){
		   
		   for(int j=0; j<colnum+1;j++){
			   if(j==0){
				   output[i][j]=array[i][j];
			   }
			   else{
			       int lookingAt= j+(lf_weight-1)*colnum;
			       output[i][j]=array[i][lookingAt];
			   }
			   }
		   }
		   
	  
	   
	   return output;
   }
    
	/**
	 * printOutputArray:
	 *  	This method is to testing, to print out into a file any 2D array constructed 
	 *      in the code(as there are quite a few of them)
	 *  @param table, any 2-dimensional array made by this code
	 *  @return void
	 */
      void printOutputArray(double[][] table, String name){
    	  //build the table
    	  String output = "";
    	  //System.out.println("len "+table[0].length);
    	 // System.out.println("#row is "+table.length); 273
    	  //System.out.println("#col is "+table[0].length);31
    	  
    	  for(int row=0; row<table.length;row++){  
    		  for (int col=0;col<table[0].length;col++){
    			  if(col%500==0){
    				  //System.out.println(row + " "+col);   				  
    			  }
    			      			  
    			  if(!Double.isNaN(table[row][col])){
    			        output +=table[row][col]+",";
    			  }
    			  else if(Double.isNaN(table[row][col])){
    				    output +=" "+",";
    			  }
    			 
    		       
    		   }  		  
    		  output = output.substring(0, output.length() - 1);
    	      output=output+"\r\n";   
    	  }
    	  try{
    		  
    		  String pathToFile = file_name+ name;
    		  // Create file 
  			  FileWriter fstream = new FileWriter(pathToFile);
  			  BufferedWriter out = new BufferedWriter(fstream);
  			  out.write(output);
  			  
  			  //Close the output stream
  			  out.close();
    	   }catch(Exception e){
    		  System.err.println("Error: "+e.getMessage());
    	    }
      }
   
      
 /**
  * 
  * @param table
  * @param name
  */
     public void printFilemakerInput(double[][] table, String name){
    	 String output="";
    	
    	 for(int row=0;row<table.length;row++){
    		 for(int col=0;col<table[0].length;col++){
    			 if(col==0){                                //counter
    				 output+=(int)table[row][col]+",";    				 
    			 }
    			 else if(col==1||col==2){                       //for pvals and q value
    				 output+=table[row][col]+",";   						 
    			 }
    			 else if(col>2){   				   
    				   
    				   if(((col-2) % 10)==0){
    					 if(!Double.isNaN(table[row][col])){
    				        output+=(int)table[row][col]+",";  
    					 }
    					 else{
    						output+=""+",";
    					 }
    				 }
    				   else if(((col-2) % 10)!=0){
      					 if(!Double.isNaN(table[row][col])){
     				        output+=(int)table[row][col]+new String(new char[] { 29 });  
     					 }
     					 else{
     						output+=""+new String(new char[] { 29 }) ;
     					 }
     				 }
   					 
    			 }
    			 
    			 
    		 }
    		 output = output.substring(0, output.length() - 1);
   	         output=output+"\r\n";
    	 }
    	 
    	 try{
   		  
   		  String pathToFile = file_name+ name;
   		  // Create file 
 			  FileWriter fstream = new FileWriter(pathToFile);
 			  BufferedWriter out = new BufferedWriter(fstream);
 			  out.write(output);
 			  
 			  //Close the output stream
 			  out.close();
   	      }catch(Exception e){
   		      System.err.println("Error: "+e.getMessage());
   	       }
    	 
     }

  	/**
  	 * wait:
  	 *   This method prints a "1" to a file if this program is still running, or a "0" if it has
  	 *   finished and all the q values are calculated.
  	 * 	
  	 * @param busy, a boolean saying whether or not the program is running and still calculating
  	 *              q values
  	 * @return void <- but it does print out a file, "wait.txt," to whatever directory the .jar for
  	 *                 this program is in
  	 */

  	void wait(boolean busy){
  		String output;
  		// Determine what to write into the file based on whether the program is busy or not
  		if(busy){
  			output = "1";			// if this program is running, the file will say "1"
  		}
  		else{
  			output = "0";			// once this program is done running, the file will say "0"
  		}
  		// Make the filename (for the same directory that the .jar is in)
  		try{
  			FileWriter fstream = new FileWriter(path_to_wait);
  			BufferedWriter out = new BufferedWriter(fstream);
  			out.write(output);
  			//Close the output stream
  			out.close();
  		}catch (Exception e){//Catch exception if any
  			System.err.println("Error: " + e.getMessage());
  		}
  	}
  	
  	void reportError(String msg){					// I guess this is for testing... @author kyu
  		System.out.println("Error: " + msg);
  	}
		
  	/**
	 * BadDataException:
	 *   This Exception class is to catch any problems that may occur with input data, if there
	 *   wasn't enough data put in or if it's not in the right format, etc. This (obviously) should
	 *   not happen, but in case it does this Exception will give the most appropriate message for
	 *   what went wrong.
	 * 	
	 * @param msg, the message saying what the error was
	 * 		  filePath, the path to the status file
	 * 		  write, a boolean with whether or not to write the error description to a .txt file
	 * @return void <- if _error is true, it will print a .txt file with the error and stack trace
	 */


	class BadDataException extends Exception{
		public BadDataException(String msg, String filePath, boolean write){
			super(msg);
			if(write){
				this.reportException(msg, filePath);
			}
		}
		
		/**
		 * reportException:
		 *   This method prints a the Exception msg of the BadDataException and the stack trace to a file
		 *   called "error.txt," located in the same directory where the status file is
		 * 	
		 * @param msg, the Exception's message of what went wrong
		 * 		  filePath, the path to the status file
		 * @return void <- but it does print out a file, "error.txt," to whatever directory the  status
		 *                 file is in
		 */
		
		void reportException(String msg, String filePath){
			System.out.println("Error: " + msg);
			msg += "\r\n";
			msg += this.getStackTraceAsString(this);
			try{
				String[] forPTF = path_to_wait.split("\\\\");
				int endLen = forPTF[forPTF.length-1].length();
				String pathToFile = filePath.substring(0, filePath.length()-endLen) + "error.txt";
				FileWriter fstream = new FileWriter(pathToFile);
				BufferedWriter out = new BufferedWriter(fstream);
				out.write(msg);
				//Close the output stream
				out.close();
			}catch (Exception e){//Catch exception if any
				System.err.println("Error: " + e.getMessage());
			}
		}
		
		/**
		 * Gets the exception stack trace as a string.
		 * @param exception
		 * @return
		 * @author Narendra Naidu from Tech Talk blog (http://www.narendranaidu.com)
		 */
		public String getStackTraceAsString(Exception exception)
		{
			java.io.StringWriter sw = new java.io.StringWriter();
			java.io.PrintWriter pw = new java.io.PrintWriter(sw);
			pw.print(" [ ");
			pw.print(exception.getClass().getName());
			pw.print(" ] ");
			pw.print(exception.getMessage());
			exception.printStackTrace(pw);
			return sw.toString();
		}
	}
}
