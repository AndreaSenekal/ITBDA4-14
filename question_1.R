
// imports
import java.io.IOException;
import org.apache.hadoop.io.IntWritable;
import org.apache.hadoop.io.LongWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapred.MapReduceBase;
import org.apache.hadoop.mapred.Mapper;
import org.apache.hadoop.mapred.OutputCollector;
import org.apache.hadoop.mapred.Reporter;

// create mapper class
public class IdentifyKeywordMapper extends Mapper< LongWritable,Text,Text, Text > {

	// private = only accessible in IdentifyKeyword class

	// one - object holds value of 1 and 
// will be used to count every instance keyword shows up
	// word – empty string object
		// goes hand in hand with ‘one’ to serialize across the cluster
private final static IntWritable one = new IntWritable(1);
	private Text word = new Text();

	// create keyword variable
	String keyword = ‘africa’

	// replace a method from the parent class
@Override

// pulls one line of code at a time
public void map(LongWritable key,Text value,Context context) 

// if the method throws and exception, this is what to do
throws IOException, InterruptedException {

// split lines into array of words by space
    		String[] words = value.toString().split("\\s+");

		// loop through all words
    		for (String w : words) {
			
			// only keeps letters in each word – remove punctuation
			wCleaned = w.replaceAll(‘[^a-zA-Z]’,’’)
			if (wCleaned.equalsIgnoreCase(keyword)) {
				// builds location of where keyword is sitting
				String location = ‘Row:’+rowNumber+’,Col:’+(col+1);
				// loads location into locationText
				locationText.set(location);
				// mapper output key-value pair
				context.write(word,locationText);
			}
  		}
}
}

////////////////////////////////////////////////////////////////////////////

// imports
import java.io.IOException;
import org.apache.hadoop.io.*;
import org.apache.hadoop.mapreduce.*;

// create reducer class
public class IdentifyKeywordReducer extends Reducer <Text,Text,Text,Text> {

	// replace a method from the parent class
 	@Override

// if the method throws and exception, this is what to do
public void reduce(Text key, Iterable<Text> values, Context context) 
throws IOException, InterruptedException {
	
	// loop through location string and write output
    		for (Text val:values) {
      			context.write(key,value);
    		}
  	}
}

////////////////////////////////////////////////////////////////////////////

// imports
import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.mapreduce.*;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;

// create driver class
public class IdentifyKeyword {

	// when program is launched this is the starting point
  	public static void main(String[] args) throws Exception {

		// holds Hadoops settings
    		Configuration conf = new Configuration();

		// creates a job object that represents our task of identifying our keyword
    		Job job = Job.getInstance(conf,’Keyword Identification’);
    		job.setJarByClass(IdentifyKeyword.class);

		// tells Hadoop which mapper and reducer to use
    		job.setMapperClass(IdentifyKeywordMapper.class);
   		job.setReducerClass(IdentifyKeywordReducer.class);

		// final output key and value will be text
    		job.setOutputKeyClass(Text.class);
    		job.setOutputValueClass(Text.class);

		// where Hadoop should read input files
   		FileInputFormat.addInputPath(job,new Path(args[0]));

		// where Hadoop should write output files
    		FileOutputFormat.setOutputPath(job, new Path(args[1]));

		// wait for the job to finish running
    		System.exit(job.waitForCompletion(true) ? 0 : 1);
  	}
}







