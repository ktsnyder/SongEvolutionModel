//Loads in sets of functions for use
using System;
using System.IO;
using System.Diagnostics;
using System.Threading.Tasks;
using MathNet.Numerics;
using SongEvolutionModelLibrary;


//you can change the name for the namespace, but make sure it matches the .csproj file!
namespace Template
{  
    class Template
    {
        static void Main(String[] args){//do not change this line!

            //Starts a timer.  You can omit this.
            Stopwatch Global = new Stopwatch();
            Global.Start();
            //warns that the simulation has started.  You can omit this.
            Console.WriteLine("Start");
            
            //prevents checks on the distributions; delete if you are changing the library, otherwise leave it alone.
            //I have internal checks to ensure distributions have appropriate boundaries, so this just wastes time.
            Control.CheckDistributionParameters = false;







            //assign arguments that are passed in the console into the right data types.
            //Add more arguments and assignments or deleted unwanted ones as needed.

            String ParamPath = args[0];//Required argument!
            String OutputPath = args[1];//Required argument!            
            int Optional = args.Length>2 ? System.Convert.ToInt32(args[2]) : 4; //optional argument with a default!
            
            



            //get the .SEMP files
            
            //Use the next two lines if you want to change the params here rather than load .SEMPs,
            //and comment out the following three lines. 
                //SimParams Par = new SimParams();
                //string Tag = 'new';

            String[] ParamFiles = Utils.GetValidParams(ParamPath);
            //for(int i = 0; i < ParamFiles.length; i++){
                SimParams Par = new SimParams(reload:true, path:ParamFiles[0]);//i instead of 0
                string Tag = Utils.GetTag(ParamFiles[0]);//i instead of 0

                
                
                //Basic is not the only simulation option, but should cover most of your needs
                WriteData Full = Simulations.Basic(Par);

                //Writes the data files (.SEMP and .csvs)
                Full.Output(Par,OutputPath,Tag, true);
            //}
            
            //Note that this code will only run one parameter file.  To do all .SEMPS in a folder,
            //uncomment the for loop and its end bracket and change marked the 0's to i.
            //Also notice that parallelization is absent.  See the IntervalSim program an example of code that parallelizes.





            
            //Warns how much time has ellasped and that the simulation is over.  you can omit this.
            Console.WriteLine(Global.ElapsedMilliseconds);
            Console.WriteLine("All simulations completed.");
        }
    }
}

