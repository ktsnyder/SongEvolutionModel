using System;
using System.Diagnostics;
using System.Threading.Tasks;
using MathNet.Numerics;
using SongEvolutionModelLibrary;

namespace SongEvolutionModel
{  
    class IntervalSims
    {
        static void Main(String[] args){
            Stopwatch Global = new Stopwatch();
            Global.Start();
            Console.WriteLine("Start");
            //do not check the distributions.
            Control.CheckDistributionParameters = false;

            string ParamPath = args[0];
            string OutputPath = args[1];
            int MaxParallel = args.Length>2?
                            System.Convert.ToInt32(args[2]):4;
            int Frequency = args.Length>3?
                            System.Convert.ToInt32(args[3]):200;
            int Repeats = args.Length>4?
                            System.Convert.ToInt32(args[4]):50;
            
            //Get files and un sims in parallel
            ParallelOptions opt = new ParallelOptions();
            opt.MaxDegreeOfParallelism = MaxParallel;
            string[] ParamFiles = Utils.GetValidParams(ParamPath);
            Parallel.ForEach(ParamFiles, opt, (FileName) => Run(FileName, Repeats, Frequency, OutputPath));
            
            Console.WriteLine(Global.ElapsedMilliseconds);
            Console.WriteLine("All simulations completed.");
        }

        static void Run(string fileName, int repeats, int frequency, string outputPath){
            Stopwatch Local = new Stopwatch();
            Local.Start();
            Console.WriteLine(fileName);

            //Get Parameters and run appropriate Simulation
            SimParams Par = new SimParams(reload:true, path: fileName);
            WriteData Temp;  
            WriteData Full = Simulations.Interval(Par, frequency, false);
            for(int j=1;j<repeats;j++){
                Temp = Simulations.Interval(Par, frequency, false);
                Full.ConCat(Par, Temp);
            }

            //Save data
            string Tag = Utils.GetTag(fileName);
            Full.Output(Par, outputPath, Tag, true);
            Console.WriteLine("{0}-{1}", fileName, Local.ElapsedMilliseconds);
        }
    }
}

