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
            bool RepAll = args.Length>5?
                            System.Convert.ToBoolean(args[5]):false;
            bool MatchAll = args.Length>6?
                            System.Convert.ToBoolean(args[6]):false;
            bool AgeAll = args.Length>7?
                            System.Convert.ToBoolean(args[7]):false;
            bool LrnThrshAll = args.Length>8?
                            System.Convert.ToBoolean(args[8]):false;
            bool AccAll = args.Length>9?
                            System.Convert.ToBoolean(args[9]):false;
            bool ChanForAll = args.Length>10?
                            System.Convert.ToBoolean(args[10]):false;
            bool ChanInvAll = args.Length>11?
                            System.Convert.ToBoolean(args[11]):false;
            
            //Get files and un sims in parallel
            ParallelOptions opt = new ParallelOptions();
            opt.MaxDegreeOfParallelism = MaxParallel;
            string[] ParamFiles = Utils.GetValidParams(ParamPath);
            Parallel.ForEach(ParamFiles, opt, (FileName) => Run(FileName, Repeats, Frequency, OutputPath,
                RepAll, MatchAll, AgeAll, LrnThrshAll, AccAll, ChanForAll, ChanInvAll));
            
            Console.WriteLine(Global.ElapsedMilliseconds);
            Console.WriteLine("All simulations completed.");
        }

        static void Run(string fileName, int repeats, int frequency, string outputPath,
        bool repAll, bool matchAll, bool ageAll, bool lrnThrshAll, bool accAll,
        bool chanForAll, bool chanInvAll){
            Stopwatch Local = new Stopwatch();
            Local.Start();
            //Get Parameters and run appropriate Simulation
            SimParams Par = new SimParams(reload:true, path: fileName);
            WriteData Temp;
            WriteData Full = Simulations.Interval(Par, frequency, repAll,
                matchAll, ageAll, lrnThrshAll, accAll, chanForAll, chanInvAll);
            for(int j=1;j<repeats;j++){
                Temp = Simulations.Interval(Par, frequency, repAll,
                matchAll, ageAll, lrnThrshAll, accAll, chanForAll, chanInvAll);
                Full.ConCat(Par, Temp);
            }

            //Save data
            string Tag = Utils.GetTag(fileName);
            Full.Output(Par, outputPath, Tag, true);
            Console.WriteLine("{0}-{1}", fileName, Local.ElapsedMilliseconds);
        }
    }
}

