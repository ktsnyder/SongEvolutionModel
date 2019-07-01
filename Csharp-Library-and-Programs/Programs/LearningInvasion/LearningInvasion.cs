using System;
using System.Diagnostics;
using System.IO;

namespace LearningInvasion
{  
    class LearningInvasion
    {
        static void Main(String[] args){
            //Closed Runs
            InvasionSet("Closed", "1", "1", args[1], args[0], 20, 1000, 500);
            InvasionSet("Closed", "1", "4", args[1], args[0], 20, 1000, 500);
            InvasionSet("Closed", "2", "1", args[1], args[0], 20, 1000, 500);
            InvasionSet("Closed", "2", "4", args[1], args[0], 20, 1000, 500);

            //Delyaed Closed Runs
            InvasionSet("DelayedClosed", ".25", "1", args[1], args[0], 20, 1000, 500);
            InvasionSet("DelayedClosed", ".25", "4", args[1], args[0], 20, 1000, 500);
            InvasionSet("DelayedClosed", "2", "1", args[1], args[0], 20, 1000, 500);
            InvasionSet("DelayedClosed", "2", "4", args[1], args[0], 20, 1000, 500);

            //Open Runs
            InvasionSet("Open", ".25", "1", args[1], args[0], 20, 1000, 500);
            InvasionSet("Open", ".25", "4", args[1], args[0], 20, 1000, 500);
            InvasionSet("Open", "1", "1", args[1], args[0], 20, 1000, 500);
            InvasionSet("Open", "1", "4", args[1], args[0], 20, 1000, 500);
        }
        static void InvasionSet(string windowType, string invadeStat, string numInvade,
                                string paramsFolder, string programFolder, int maxPar, int repeats, int burnIn){
            string ProgramPath = Path.Combine(programFolder,"InvasionSims.dll");
            string ParamPath = Path.Combine(paramsFolder+windowType);
            string OutputPath = Path.Combine(ParamPath,string.Format(windowType+"-"+invadeStat+"-"+numInvade));
            Directory.CreateDirectory(OutputPath);
            string SubArgs = string.Format("{0} {1} {2} {3} {4} {5} {6} {7} {8}", ProgramPath, ParamPath,
                                                                    OutputPath, "Learning", invadeStat, numInvade, maxPar, repeats, burnIn);
         
            //Set up process and run
            Process ClosedtoDelayed = new Process();
            ClosedtoDelayed.StartInfo.FileName = "dotnet";//FinalPath;
            ClosedtoDelayed.StartInfo.Arguments = SubArgs;
            ClosedtoDelayed.EnableRaisingEvents =true;
            ClosedtoDelayed.StartInfo.UseShellExecute=false;
            ClosedtoDelayed.StartInfo.RedirectStandardOutput=false;
            ClosedtoDelayed.StartInfo.WorkingDirectory=programFolder;
            ClosedtoDelayed.Start();
            //Process.Start("dotnet", SubArgs);
            ClosedtoDelayed.WaitForExit();
        }
    }
}

