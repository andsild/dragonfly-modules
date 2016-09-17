using System;
using System.Diagnostics;
using System.Linq;
using System.Runtime.InteropServices;

namespace WindowsAPIGateway
{
    public class SetCurrentProgram
    {
        [DllImport("user32.dll")]
        static extern bool SetForegroundWindow(IntPtr hWnd);

        private static void printProcesses()
        {
            var processss = from proc in System.Diagnostics.Process.GetProcesses() orderby proc.ProcessName ascending select proc;
            foreach (var item in processss)
                Console.WriteLine(item.ProcessName);
        }

        public static bool ProgramIsSpecialCase(string processName, Process[] processes)
        {
            if(processName.Equals("chrome"))
            {
                SetForegroundWindow(processes[4].MainWindowHandle);
                return true;
            }
            return false;
        }

        public static void ActivateApp(string processName)
        {
            var p = Process.GetProcessesByName(processName);
            Console.WriteLine(processName);
            Console.WriteLine(p);

            // for debug purposes
            //printProcesses();

            if (ProgramIsSpecialCase(processName, p))
                Environment.Exit(0);


            if (p.Count() > 1)
            {
                string alphabet = "abcdefghijklmnopqrstuvyz";
                int index = 0;
                foreach (var proc in p)
                    Console.WriteLine($"{alphabet[index++]}: ${proc.MainWindowTitle}");
                Console.WriteLine("x: EXIT");
                char choice = Console.ReadKey().KeyChar;
                if (choice == 'x')
                    Environment.Exit(0);
                SetForegroundWindow(p[alphabet.IndexOf(choice)].MainWindowHandle);
            }
            else if (p.Count() == 1)
                SetForegroundWindow(p[0].MainWindowHandle);
            else
            {
                Console.WriteLine($"Could not find process {processName}");
            }
        }
    }
}
