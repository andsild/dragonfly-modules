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

        [DllImport("user32.dll", SetLastError = true)]
        public static extern IntPtr GetWindow(IntPtr hWnd, GetWindow_Cmd uCmd);

        [DllImport("user32.dll", SetLastError = true)]
        public static extern IntPtr FindWindow(string lpClassName, string lpWindowName);

        public enum GetWindow_Cmd : uint
        {
            GW_HWNDFIRST = 0,
            GW_HWNDLAST = 1,
            GW_HWNDNEXT = 2,
            GW_HWNDPREV = 3,
            GW_OWNER = 4,
            GW_CHILD = 5,
            GW_ENABLEDPOPUP = 6
        }

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
                IntPtr chromeWindow = FindWindow("Chrome_WidgetWin_1", null);
                IntPtr chrome = GetWindow(chromeWindow, GetWindow_Cmd.GW_HWNDNEXT);

                //Setting the window to the foreground (implies focus and activating)
                SetForegroundWindow(chrome);
                return true;
            }
            if (processName.Equals("AcroRd32"))
            {
                foreach(var proc in processes)
                    if(proc.MainWindowTitle.Contains("Adobe Acrobat Reader"))
                    {
                        SetForegroundWindow(proc.MainWindowHandle);
                        return true;
                    }
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
